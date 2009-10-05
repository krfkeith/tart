/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Sema/ClassAnalyzer.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/FunctionType.h"
#include "tart/CFG/FunctionDefn.h"
#include "tart/CFG/PrimitiveType.h"
#include "tart/CFG/TypeDefn.h"
#include "tart/CFG/Template.h"
#include "tart/CFG/Module.h"
#include "tart/CFG/Block.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/FunctionAnalyzer.h"
#include "tart/Sema/EvalPass.h"
#include "tart/Objects/Builtins.h"

namespace tart {

static const Defn::Traits CONSTRUCTOR_TRAITS = Defn::Traits::of(Defn::Ctor);

static const DefnPasses PASS_SET_COMPARE = DefnPasses::of(
  Pass_ResolveBaseTypes
);

static const DefnPasses PASS_SET_LOOKUP = DefnPasses::of(
  Pass_CreateMembers,
  Pass_ResolveAttributes,
  Pass_ResolveBaseTypes
);

static const DefnPasses PASS_SET_CALL_OR_USE = DefnPasses::of(
  Pass_CreateMembers,
  Pass_ResolveBaseTypes,
  Pass_ResolveAttributes,
  Pass_AnalyzeConstructors,
  Pass_AnalyzeFields
);

static const DefnPasses PASS_SET_CODEGEN = DefnPasses::of(
  Pass_CreateMembers,
  Pass_ResolveBaseTypes,
  Pass_ResolveAttributes,
  Pass_AnalyzeConstructors,
  Pass_AnalyzeFields,
  Pass_AnalyzeMethods,
  Pass_ResolveOverloads,
  Pass_ResolveStaticInitializers
);

ClassAnalyzer::ClassAnalyzer(TypeDefn * de)
  : DefnAnalyzer(de->module(), de->definingScope())
  , target(de)
{
  DASSERT(de != NULL);
}

bool ClassAnalyzer::analyze(AnalysisTask task) {
  // Work out what passes need to be run.
  DefnPasses passesToRun;

  switch (task) {
    case Task_PrepTypeComparison:
      addPasses(target, passesToRun, PASS_SET_COMPARE);
      break;

    case Task_PrepMemberLookup:
      addPasses(target, passesToRun, PASS_SET_LOOKUP);
      break;

    case Task_PrepCallOrUse:
      addPasses(target, passesToRun, PASS_SET_CALL_OR_USE);
      break;

    case Task_PrepCodeGeneration:
      addPasses(target, passesToRun, PASS_SET_CODEGEN);
      break;

    case Task_PrepOverloadSelection:
    case Task_InferType:
      break;
  }

  passesToRun.removeAll(target->finished());
  if (passesToRun.empty()) {
    return true;
  }

  // Skip analysis of templates - for now.
  if (target->isTemplate()) {
    // Get the template scope and set it as the active scope.
    analyzeTemplateSignature(target);
    if (passesToRun.contains(Pass_ResolveBaseTypes) && !analyzeBaseClasses()) {
      return false;
    }

    return true;
  }

  if (target->isTemplateMember()) {
    return true;
  }

  DefnAnalyzer::analyze(target, passesToRun);

  if (passesToRun.contains(Pass_ResolveBaseTypes) && !analyzeBaseClasses()) {
    return false;
  }

  if (passesToRun.contains(Pass_AnalyzeFields) && !analyzeFields()) {
    return false;
  }

  if (passesToRun.contains(Pass_AnalyzeConstructors) && !analyzeConstructors()) {
    return false;
  }

  if (passesToRun.contains(Pass_AnalyzeMethods) && !analyzeMethods()) {
    return false;
  }

  if (passesToRun.contains(Pass_ResolveOverloads) && !analyzeOverloading()) {
    return false;
  }

  if (passesToRun.contains(Pass_ResolveStaticInitializers) && !analyzeStaticInitializers()) {
    return false;
  }

  return true;
}

bool ClassAnalyzer::analyzeBaseClasses() {
  if (target->isPassRunning(Pass_ResolveBaseTypes)) {
    diag.error(target) << "Circular inheritance not allowed";
    return false;
  }

  if (!target->beginPass(Pass_ResolveBaseTypes)) {
    return true;
  }

  bool result = analyzeBaseClassesImpl();
  target->finishPass(Pass_ResolveBaseTypes);
  return result;
}

bool ClassAnalyzer::analyzeBaseClassesImpl() {

  // If there is no AST, then it means that this class was created
  // internally by the compiler, in which case the compiler is responsible
  // for setting up the base class list correctly.
  const ASTTypeDecl * ast = cast_or_null<const ASTTypeDecl>(target->ast());
  if (ast == NULL) {
    return true;
  }

  CompositeType * type = cast<CompositeType>(target->typeValue());
  bool isFromTemplate =
      target->isTemplate() || target->isTemplateMember() || target->isPartialInstantiation();
  DASSERT_OBJ(isFromTemplate || type->isSingular(), type);
  DASSERT_OBJ(type->super() == NULL, type);

  // Check for valid finality
  if (target->isFinal()) {
    if (type->typeClass() == Type::Interface) {
      diag.error(target) << "Interface type cannot be final";
    } else if (type->typeClass() == Type::Protocol) {
      diag.error(target) << "Protocol type cannot be final";
    }
  }

  // Resolve base class references to real types.
  Type::TypeClass dtype = type->typeClass();
  const ASTNodeList & astBases = ast->bases();
  CompositeType * primaryBase = NULL;
  TypeAnalyzer ta(moduleForDefn(target), target->definingScope());
  if (target->isTemplate()) {
    ta.setActiveScope(&target->templateSignature()->paramScope());
  }

  for (ASTNodeList::const_iterator it = astBases.begin(); it != astBases.end(); ++it) {
    Type * baseType = ta.typeFromAST(*it);
    if (isErrorResult(baseType)) {
      return false;
    }

    TypeDefn * baseDefn = baseType->typeDefn();
    if (baseDefn == NULL || !isa<CompositeType>(baseType)) {
      diag.error(*it) << "Cannot inherit from " << *it << " type";
      return false;
    }

    if (!baseType->isSingular() && !isFromTemplate) {
      diag.error(*it) << "Base type '" << baseDefn << "' is a template, not a type";
      return false;
    }

    if (baseDefn->isFinal()) {
      diag.error(*it) << "Base type '" << baseDefn << "' is final";
    }

    // Recursively analyze the bases of the base
    if (!ClassAnalyzer(baseDefn).analyze(Task_PrepMemberLookup)) {
      return false;
    }

    Type::TypeClass baseKind = baseType->typeClass();
    bool isPrimary = false;
    switch (dtype) {
      case Type::Class:
        if (baseKind == Type::Class) {
          if (primaryBase == NULL) {
            isPrimary = true;
          } else {
            diag.error(target) << "classes can only have a single concrete supertype";
          }
        } else if (baseKind != Type::Interface) {
          diag.error(target) << (Defn *)target <<
              "a class can only inherit from class or interface";
        }
        break;

      case Type::Struct:
        if (baseKind != Type::Struct && baseKind != Type::Protocol) {
          diag.error(target) <<
            "struct can only derive from a struct or static interface type";
        } else if (primaryBase == NULL) {
          isPrimary = true;
        } else {
          diag.error(target) << "structs can only have a single concrete supertype";
        }
        break;

      case Type::Interface:
        if (baseKind != Type::Interface && baseKind != Type::Protocol) {
          diag.error(*it) << "interface can only inherit from interface or protocol";
        } else if (primaryBase == NULL) {
          isPrimary = true;
        }

        break;

      default:
        DFAIL("IllegalState");
        break;
    }

    // Add an external reference to this base (does nothing if it's defined
    // by this module.)
    CompositeType * baseClass = cast<CompositeType>(baseType);
    if (baseClass->isSingular()) {
      baseClass->addBaseXRefs(module);
    }

    if (isPrimary) {
      primaryBase = baseClass;
    } else {
      type->bases().push_back(baseClass);
    }
  }

  // If no base was specified, use Object.
  if (dtype == Type::Class && primaryBase == NULL && type != Builtins::typeObject) {
    primaryBase = static_cast<CompositeType *>(Builtins::typeObject);
    module->addSymbol(primaryBase->typeDefn());
  }

  type->setSuper(primaryBase);

  // define the super type
  if (primaryBase != NULL) {
    // Move the primary base to be first in the list.
    type->bases().insert(type->bases().begin(), primaryBase);
  }

  if (dtype == Type::Interface) {
    module->addSymbol(Builtins::funcTypecastError);
  }

  return true;
}

bool ClassAnalyzer::analyzeFields() {
  if (target->beginPass(Pass_AnalyzeFields)) {
    CompositeType * type = cast<CompositeType>(target->typeValue());
    CompositeType * super = type->super();
    // Also analyze base class fields.
    int instanceFieldCount = 0;
    int instanceFieldCountRecursive = 0;
    if (super != NULL) {
      ClassAnalyzer(super->typeDefn()).analyze(Task_PrepCallOrUse);

      // Reserve one slot for the superclass.
      type->instanceFields_.push_back(NULL);
      instanceFieldCount = 1;
      instanceFieldCountRecursive = super->instanceFieldCountRecursive();
    }

    Defn::DefnType dtype = target->defnType();
    for (Defn * member = type->firstMember(); member != NULL; member = member->nextInScope()) {
      switch (member->defnType()) {
        case Defn::Var:
        case Defn::Let: {
          VariableDefn * field = static_cast<VariableDefn *>(member);
          field->copyTrait(target, Defn::Final);

          analyzeValueDefn(field, Task_PrepCallOrUse);
          DASSERT(field->type().isDefined());

          bool isStorageRequired = true;
          if (field->defnType() == Defn::Let) {
            if (field->initValue() != NULL && field->initValue()->isConstant()) {
              // TODO: There may be other cases not handled here.
              isStorageRequired = false;
            }
          }

          if (isStorageRequired) {
            if (type->typeClass() == Type::Interface) {
              diag.error(field) << "Data member not allowed in interface: " << field;
            }

            if (field->storageClass() == Storage_Instance) {
              field->setMemberIndex(instanceFieldCount++);
              field->setMemberIndexRecursive(instanceFieldCountRecursive++);
              type->instanceFields_.push_back(field);
            } else if (field->storageClass() == Storage_Static) {
              module->addSymbol(field);
              type->staticFields_.push_back(field);
            }
          }

          break;
        }

        case Defn::Typedef:
        case Defn::Namespace: {
          //DFAIL("Implement");
          break;
        }
      }
    }

    DASSERT(type->instanceFields_.size() == instanceFieldCount);
    target->finishPass(Pass_AnalyzeFields);
  }

  return true;
}

bool ClassAnalyzer::analyzeConstructors() {
  if (target->beginPass(Pass_AnalyzeConstructors)) {
    CompositeType * type = cast<CompositeType>(target->typeValue());
    // Analyze the constructors first, because we may need them
    // during the rest of the analysis.
    Type::TypeClass tcls = type->typeClass();
    if (tcls == Type::Class || tcls == Type::Struct) {
      // Analyze superclass constructors
      if (type->super() != NULL) {
        ClassAnalyzer ca(type->super()->typeDefn());
        if (!ca.analyze(Task_PrepCallOrUse)) {
          return false;
        }
      }

      DefnList ctors;
      size_t hasConstructor = false;
      if (type->lookupMember(istrings.idConstruct, ctors, false)) {
        for (DefnList::iterator it = ctors.begin(); it != ctors.end(); ++it) {
          if (FunctionDefn * ctor = dyn_cast<FunctionDefn>(*it)) {
            diag.recovered();

            hasConstructor = true;
            ctor->addTrait(Defn::Ctor);

            if (!FunctionAnalyzer(ctor).analyze(Task_PrepOverloadSelection)) {
              continue;
            }

            if (!ctor->returnType().isDefined()) {
              ctor->functionType()->setReturnType(&VoidType::instance);
            }

            if (ctor->returnType().isNonVoidType()) {
              diag.fatal(ctor) << "Constructor cannot declare a return type.";
              break;
            }

            if (ctor->storageClass() != Storage_Instance) {
              diag.fatal(ctor) << "Constructor must be instance method.";
              break;
            }

            if (!ctor->isTemplate() && type->isSingular()) {
              // Mark the constructor as singular if in fact it is.
              ctor->addTrait(Defn::Singular);
            }

            analyzeConstructBase(ctor);
          } else {
            diag.fatal(*it) << "Member named 'construct' must be a method.";
            break;
          }
        }
      }

      // If there were no constructors, look for a creator.
      ctors.clear();
      if (!hasConstructor && type->lookupMember(istrings.idCreate, ctors, false)) {
        for (DefnList::iterator it = ctors.begin(); it != ctors.end(); ++it) {
          if (FunctionDefn * ctor = dyn_cast<FunctionDefn>(*it)) {
            diag.recovered();
            if (ctor->storageClass() == Storage_Static) {
              hasConstructor = true;
            }

            if (!FunctionAnalyzer(ctor).analyze(Task_PrepOverloadSelection)) {
              continue;
            }

            // TODO: check return type.
          }
        }
      }

      if (!hasConstructor) {
        //diag.debug(target) << "Creating default constructor for " << target;
        createDefaultConstructor();
      }
    }

    target->finishPass(Pass_AnalyzeConstructors);
  }

  return true;
}

void ClassAnalyzer::analyzeConstructBase(FunctionDefn * ctor) {
  CompositeType * type = cast<CompositeType>(target->typeValue());
  CompositeType * superType = cast_or_null<CompositeType>(type->super());
  if (superType != NULL) {
    BlockList & blocks = ctor->blocks();
    for (BlockList::iterator blk = blocks.begin(); blk != blocks.end(); ++blk) {
      ExprList & exprs = (*blk)->exprs();
      for (ExprList::iterator e = exprs.begin(); e != exprs.end(); ++e) {
        //if (e->exprType() ==
      }
    }
  }
}

bool ClassAnalyzer::analyzeMethods() {
  if (target->beginPass(Pass_AnalyzeMethods)) {
    CompositeType * type = cast<CompositeType>(target->typeValue());
    Defn::DefnType dtype = target->defnType();
    for (Defn * member = type->firstMember(); member != NULL; member = member->nextInScope()) {
      if (member->isTemplate()) {
        continue;
      }

      if (member->isFinal()) {
        if (type->typeClass() == Type::Interface || type->typeClass() == Type::Protocol) {
          diag.error(target) << "Interface or protocol method cannot be final";
        }
      } else if (member->visibility() != Public) {
        if (type->typeClass() == Type::Interface || type->typeClass() == Type::Protocol) {
          diag.error(target) << "Interface or protocol method cannot be non-public";
        }
      }

      if (METHOD_DEFS.contains(member->defnType()) || member->defnType() == Defn::Property) {
        if (ValueDefn * val = dyn_cast<ValueDefn>(member)) {
          analyzeValueDefn(val, Task_PrepCodeGeneration);
        }
      }
    }

    target->finishPass(Pass_AnalyzeMethods);
  }

  return true;
}

bool ClassAnalyzer::analyzeOverloading() {
  if (target->beginPass(Pass_ResolveOverloads)) {
    CompositeType * type = cast<CompositeType>(target->typeValue());
    // Do overload analysis on all bases
    ClassList & bases = type->bases();
    for (ClassList::iterator it = bases.begin(); it != bases.end(); ++it) {
      analyzeDefn((*it)->typeDefn(), Task_PrepCodeGeneration);
    }

    copyBaseClassMethods();
    createInterfaceTables();
    overrideMembers();
    addNewMethods();
    checkForRequiredMethods();

    target->finishPass(Pass_ResolveOverloads);
  }

  return true;
}

void ClassAnalyzer::copyBaseClassMethods() {
  // If it's not a normal class, it can still have a supertype.
  CompositeType * type = cast<CompositeType>(target->typeValue());
  Type::TypeClass tcls = type->typeClass();
  CompositeType * superClass = type->super();
  if (superClass == NULL &&
      (tcls == Type::Interface || tcls == Type::Struct) &&
      !type->bases().empty()) {
    superClass = type->bases().front();
  }

  // Copy superclass methods to instance method table
  if (superClass != NULL) {
    DASSERT_OBJ(superClass->isSingular(), target);
    type->instanceMethods_.append(
        superClass->instanceMethods_.begin(),
        superClass->instanceMethods_.end());
  }
}

void ClassAnalyzer::createInterfaceTables() {
  typedef CompositeType::InterfaceList InterfaceList;

  // Get the set of all ancestor types.
  ClassSet ancestors;
  CompositeType * type = cast<CompositeType>(target->typeValue());
  type->ancestorClasses(ancestors);

  // Remove from the set all types which are the first parent of some other type
  // that is already in the set, since they can use the same dispatch table.
  ClassSet interfaceTypes(ancestors);
  ancestors.insert(type);
  for (ClassSet::iterator it = ancestors.begin(); it != ancestors.end(); ++it) {
    CompositeType * base = *it;

    // The first parent of each parent can always be removed, since the itable
    // of any class is always a superset of the itable of its first parent.
    if (!base->bases().empty()) {
      CompositeType * baseBase = base->bases().front();
      interfaceTypes.remove(baseBase);
    }
  }

  // Create the tables for each interface that remains.
  for (ClassSet::iterator it = interfaceTypes.begin(); it != interfaceTypes.end(); ++it) {
    CompositeType * itype = *it;
    DASSERT(itype->typeClass() == Type::Interface);

    // Do the search before we push the new itable entry.
    const CompositeType::InterfaceTable * parentImpl = type->findBaseImplementationOf(itype);

    // Add an itable entry.
    type->interfaces_.push_back(CompositeType::InterfaceTable(itype));
    CompositeType::InterfaceTable & itable = type->interfaces_.back();

    if (parentImpl != NULL) {
      DASSERT(itype->instanceMethods_.size() == parentImpl->methods.size());
      itable.methods.append(parentImpl->methods.begin(), parentImpl->methods.end());
    } else {
      itable.methods.append(itype->instanceMethods_.begin(), itype->instanceMethods_.end());
    }
  }
}

void ClassAnalyzer::overrideMembers() {
  typedef CompositeType::InterfaceList InterfaceList;

  // In this case, we iterate through the symbol table so that we can
  // get all of the overloads at once.
  CompositeType * type = cast<CompositeType>(target->typeValue());
  SymbolTable & clMembers = type->members();
  for (SymbolTable::iterator s = clMembers.begin(); s != clMembers.end(); ++s) {
    SymbolTable::Entry & entry = s->second;
    MethodList methods;
    MethodList getters;
    MethodList setters;
    FunctionDefn * uniqueGetter = NULL;
    PropertyDefn * prop = NULL;

    // Look for properties and methods. Methods can have more than one implementation
    // for the same name.
    // Find all same-named methods.
    for (SymbolTable::Entry::iterator it = entry.begin(); it != entry.end(); ++it) {
      if (FunctionDefn * func = dyn_cast<FunctionDefn>(*it)) {
        if (func->isSingular()) {
          module->addSymbol(func);
          if (func->storageClass() == Storage_Instance && !func->isCtor()) {
            methods.push_back(func);
          }
        }
      } else if ((*it)->defnType() == Defn::Property || (*it)->defnType() == Defn::Indexer) {
        prop = cast<PropertyDefn>(*it);
        if (prop->storageClass() == Storage_Instance && prop->isSingular()) {
          DASSERT_OBJ(prop->isPassFinished(Pass_ResolveVarType), prop);
          if (prop->getter() != NULL) {
            analyzeValueDefn(prop->getter(), Task_PrepCodeGeneration);
            getters.push_back(prop->getter());
          }

          if (prop->setter() != NULL) {
            analyzeValueDefn(prop->setter(), Task_PrepCodeGeneration);
            setters.push_back(prop->setter());
          }
        }
      }
    }

    InterfaceList & ifaceList = type->interfaces_;

    if (!methods.empty()) {
      // Insure that there's no duplicate method signatures.
      ensureUniqueSignatures(methods);

      // Update the table of instance methods and the interface tables
      overrideMethods(type->instanceMethods_, methods, true);
      for (InterfaceList::iterator it = ifaceList.begin(); it != ifaceList.end(); ++it) {
        overrideMethods(it->methods, methods, false);
      }
    }

    if (!getters.empty()) {
      ensureUniqueSignatures(getters);
      overrideMethods(type->instanceMethods_, getters, true);
      for (InterfaceList::iterator it = ifaceList.begin(); it != ifaceList.end(); ++it) {
        overrideMethods(it->methods, getters, false);
      }
    }

    if (!setters.empty()) {
      ensureUniqueSignatures(setters);
      overrideMethods(type->instanceMethods_, setters, true);
      for (InterfaceList::iterator it = ifaceList.begin(); it != ifaceList.end(); ++it) {
        overrideMethods(it->methods, setters, false);
      }
    }
  }
}

void ClassAnalyzer::ensureUniqueSignatures(MethodList & methods) {
  for (size_t i = 0; i < methods.size(); ++i) {
    for (size_t j = i + 1; j < methods.size(); ++j) {
      if (hasSameSignature(methods[i], methods[j])) {
        diag.error(methods[j]) << "Member type signature conflict";
        diag.info(methods[i]) << "From here";
      }
    }
  }
}

void ClassAnalyzer::addNewMethods() {
  // Append all methods that aren't overrides of a superclass. Note that we
  // don't need to include 'final' methods since they are never called via
  // vtable lookup.
  CompositeType * type = cast<CompositeType>(target->typeValue());
  for (Defn * de = type->firstMember(); de != NULL; de = de->nextInScope()) {
    if (de->storageClass() == Storage_Instance && de->isSingular()) {
      Defn::DefnType dt = de->defnType();
      if (dt == Defn::Function) {
        FunctionDefn * fn = static_cast<FunctionDefn *>(de);
        if (fn->isUndefined() && fn->overriddenMethods().empty()) {
          diag.error(fn) << "Method '" << fn->name() <<
              "' defined with 'undef' but does not override a base class method.";
        }

        if (!fn->isCtor() && !fn->isFinal() && fn->dispatchIndex() < 0) {
          fn->setDispatchIndex(type->instanceMethods_.size());
          type->instanceMethods_.push_back(fn);
        }
      } else if (dt == Defn::Property || dt == Defn::Indexer) {
        PropertyDefn * prop = static_cast<PropertyDefn *>(de);
        FunctionDefn * getter = prop->getter();
        if (getter != NULL && !getter->isFinal() && getter->dispatchIndex() < 0) {
          getter->setDispatchIndex(type->instanceMethods_.size());
          type->instanceMethods_.push_back(getter);
        }

        FunctionDefn * setter = prop->setter();
        if (setter != NULL && !setter->isFinal() && setter->dispatchIndex() < 0) {
          setter->setDispatchIndex(type->instanceMethods_.size());
          type->instanceMethods_.push_back(setter);
        }
      }
    }
  }
}

void ClassAnalyzer::checkForRequiredMethods() {
  typedef CompositeType::InterfaceList InterfaceList;

  if (target->isAbstract()) {
    return;
  }

  CompositeType * type = cast<CompositeType>(target->typeValue());
  Type::TypeClass tcls = type->typeClass();
  MethodList & methods = type->instanceMethods_;
  if (!methods.empty()) {

    // Check for abstract or interface methods which weren't overridden.
    MethodList abstractMethods;
    for (MethodList::iterator it = methods.begin(); it != methods.end(); ++it) {
      FunctionDefn * func = *it;
      if (!func->hasBody() && !func->isExtern() && !func->isIntrinsic() && !func->isUndefined()) {
        abstractMethods.push_back(func);
      }
    }

    if (!abstractMethods.empty()) {
      if (tcls == Type::Struct || (tcls == Type::Class && !target->isAbstract())) {
        diag.recovered();
        diag.error(target) << "Concrete type '" << target <<
            "'lacks definition for the following methods:";
        for (MethodList::iterator it = abstractMethods.begin(); it != abstractMethods.end(); ++it) {
          diag.info(*it) << Format_Type << *it;
        }
      }

      return;
    }
  }

  InterfaceList & itab = type->interfaces_;
  for (InterfaceList::iterator it = itab.begin(); it != itab.end(); ++it) {
    MethodList unimpMethods;
    for (MethodList::iterator di = it->methods.begin(); di != it->methods.end(); ++di) {
      FunctionDefn * func = *di;
      if (!func->hasBody() && !func->isExtern() && !func->isIntrinsic() && !func->isUndefined()) {
        unimpMethods.push_back(func);
      }
    }

    if (!unimpMethods.empty()) {
      diag.recovered();
      diag.error(target) << "Concrete class '" << target <<
          "' implements interface '" << it->interfaceType <<
          "' but lacks implementations for:";
      for (MethodList::iterator it = unimpMethods.begin(); it != unimpMethods.end(); ++it) {
        diag.info(*it) << Format_Verbose << *it;
      }

      return;
    }
  }
}

void ClassAnalyzer::overrideMethods(MethodList & table, const MethodList & overrides,
    bool canHide) {
  const char * name = overrides.front()->name();
  size_t tableSize = table.size();
  for (size_t i = 0; i < tableSize; ++i) {
    FunctionDefn * m = table[i];
    if (m->name() == name) {
      FunctionDefn * newMethod = findOverride(m, overrides);
      if (newMethod != NULL) {
        table[i] = newMethod;
        if (canHide && newMethod->dispatchIndex() < 0) {
          newMethod->setDispatchIndex(i);
        }
        newMethod->overriddenMethods().insert(m);
      } else if (canHide) {
        diag.recovered();
        diag.warn(m) << "Definition of '" << m << "' is hidden";
        for (MethodList::const_iterator it = overrides.begin(); it != overrides.end(); ++it) {
          diag.info(*it) << "by '" << *it << "'";
        }
      }
    }
  }
}

#if 0
void ClassAnalyzer::overridePropertyAccessors(MethodList & table, const MethodList & accessors,
    bool canHide) {
  const char * name = accessor->name();
  size_t tableSize = table.size();
  for (size_t i = 0; i < tableSize; ++i) {
    FunctionDefn * m = table[i];
    if (m->name() == name) {
      if (canOverride(m, accessor)) {
        table[i] = accessor;
        if (canHide && accessor->dispatchIndex() < 0) {
          accessor->setDispatchIndex(i);
        }
        accessor->overriddenMethods().insert(m);
      } else {
        diag.recovered();
        diag.fatal(accessor) << "'" << accessor << "' attempts to override '" <<
            m << "' which has incompatible type";
        diag.info(m) << "originally defined here";
      }
    }
  }
}
#endif

bool ClassAnalyzer::hasSameSignature(FunctionDefn * f0, FunctionDefn * f1) {
  FunctionType * ft0 = f0->functionType();
  FunctionType * ft1 = f1->functionType();

  DASSERT_OBJ(ft0->returnType().isDefined(), f0);
  DASSERT_OBJ(ft1->returnType().isDefined(), f1);
  if (!ft0->returnType().isEqual(ft1->returnType())) {
    return false;
  }

  if (ft0->params().size() != ft1->params().size()) {
    return false;
  }

  for (size_t i = 0; i < ft0->params().size(); ++i) {
    ParameterDefn * p0 = ft0->params()[i];
    ParameterDefn * p1 = ft1->params()[i];

    DASSERT_OBJ(p0->type().isDefined(), p0);
    DASSERT_OBJ(p1->type().isDefined(), p1);

    if (!p0->type().isEqual(p1->type())) {
      return false;
    }
  }

  return true;
}

FunctionDefn * ClassAnalyzer::findOverride(const FunctionDefn * f, const MethodList & overrides) {
  for (MethodList::const_iterator it = overrides.begin(); it != overrides.end(); ++it) {
    if (canOverride(f, *it)) {
      return *it;
    }
  }

  return NULL;
}

bool ClassAnalyzer::canOverride(const FunctionDefn * base, const FunctionDefn * func) {
  DASSERT_OBJ(base->type().isDefined(), base);
  DASSERT_OBJ(func->type().isDefined(), func);

  const FunctionType * baseType = base->functionType();
  const FunctionType * funcType = func->functionType();

  const TypeRef baseReturn = baseType->returnType();
  const TypeRef funcReturn = funcType->returnType();

  if (!baseReturn.isEqual(funcReturn)) {
    // TODO: Variance test.
    return false;
  }

  size_t paramCount = baseType->params().size();
  if (paramCount != funcType->params().size()) {
    // Different number of parameters.
    return false;
  }

  for (size_t i = 0; i < paramCount; ++i) {
    const ParameterDefn * baseArg = baseType->params()[i];
    const ParameterDefn * funcArg = funcType->params()[i];

    if (baseArg->isVariadic() != funcArg->isVariadic())
      return false;

    TypeRef baseArgType = baseArg->type();
    TypeRef funcArgType = funcArg->type();

    if (!baseArgType.isEqual(funcArgType)) {
      switch (baseArg->variance()) {
        case Invariant:
          // funcArgType must be equal to base type
          return false;

        case Covariant:
          // funcArgType is narrower than base type
          // TODO: 'isSubtype' is the wrong test here.
          //if (!funcArgType->isSubtype(baseArgType)) {
          //  return false;
          //}
          return false;
          break;

        case Contravariant:
          // funcArgType is broader than base type
          // TODO: 'isSubtype' is the wrong test here.
          //if (!baseArgType->isSubtype(funcArgType)) {
          //  return false;
          //}
          return false;
          break;
      }
    }
  }

  return true;
}

bool ClassAnalyzer::createDefaultConstructor() {
  // Determine if the superclass has a default constructor. If it doesn't,
  // then we cannot make a default constructor.
  CompositeType * type = cast<CompositeType>(target->typeValue());
  CompositeType * super = type->super();
  FunctionDefn * superCtor = NULL;
  if (super != NULL && super->defaultConstructor() == NULL) {
    diag.fatal(target) << "Cannot create a default constructor for '" <<
        target << "' because super type '" << super <<
        "' has no default constructor";
    return false;
  }

  // List of parameters to the default constructor
  ParameterList requiredParams;
  ParameterList optionalParams;
  ParameterDefn * selfParam = new ParameterDefn(module, istrings.idSelf);
  selfParam->setType(type);
  selfParam->setInternalType(type);
  selfParam->addTrait(Defn::Singular);
  selfParam->setFlag(ParameterDefn::Reference, true);
  LValueExpr * selfExpr = new LValueExpr(target->location(), NULL, selfParam);

  //if (classType->getKind() == Type::Struct) {
  // The 'self' param of struct methods is passed by reference instead of by
  // value as normal.
  //  selfParam->setParameterFlag(ParameterDef::Reference, true);
  //}
  //requiredParams.push_back(selfParam);

  Block * constructorBody = new Block("entry");
  constructorBody->exitReturn(target->location(), NULL);
  for (Defn * de = type->firstMember(); de != NULL; de = de->nextInScope()) {
    if (de->storageClass() == Storage_Instance) {
      if (de->defnType() == Defn::Let) {
        VariableDefn * let = static_cast<VariableDefn *>(de);
        //analyze(let);

        // TODO: Write tests for this case (instance lets)
        if (let->initValue() != NULL) {
          // We need a better way to designate which lets require runtime init.
          DFAIL("Implement me!");
        }
      } else if (de->defnType() == Defn::Var) {
        VariableDefn * memberVar = static_cast<VariableDefn *>(de);
        analyzeValueDefn(memberVar, Task_PrepCallOrUse);
        Expr * defaultValue = memberVar->initValue();
        Type * memberType = memberVar->type().type();
        if (defaultValue == NULL) {
          // TODO: If this is 'final' it must be initialized here or in
          // the constructor.
          defaultValue = memberType->nullInitValue();
          // TODO: Must be a constant...?
          if (defaultValue && !defaultValue->isConstant()) {
            defaultValue = NULL;
          }
        }

        Expr * initVal;
        if (memberType->typeClass() == Type::NativeArray) {
          // TODO: If this array is non-zero size, we have a problem I think.
          // Native arrays must be initialized in the constructor.
          continue;
        } else if (memberVar->visibility() == Public) {
          ParameterDefn * param = new ParameterDefn(module, memberVar->name());
          param->setLocation(target->location());
          param->setType(memberType);
          param->setInternalType(memberType);
          param->addTrait(Defn::Singular);
          param->finishPass(Pass_ResolveVarType);
          param->setDefaultValue(defaultValue);

          if (defaultValue != NULL) {
            optionalParams.push_back(param);
          } else {
            requiredParams.push_back(param);
          }

          initVal = new LValueExpr(target->location(), NULL, param);
        } else {
          if (defaultValue != NULL) {
            // TODO: This doesn't work because native pointer initializations
            // are the wrong type.
            initVal = defaultValue;
            continue;
          } else if (type == Builtins::typeObject) {
            continue;
          } else {
            // TODO: Write tests for this case (private instance variables
            // being initialized to default values.)
            diag.fatal(de) << "Unimplemented default initialization: " << de;
            DFAIL("Implement");
            continue;
          }
        }

        LValueExpr * memberExpr = new LValueExpr(target->location(), selfExpr, memberVar);
        Expr * initExpr = new AssignmentExpr(target->location(), memberExpr, initVal);
        constructorBody->append(initExpr);
        //diag.info(de) << "Uninitialized field " << de->qualifiedName() << " with default value " << initExpr;
        //DFAIL("Implement");
      }
    }
  }

  // Optional params go after required params.
  ParameterList params(requiredParams);
  params.append(optionalParams.begin(), optionalParams.end());

  FunctionType * funcType = new FunctionType(&VoidType::instance, params);
  funcType->setSelfParam(selfParam);
  FunctionDefn * constructorDef = new FunctionDefn(Defn::Function, module, istrings.idConstruct);
  constructorDef->setFunctionType(funcType);
  constructorDef->setLocation(target->location());
  constructorDef->setStorageClass(Storage_Instance);
  constructorDef->addTrait(Defn::Ctor);
  constructorDef->addTrait(Defn::Ctor);
  constructorDef->copyTrait(target, Defn::Synthetic);
  constructorDef->blocks().push_back(constructorBody);
  constructorDef->finished().addAll(
      DefnPasses::of(
          Pass_ResolveAttributes,
          Pass_CreateCFG,
          Pass_ResolveParameterTypes,
          Pass_ResolveReturnType));

  //constructorDef->setBody(constructorBody);
  if (target->isSingular()) {
    constructorDef->addTrait(Defn::Singular);

    // If it's synthetic, then don't add the constructor unless someone
    // actually calls it.
    if (!target->isSynthetic()) {
      module->addSymbol(constructorDef);
    }
  }

  DASSERT_OBJ(constructorDef->isSingular(), constructorDef);
  if (!funcType->isSingular()) {
    diag.fatal(target) << "Default constructor type " << funcType << " is not singular";
    funcType->whyNotSingular();
  }

  type->addMember(constructorDef);
  constructorDef->createQualifiedName(target);
  return true;
}

bool ClassAnalyzer::analyzeStaticInitializers() {
  bool success = true;

  if (target->beginPass(Pass_ResolveStaticInitializers)) {
    CompositeType * type = cast<CompositeType>(target->typeValue());
    CompositeType * super = type->super();

    if (super != NULL) {
      DASSERT_OBJ(super->typeDefn()->isPassFinished(Pass_ResolveStaticInitializers), super);
    }

    for (Defn * member = type->firstMember(); member != NULL; member = member->nextInScope()) {
      if (VariableDefn * var = dyn_cast<VariableDefn>(member)) {
        analyzeValueDefn(var, Task_PrepCodeGeneration);
      }
    }

    /*for (DefnList::iterator it = type->staticFields_.begin(); it != type->staticFields_.end();
        ++it) {
      VariableDefn * var = cast<VariableDefn>(*it);
      if (var->initValue() != NULL) {
        Expr * initVal = var->initValue();
        Expr * constInitVal = EvalPass::eval(initVal, true);
        if (constInitVal != NULL) {
          var->setInitValue(constInitVal);
        } else {
          DFAIL("Implement");
        }
      }
    }*/

    target->finishPass(Pass_ResolveStaticInitializers);
  }

  return success;
}

}
