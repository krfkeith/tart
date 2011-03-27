/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Expr/Exprs.h"
#include "tart/Expr/StmtExprs.h"
#include "tart/Type/CompositeType.h"
#include "tart/Type/NativeType.h"
#include "tart/Type/FunctionType.h"
#include "tart/Defn/FunctionDefn.h"
#include "tart/Defn/PropertyDefn.h"
#include "tart/Type/PrimitiveType.h"
#include "tart/Type/UnionType.h"
#include "tart/Type/TupleType.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/Template.h"
#include "tart/Defn/Module.h"

#include "tart/Common/Diagnostics.h"
#include "tart/Common/InternedString.h"

#include "tart/Sema/ClassAnalyzer.h"
#include "tart/Sema/TypeAnalyzer.h"
#include "tart/Sema/FunctionAnalyzer.h"
#include "tart/Sema/ExprAnalyzer.h"

#include "tart/Objects/Builtins.h"
#include "tart/Objects/SystemDefs.h"

namespace tart {

static const CompositeType::PassSet PASS_SET_RESOLVE_TYPE = CompositeType::PassSet::of(
  CompositeType::ScopeCreationPass,
  CompositeType::BaseTypesPass
);

static const CompositeType::PassSet PASS_SET_LOOKUP = CompositeType::PassSet::of(
  CompositeType::ScopeCreationPass,
  CompositeType::BaseTypesPass,
  CompositeType::AttributePass
);

static const CompositeType::PassSet PASS_SET_CONSTRUCTION = CompositeType::PassSet::of(
  CompositeType::ScopeCreationPass,
  CompositeType::BaseTypesPass,
  CompositeType::AttributePass,
  CompositeType::ImportPass,
  CompositeType::NamingConflictPass,
  CompositeType::ConstructorPass,
  CompositeType::FieldPass
);

static const CompositeType::PassSet PASS_SET_CONVERSION = CompositeType::PassSet::of(
  CompositeType::ScopeCreationPass,
  CompositeType::BaseTypesPass,
  CompositeType::AttributePass,
  CompositeType::ImportPass,
  CompositeType::NamingConflictPass,
  CompositeType::CoercerPass
);

static const CompositeType::PassSet PASS_SET_EVALUATION = CompositeType::PassSet::of(
  CompositeType::ScopeCreationPass,
  CompositeType::BaseTypesPass,
  CompositeType::AttributePass,
  CompositeType::ImportPass,
  CompositeType::NamingConflictPass,
  CompositeType::CoercerPass,
  CompositeType::MemberTypePass,
  CompositeType::FieldPass,
  CompositeType::MethodPass,
  CompositeType::OverloadingPass
);

static const CompositeType::PassSet PASS_SET_TYPEGEN = CompositeType::PassSet::of(
  CompositeType::ScopeCreationPass,
  CompositeType::BaseTypesPass,
  CompositeType::ImportPass,
  CompositeType::NamingConflictPass,
  CompositeType::AttributePass,
  CompositeType::FieldPass,
  CompositeType::RecursiveFieldTypePass,
  CompositeType::MethodPass,
  CompositeType::OverloadingPass
);

static const CompositeType::PassSet PASS_SET_CODEGEN = CompositeType::PassSet::of(
  CompositeType::ScopeCreationPass,
  CompositeType::BaseTypesPass,
  CompositeType::AttributePass,
  CompositeType::ImportPass,
  CompositeType::NamingConflictPass,
  CompositeType::CoercerPass,
  CompositeType::ConstructorPass,
  CompositeType::MemberTypePass,
  CompositeType::FieldPass,
  CompositeType::MethodPass,
  CompositeType::OverloadingPass,
  CompositeType::CompletionPass
);

ClassAnalyzer::ClassAnalyzer(TypeDefn * de)
  : DefnAnalyzer(de->module(), de->definingScope(), de, NULL)
  , target(de)
  , trace_(isTraceEnabled(de))
{
  DASSERT(de != NULL);
  DASSERT(isa<CompositeType>(target->typeValue()));
}

CompositeType * ClassAnalyzer::targetType() const {
  return static_cast<CompositeType *>(target->typeValue());
}

bool ClassAnalyzer::analyze(AnalysisTask task) {
  TaskInProgress tip(target, task);

  switch (task) {
    case Task_PrepTypeComparison:
      return runPasses(PASS_SET_RESOLVE_TYPE);

    case Task_PrepMemberLookup:
      return runPasses(PASS_SET_LOOKUP);

    case Task_PrepConstruction:
      return runPasses(PASS_SET_CONSTRUCTION);

    case Task_PrepConversion:
      return runPasses(PASS_SET_CONVERSION);

    case Task_PrepEvaluation:
      return runPasses(PASS_SET_EVALUATION);

    case Task_PrepTypeGeneration:
      return runPasses(PASS_SET_TYPEGEN);

    case Task_PrepCodeGeneration:
      return runPasses(PASS_SET_CODEGEN);

    default:
      return true;
  }
}

bool ClassAnalyzer::runPasses(CompositeType::PassSet passesToRun) {
  // Work out what passes need to be run.
  CompositeType * type = targetType();
  passesToRun.removeAll(type->passes().finished());
  if (passesToRun.empty()) {
    return true;
  }

  if (trace_) {
    diag.debug(target) << Format_Verbose << "Analyzing: " << target;
  }

  AutoIndent A(trace_);

  // Skip analysis of templates - for now.
  if (target->isTemplate()) {
    // Get the template scope and set it as the active scope.
    analyzeTemplateSignature(target);

    if (passesToRun.contains(CompositeType::BaseTypesPass) && !analyzeBaseClasses()) {
      return false;
    }

    if (passesToRun.contains(CompositeType::ScopeCreationPass) &&
        type->passes().begin(CompositeType::ScopeCreationPass)) {
      if (trace_) {
        diag.debug() << "Scope creation";
      }
      if (!createMembersFromAST(target)) {
        return false;
      }

      type->passes().finish(CompositeType::ScopeCreationPass);
    }

    if (target->hasUnboundTypeParams()) {
      return true;
    }
  }

  if (target->isTemplateMember()) {
    return true;
  }

  if (passesToRun.contains(CompositeType::ScopeCreationPass) &&
      type->passes().begin(CompositeType::ScopeCreationPass)) {
    if (trace_) {
      diag.debug() << "Scope creation";
    }
    if (!createMembersFromAST(target)) {
      return false;
    }

    type->passes().finish(CompositeType::ScopeCreationPass);
  }

  if (passesToRun.contains(CompositeType::AttributePass) &&
      type->passes().begin(CompositeType::AttributePass)) {
    if (trace_) {
      diag.debug() << "Attributes";
    }
    if (!resolveAttributes(target)) {
      return false;
    }

    type->passes().finish(CompositeType::AttributePass);
  }

  if (passesToRun.contains(CompositeType::ImportPass) && !analyzeImports()) {
    return false;
  }

  if (passesToRun.contains(CompositeType::NamingConflictPass) && !checkNameConflicts()) {
    return false;
  }

  if (passesToRun.contains(CompositeType::BaseTypesPass) && !analyzeBaseClasses()) {
    return false;
  }

  if (passesToRun.contains(CompositeType::MemberTypePass) && !analyzeMemberTypes()) {
    return false;
  }

  if (passesToRun.contains(CompositeType::FieldPass) && !analyzeFields()) {
    return false;
  }

  if (passesToRun.contains(CompositeType::CoercerPass) && !analyzeCoercers()) {
    return false;
  }

  if (passesToRun.contains(CompositeType::ConstructorPass) && !analyzeConstructors()) {
    return false;
  }

  if (passesToRun.contains(CompositeType::MethodPass) && !analyzeMethods()) {
    return false;
  }

  if (passesToRun.contains(CompositeType::OverloadingPass) && !analyzeOverloading()) {
    return false;
  }

  if (passesToRun.contains(CompositeType::RecursiveFieldTypePass) &&
      !analyzeFieldTypesRecursive()) {
    return false;
  }

  if (passesToRun.contains(CompositeType::CompletionPass) && !analyzeCompletely()) {
    return false;
  }

  return true;
}

bool ClassAnalyzer::checkNameConflicts() {
  CompositeType * type = targetType();
  bool success = true;
  if (type->passes().begin(CompositeType::NamingConflictPass)) {
    if (trace_) {
      diag.debug() << "Check name conflicts";
    }
    Defn::DefnType dtype = target->defnType();
    const SymbolTable & symbols = type->members();
    for (SymbolTable::const_iterator entry = symbols.begin(); entry != symbols.end(); ++entry) {
      const SymbolTable::Entry & defns = entry->second;
      Defn::DefnType dtype = defns.front()->defnType();

      // First insure that all entries are the same type
      for (SymbolTable::Entry::const_iterator it = defns.begin(); it != defns.end(); ++it) {
        Defn * de = *it;
        if (de->defnType() != dtype) {
          diag.error(de) << "Definition of '" << de->name() << "' as '" << de <<
              "' conflicts with earlier definition:";
          diag.info(defns.front()) << defns.front();
          success = false;
          break;
        }
      }
    }

    type->passes().finish(CompositeType::NamingConflictPass);
  }

  return success;
}

bool ClassAnalyzer::analyzeBaseClasses() {
  CompositeType * type = targetType();
  if (type->passes().isRunning(CompositeType::BaseTypesPass)) {
    //diag.error(target) << "Circular inheritance not allowed";
    //return false;
    return true;
  }

  if (!type->passes().begin(CompositeType::BaseTypesPass)) {
    return true;
  }

  if (trace_) {
    diag.debug() << "Base classes";
  }

  bool result = analyzeBaseClassesImpl();
  type->passes().finish(CompositeType::BaseTypesPass);
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

  CompositeType * type = targetType();
  bool isFromTemplate =
      target->isTemplate() || target->isTemplateMember() || target->isPartialInstantiation();
  DASSERT_OBJ(isFromTemplate || type->isSingular(), type);
  DASSERT_OBJ(type->super() == NULL, type);

  // Check for valid finality
  if (type->isFinal()) {
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
  ta.setTypeLookupOptions(isFromTemplate ? LOOKUP_NO_RESOLVE : LOOKUP_DEFAULT);

  // Mark the base type pass as finished early, allowing inherited symbols from one
  // base to be used as template parameters for the next base.
  type->passes().finish(CompositeType::BaseTypesPass);
  ta.setActiveScope(type->memberScope());

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

    if (CompositeType * baseClass = dyn_cast<CompositeType>(baseType)) {
      if (baseClass->isFinal()) {
        diag.error(*it) << "Base type '" << baseDefn << "' is final";
      }
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
            // TODO: Insure last field of base isn't a flexible array.
            isPrimary = true;
          } else {
            diag.error(target) << "classes can only have a single concrete supertype";
          }
        } else if (baseKind != Type::Interface && baseKind != Type::Protocol) {
          diag.error(target) << "class '" << target <<
              "' can only inherit from a class, interface or protocol";
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
      baseClass->addBaseXRefs(module_);
    }

    if (baseClass->isSubclassOf(type)) {
      diag.error(target) << "Circular inheritance not allowed";
      return false;
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
    if (target->isSingular()) {
      module_->addSymbol(primaryBase->typeDefn());
    }
  }

  type->setSuper(primaryBase);

  // define the super type
  if (primaryBase != NULL) {
    // Move the primary base to be first in the list.
    type->bases().insert(type->bases().begin(), primaryBase);
    propagateSubtypeAttributes(primaryBase->typeDefn(), target);
  }

  if (dtype == Type::Interface && Builtins::funcTypecastError != NULL) {
    module_->addSymbol(Builtins::funcTypecastError);
  }

  return true;
}

bool ClassAnalyzer::analyzeImports() {
  CompositeType * type = targetType();
  if (type->passes().begin(CompositeType::ImportPass)) {
    if (trace_) {
      diag.debug() << "Imports";
    }
    if (target->ast() != NULL) {
      DefnAnalyzer da(target->module(), type->memberScope(), target, NULL);
      const ASTNodeList & imports = target->ast()->imports();
      for (ASTNodeList::const_iterator it = imports.begin(); it != imports.end(); ++it) {
        da.importIntoScope(cast<ASTImport>(*it), type->memberScope());
      }
    }

    type->passes().finish(CompositeType::ImportPass);
  }

  return true;
}

bool ClassAnalyzer::analyzeCoercers() {
  CompositeType * type = targetType();
  if (type->passes().begin(CompositeType::CoercerPass)) {
    if (trace_) {
      diag.debug() << "Coercers";
    }
    Type::TypeClass tcls = type->typeClass();
    if (tcls == Type::Class || tcls == Type::Struct) {
      // Note: "coerce" methods are *not* inherited.
      DefnList methods;
      if (type->lookupMember(istrings.idCoerce, methods, false)) {
        for (DefnList::iterator it = methods.begin(); it != methods.end(); ++it) {
          if (FunctionDefn * fn = dyn_cast<FunctionDefn>(*it)) {
            diag.recovered();

            if (FunctionAnalyzer(fn).analyze(Task_PrepTypeComparison) &&
                !fn->returnType()->isVoidType() &&
                fn->storageClass() == Storage_Static &&
                fn->params().size() == 1) {

              // Mark the constructor as singular if in fact it is.
              if (!fn->hasUnboundTypeParams() && type->isSingular()) {
                fn->addTrait(Defn::Singular);
              }

              type->coercers_.push_back(fn);
            }
          }
        }
      }
    }

    type->passes().finish(CompositeType::CoercerPass);
  }

  return true;
}

bool ClassAnalyzer::analyzeMemberTypes() {
  CompositeType * type = targetType();
  if (type->passes().begin(CompositeType::MemberTypePass)) {
    if (trace_) {
      diag.debug() << "Member types";
    }

    for (Defn * member = type->firstMember(); member != NULL; member = member->nextInScope()) {
      if (TypeDefn * memberType = dyn_cast<TypeDefn>(member)) {
        // TODO: Copy attributes that are inherited.
        memberType->copyTrait(target, Defn::Reflect);
        switch (memberType->typeValue()->typeClass()) {
          case Type::Class:
          case Type::Struct:
          case Type::Interface:
          case Type::Enum:
            module_->addSymbol(memberType);
            break;

          default:
            break;
        }
      }
    }

    type->passes().finish(CompositeType::MemberTypePass);
  }

  return true;
}

bool ClassAnalyzer::analyzeFields() {
  CompositeType * type = targetType();
  if (type->passes().begin(CompositeType::FieldPass)) {
    if (trace_) {
      diag.debug() << "Fields";
    }

    CompositeType * super = type->super();
    // Also analyze base class fields.
    int instanceFieldCount = 0;
    int instanceFieldCountRecursive = 0;
    if (super != NULL) {
      // The extra check is to prevent infinite recursion when analyzing class Object.
      if (!super->passes().isFinished(CompositeType::FieldPass)) {
        ClassAnalyzer(super->typeDefn()).analyze(Task_PrepConstruction);
      }

      // Reserve one slot for the superclass.
      type->instanceFields_.push_back(NULL);
      instanceFieldCount = 1;
      instanceFieldCountRecursive = super->instanceFieldCountRecursive();
      DASSERT(instanceFieldCountRecursive >= 0);
      DASSERT(type->instanceFieldCountRecursive() == instanceFieldCountRecursive);
    }

    for (Defn * member = type->firstMember(); member != NULL; member = member->nextInScope()) {
      switch (member->defnType()) {
        case Defn::Var:
        case Defn::Let: {
          VariableDefn * field = static_cast<VariableDefn *>(member);
          //field->copyTrait(target, Defn::Final);
          field->copyTrait(target, Defn::Reflect);

          if (field->passes().isRunning(VariableDefn::VariableTypePass)) {
            continue;
          }

          analyzeVariable(field, Task_PrepTypeComparison);
          DASSERT(field->type() != NULL);

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

              // Special case for non-reflective classes, we need to also export the types
              // of members.
              //if (target->isNonreflective()) {
              //  module->addSymbol(field);
              //}

              if (!type->isAttribute() && type != Builtins::typeObject) {
                analyzeType(field->type(), Task_PrepConstruction);
              }

              //analyzeType(field->type(), Task_PrepTypeGeneration);
            } else if (field->storageClass() == Storage_Static) {
              module_->addSymbol(field);
              type->staticFields_.push_back(field);
            }
          }

          break;
        }

        case Defn::Namespace: {
          //DFAIL("Implement");
          break;
        }

        default:
          break;
      }
    }

    DASSERT(type->instanceFields_.size() == size_t(instanceFieldCount));
    DASSERT(type->instanceFieldCountRecursive() == instanceFieldCountRecursive);
    type->passes().finish(CompositeType::FieldPass);

    for (size_t i = 0; i + 1 < type->instanceFields_.size(); ++i) {
      VariableDefn * field = cast_or_null<VariableDefn>(type->instanceFields_[i]);
      if (field != NULL) {
        const Type * fieldType = dealias(field->type());
        if (fieldType->typeClass() == Type::FlexibleArray) {
          diag.error(field) << "Member of type FlexibleArray must be the last member.";
        }
      }
    }
  }

  return true;
}

bool ClassAnalyzer::analyzeConstructors() {
  // Analyze the constructors before methods, because we may need them
  // during the rest of the analysis.
  CompositeType * type = targetType();
  if (type->passes().begin(CompositeType::ConstructorPass)) {
    if (trace_) {
      diag.debug() << "Constructors";
    }

    // Closures don't have constructors
    if (type->getClassFlag(CompositeType::Closure)) {
      type->passes().finish(CompositeType::ConstructorPass);
      return true;
    }

    Type::TypeClass tcls = type->typeClass();
    if (tcls == Type::Class || tcls == Type::Struct) {
      // Analyze superclass constructors
      if (type->super() != NULL &&
          !type->super()->passes().isFinished(CompositeType::ConstructorPass) &&
          !type->super()->passes().isRunning(CompositeType::ConstructorPass)) {
        ClassAnalyzer ca(type->super()->typeDefn());
        if (!ca.analyze(Task_PrepConstruction)) {
          return false;
        }
      }

      DefnList ctors;
      bool hasConstructors = false;
      if (type->lookupMember(istrings.idConstruct, ctors, false)) {
        for (DefnList::iterator it = ctors.begin(); it != ctors.end(); ++it) {
          if (FunctionDefn * ctor = dyn_cast<FunctionDefn>(*it)) {
            diag.recovered();

            hasConstructors = true;
            ctor->setFlag(FunctionDefn::Ctor);

            if (!FunctionAnalyzer(ctor).analyze(Task_PrepTypeComparison)) {
              continue;
            }

            if (ctor->returnType() == NULL) {
              ctor->functionType()->setReturnType(&VoidType::instance);
            }

            if (!ctor->returnType()->isVoidType()) {
              diag.fatal(ctor) << "Constructor cannot declare a return type.";
              break;
            }

            if (ctor->storageClass() != Storage_Instance) {
              diag.fatal(ctor) << "Constructor must be instance method.";
              break;
            }

            if (!ctor->hasUnboundTypeParams() && type->isSingular()) {
              // Mark the constructor as singular if in fact it is.
              ctor->addTrait(Defn::Singular);
            }

            ctor->copyTrait(target, Defn::Reflect);
            analyzeConstructBase(ctor);
          } else {
            diag.fatal(*it) << "Member named 'construct' must be a method.";
            break;
          }
        }
      }

      // Look for creator functions.
      ctors.clear();
      if (type->lookupMember(istrings.idCreate, ctors, false)) {
        for (DefnList::iterator it = ctors.begin(); it != ctors.end(); ++it) {
          if (FunctionDefn * ctor = dyn_cast<FunctionDefn>(*it)) {
            diag.recovered();
            if (ctor->storageClass() == Storage_Static) {
              hasConstructors = true;
            }

            if (!FunctionAnalyzer(ctor).analyze(Task_PrepTypeComparison)) {
              continue;
            }

            ctor->copyTrait(target, Defn::Reflect);
            // TODO: check return type.
          }
        }
      }

      if (!hasConstructors && type != Builtins::typeTypeInfoBlock) {
        //createDefaultConstructor();
        createNoArgConstructor();
      }
    }

    type->passes().finish(CompositeType::ConstructorPass);
  }

  return true;
}

void ClassAnalyzer::analyzeConstructBase(FunctionDefn * ctor) {
  CompositeType * type = targetType();
  CompositeType * superType = cast_or_null<CompositeType>(type->super());
/*  if (superType != NULL) {
    BlockList & blocks = ctor->blocks();
    for (BlockList::iterator blk = blocks.begin(); blk != blocks.end(); ++blk) {
      ExprList & exprs = (*blk)->exprs();
      for (ExprList::iterator e = exprs.begin(); e != exprs.end(); ++e) {
        //if (e->exprType() ==
      }
    }
  }*/
}

bool ClassAnalyzer::analyzeMethods() {
  CompositeType * type = targetType();
  if (type->passes().begin(CompositeType::MethodPass)) {
    if (trace_) {
      diag.debug() << "Methods";
    }
    Defn::DefnType dtype = target->defnType();

    // Analyze all methods
    for (Defn * member = type->firstMember(); member != NULL; member = member->nextInScope()) {
      if (METHOD_DEFS.contains(member->defnType()) || member->defnType() == Defn::Property) {
        if (member->isTemplate()) {
          analyzeTemplateSignature(member);
          if (member->hasUnboundTypeParams()) {
            continue;
          }
        }

        if (member->visibility() != Public) {
          if (type->typeClass() == Type::Interface || type->typeClass() == Type::Protocol) {
            diag.error(target) << "Interface or protocol method cannot be non-public";
            continue;
          }
        }

        if (FunctionDefn * fn = dyn_cast<FunctionDefn>(member)) {
          if (type->typeClass() == Type::Interface || type->typeClass() == Type::Protocol) {
            if (fn->isFinal()) {
               diag.error(target) << "Interface or protocol method cannot be final";
               continue;
             }
           }
        }

        member->copyTrait(target, Defn::Reflect);

        if (FunctionDefn * val = dyn_cast<FunctionDefn>(member)) {
          analyzeFunction(val, Task_PrepTypeComparison);
        } else if (PropertyDefn * prop = dyn_cast<PropertyDefn>(member)) {
          analyzeProperty(prop, Task_PrepTypeComparison);
        }
      }
    }

    const SymbolTable & symbols = type->members();
    for (SymbolTable::const_iterator entry = symbols.begin(); entry != symbols.end(); ++entry) {
      const SymbolTable::Entry & defns = entry->second;
      Defn::DefnType dtype = defns.front()->defnType();

      if (METHOD_DEFS.contains(dtype) || dtype == Defn::Property) {
        for (SymbolTable::Entry::const_iterator it = defns.begin(); it != defns.end(); ++it) {
          ValueDefn * val = cast<ValueDefn>(*it);
          if (val->hasUnboundTypeParams()) {
            continue;
          }

          // Compare with all previous defns
          for (SymbolTable::Entry::const_iterator m = defns.begin(); m != it; ++m) {
            ValueDefn * prevVal = cast<ValueDefn>(*m);
            if (prevVal->hasUnboundTypeParams()) {
              continue;
            }

            if (dtype == Defn::Property) {
              PropertyDefn * p1 = cast<PropertyDefn>(val);
              PropertyDefn * p2 = cast<PropertyDefn>(prevVal);
              if (p1->type()->isEqual(p2->type())) {
                diag.error(p2) << "Definition of property << '" << p2 <<
                    "' conflicts with earlier definition:";
                diag.info(p1) << p1;
              }
            } else if (dtype == Defn::Indexer) {
              IndexerDefn * i1 = cast<IndexerDefn>(val);
              IndexerDefn * i2 = cast<IndexerDefn>(prevVal);
            } else {
              FunctionDefn * f1 = cast<FunctionDefn>(val);
              FunctionDefn * f2 = cast<FunctionDefn>(prevVal);
              if (f1->hasSameSignature(f2)) {
                diag.error(f2) << "Member type signature conflict";
                diag.info(f1) << "From here";
              }
            }
          }
        }
      }
    }

    type->passes().finish(CompositeType::MethodPass);
  }

  return true;
}

bool ClassAnalyzer::analyzeOverloading() {
  CompositeType * type = targetType();
  if (type->passes().begin(CompositeType::OverloadingPass)) {
    if (trace_) {
      diag.debug() << "Overloading";
    }

    if (!target->isSingular()) {
      type->passes().finish(CompositeType::OverloadingPass);
      return true;
    }

    // Do overload analysis on all bases
    ClassList & bases = type->bases();
    for (ClassList::iterator it = bases.begin(); it != bases.end(); ++it) {
      analyzeTypeDefn((*it)->typeDefn(), Task_PrepEvaluation);
    }

    copyBaseClassMethods();
    createInterfaceTables();
    overrideMembers();
    addNewMethods();
    checkForRequiredMethods();

    type->passes().finish(CompositeType::OverloadingPass);
  }

  return true;
}

void ClassAnalyzer::copyBaseClassMethods() {
  // If it's not a normal class, it can still have a supertype.
  CompositeType * type = targetType();
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
  CompositeType * type = targetType();
  type->ancestorClasses(ancestors);

  // Remove from the set all types which are the first parent of some other type
  // that is already in the set, since they can use the same dispatch table.
  ClassSet interfaceTypes(ancestors);
  //ancestors.insert(type);

/*  for (ClassSet::iterator it = ancestors.begin(); it != ancestors.end(); ++it) {
    CompositeType * base = *it;

    // The first parent of each parent can always be removed, since the itable
    // of any class is always a superset of the itable of its first parent.
    if (!base->bases().empty()) {
      CompositeType * baseBase = base->bases().front();
      interfaceTypes.remove(baseBase);
    }
  }*/

  // Create the tables for each interface that remains.
  for (ClassSet::iterator it = ancestors.begin(); it != ancestors.end(); ++it) {
    CompositeType * itype = *it;
    if (itype->typeClass() != Type::Interface) {
      continue;
    }

    //if (!interfaceTypes.count(itype)) {
    //  continue;
    //}

    // Do the search before we push the new itable entry.
    const CompositeType::InterfaceTable * parentImpl = type->findBaseImplementationOf(itype);

    // Add an itable entry.
    type->interfaces_.push_back(CompositeType::InterfaceTable(itype));
    CompositeType::InterfaceTable & itable = type->interfaces_.back();

    // Insert the interface, and it's first parents into the list, but only if they
    // aren't already on some other list.
    /*(for (CompositeType * iface = itype; iface != NULL; ) {
      if (!interfaceTypes.count(iface)) {
        break;
      }

      itable.ifaces.insert(iface);
      interfaceTypes.remove(iface);

      if (!iface->bases().empty()) {
        iface = iface->bases().front();
      } else {
        break;
      }
    }*/

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
  CompositeType * type = targetType();
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
          if (!func->isInterfaceMethod()) {
            module_->addSymbol(func);
          }
          if (func->storageClass() == Storage_Instance && !func->isCtor()) {
            methods.push_back(func);
          }
        }
      } else if ((*it)->defnType() == Defn::Property || (*it)->defnType() == Defn::Indexer) {
        prop = cast<PropertyDefn>(*it);
        if (prop->storageClass() == Storage_Instance && prop->isSingular()) {
          DASSERT_OBJ(prop->passes().isFinished(PropertyDefn::PropertyTypePass), prop);
          if (prop->getter() != NULL) {
            analyzeFunction(prop->getter(), Task_PrepTypeGeneration);
            getters.push_back(prop->getter());
          }

          if (prop->setter() != NULL) {
            analyzeFunction(prop->setter(), Task_PrepTypeGeneration);
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
      overridePropertyAccessors(type->instanceMethods_, prop, getters, true);
      for (InterfaceList::iterator it = ifaceList.begin(); it != ifaceList.end(); ++it) {
        overridePropertyAccessors(it->methods, prop, getters, false);
      }
    }

    if (!setters.empty()) {
      ensureUniqueSignatures(setters);
      overridePropertyAccessors(type->instanceMethods_, prop, setters, true);
      for (InterfaceList::iterator it = ifaceList.begin(); it != ifaceList.end(); ++it) {
        overridePropertyAccessors(it->methods, prop, setters, false);
      }
    }
  }
}

void ClassAnalyzer::ensureUniqueSignatures(MethodList & methods) {
  for (size_t i = 0; i < methods.size(); ++i) {
    for (size_t j = i + 1; j < methods.size(); ++j) {
      if (methods[i]->hasSameSignature(methods[j])) {
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
  CompositeType * type = targetType();
  for (Defn * de = type->firstMember(); de != NULL; de = de->nextInScope()) {
    if (de->storageClass() == Storage_Instance && de->isSingular()) {
      Defn::DefnType dt = de->defnType();
      if (dt == Defn::Function) {
        FunctionDefn * fn = static_cast<FunctionDefn *>(de);
        if (fn->isUndefined() && fn->overriddenMethods().empty()) {
          if (!fn->isCtor() || !fn->params().empty()) {
            diag.error(fn) << "Method '" << fn->name() <<
                "' defined with 'undef' but does not override a base class method.";
          }
        } else if (fn->isOverride()) {
          // TODO: Implement (what?)
          if (fn->overriddenMethods().empty()) {
            diag.error(de) << de << " huh?";
          }
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

  CompositeType * type = targetType();
  if (type->isAbstract()) {
    return;
  }

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
      if (tcls == Type::Struct || (tcls == Type::Class)) {
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
    bool isClassTable) {
  // 'table' is the set of methods inherited from the superclass or interface.
  // 'overrides' is all of the methods defined in *this* class that share the same name.
  // 'canHide' is true if 'overrides' are from a class, false if from an interface.
  const char * name = overrides.front()->name();
  size_t tableSize = table.size();
  for (size_t i = 0; i < tableSize; ++i) {
    // For every inherited method whose name matches the name of the overrides.
    // See if there is a new method that goes in that same slot
    FunctionDefn * m = table[i];
    if (m->name() == name) {
      FunctionDefn * newMethod = findOverride(m, overrides);
      if (newMethod != NULL) {
        table[i] = newMethod;
        if (isClassTable) {
          if (newMethod->dispatchIndex() < 0) {
            newMethod->setDispatchIndex(i);
          }

          if ((m->hasBody() || m->isExtern()) && !newMethod->isOverride()) {
            diag.recovered();
            diag.error(newMethod) << "Method '" << newMethod->name() <<
                "' which overrides method in base class '" << m->parentDefn()->qualifiedName() <<
                "' should be declared with 'override'";
          }
        }

        if (newMethod->isIntrinsic()) {
          diag.error(newMethod) << "Intrinsic methods cannot override base class methods.";
        }

        newMethod->overriddenMethods().insert(m);
      } else if (isClassTable) {
        diag.recovered();
        diag.warn(m) << "Definition of '" << m << "' is hidden";
        for (MethodList::const_iterator it = overrides.begin(); it != overrides.end(); ++it) {
          diag.info(*it) << "by '" << *it << "'";
        }
      }
    }
  }
}

void ClassAnalyzer::overridePropertyAccessors(MethodList & table, PropertyDefn * prop,
    const MethodList & accessors, bool isClassTable) {
  const char * name = accessors.front()->name();
  size_t tableSize = table.size();
  for (size_t i = 0; i < tableSize; ++i) {
    FunctionDefn * m = table[i];
    if (PropertyDefn * p = dyn_cast_or_null<PropertyDefn>(m->parentDefn())) {
      if (m->name() == name && p->name() == prop->name()) {
        FunctionDefn * newAccessor = findOverride(m, accessors);
        if (newAccessor != NULL) {
          table[i] = newAccessor;
          if (isClassTable && newAccessor->dispatchIndex() < 0) {
            newAccessor->setDispatchIndex(i);
          }

          if (isClassTable && (m->hasBody() || m->isExtern()) && !newAccessor->isOverride()) {
            diag.error(newAccessor) << "Property '" << newAccessor->name() <<
                "' which overrides property in base class '" << m->parentDefn()->qualifiedName() <<
                "' should be declared with 'override'";
          }

          newAccessor->overriddenMethods().insert(m);
        } else {
          diag.recovered();
          diag.warn(m) << "Invalid override of property accessor '" << m
              << "' by accessor of incompatible type:";
          for (MethodList::const_iterator it = accessors.begin(); it != accessors.end(); ++it) {
            diag.info(*it) << "by '" << *it << "'";
          }
        }
      }
    }
  }
}

FunctionDefn * ClassAnalyzer::findOverride(const FunctionDefn * f, const MethodList & overrides) {
  for (MethodList::const_iterator it = overrides.begin(); it != overrides.end(); ++it) {
    if ((*it)->canOverride(f)) {
      return *it;
    }
  }

  return NULL;
}

bool ClassAnalyzer::createDefaultConstructor() {
  if (trace_) {
    diag.debug() << "Default constructor";
  }
  // Determine if the superclass has a default constructor. If it doesn't,
  // then we cannot make a default constructor.
  CompositeType * type = targetType();
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
  ParameterDefn * selfParam = new ParameterDefn(module_, istrings.idSelf);
  selfParam->setType(type);
  selfParam->setInternalType(type);
  selfParam->addTrait(Defn::Singular);
  selfParam->setFlag(ParameterDefn::Reference, true);
  LValueExpr * selfExpr = LValueExpr::get(SourceLocation(), NULL, selfParam);

  //Block * constructorBody = new Block("ctor_entry");
  SeqExpr * constructorBody = new SeqExpr(SourceLocation(), &VoidType::instance);
  //constructorBody->exitReturn(SourceLocation(), NULL);

  // TODO: Call the super ctor;
  DASSERT_OBJ(superCtor == NULL, target);

  for (Defn * de = type->firstMember(); de != NULL; de = de->nextInScope()) {
    if (de->storageClass() == Storage_Instance) {
      if (de->defnType() == Defn::Let || de->defnType() == Defn::Var) {
        VariableDefn * field = static_cast<VariableDefn *>(de);
        analyzeVariable(field, Task_PrepConstruction);
        Expr * initValue = getFieldInitVal(field);

        if (field->visibility() == Public) {
          ParameterDefn * param = new ParameterDefn(module_, field->name());
          param->setLocation(SourceLocation());
          param->setType(field->type());
          param->setInternalType(field->type());
          param->addTrait(Defn::Singular);
          param->passes().finish(VariableDefn::VariableTypePass);

          if (initValue != NULL && initValue->isConstant()) {
            param->setInitValue(initValue);
            optionalParams.push_back(param);
          } else {
            requiredParams.push_back(param);
          }

          initValue = LValueExpr::get(SourceLocation(), NULL, param);
        }

        if (initValue != NULL) {
          LValueExpr * memberExpr = LValueExpr::get(SourceLocation(), selfExpr, field);
          Expr * initExpr = new AssignmentExpr(SourceLocation(), memberExpr, initValue);
          constructorBody->appendArg(initExpr);
        }
      }
    }
  }

  // Optional params go after required params.
  ParameterList params(requiredParams);
  params.append(optionalParams.begin(), optionalParams.end());
  createConstructorFunc(selfParam, params, constructorBody);
  return true;
}

bool ClassAnalyzer::createNoArgConstructor() {
  if (trace_) {
    diag.debug() << "No-arg constructor";
  }

  // Determine if the superclass has a default constructor. If it doesn't,
  // then we cannot make a no-arg constructor.
  CompositeType * type = targetType();
  SourceLocation loc(NULL, 0, 0);

  // If the default constructor took no args, then we don't need a separate no-arg
  // constructor.
  if (type->noArgConstructor() != NULL) {
    return true;
  }

  CompositeType * super = type->super();
  FunctionDefn * superCtor = NULL;
  if (super != NULL && super->defaultConstructor() == NULL) {
    diag.fatal(target) << "Cannot create a no-arg constructor for '" <<
        target << "' because super type '" << super <<
        "' has no default constructor";
    return false;
  }

  // List of parameters to the no-arg constructor
  ParameterDefn * selfParam = new ParameterDefn(module_, istrings.idSelf);
  selfParam->setType(type);
  selfParam->setInternalType(type);
  selfParam->addTrait(Defn::Singular);
  selfParam->setFlag(ParameterDefn::Reference, true);
  LValueExpr * selfExpr = LValueExpr::get(loc, NULL, selfParam);

  SeqExpr * constructorBody = new SeqExpr(SourceLocation(), &VoidType::instance);

  // TODO: Call the super ctor;
  DASSERT_OBJ(superCtor == NULL, target);

  for (Defn * de = type->firstMember(); de != NULL; de = de->nextInScope()) {
    if (de->storageClass() == Storage_Instance) {
      if (de->defnType() == Defn::Let || de->defnType() == Defn::Var) {
        VariableDefn * field = static_cast<VariableDefn *>(de);
        analyzeVariable(field, Task_PrepConstruction);
        Expr * initValue = getFieldInitVal(field);
        if (initValue != NULL) {
          LValueExpr * memberExpr = LValueExpr::get(loc, selfExpr, field);
          Expr * initExpr = new AssignmentExpr(loc, memberExpr, initValue);
          constructorBody->appendArg(initExpr);
        }
      }
    }
  }

  ParameterList params;
  createConstructorFunc(selfParam, params, constructorBody);
  return true;
}

Expr * ClassAnalyzer::getFieldInitVal(VariableDefn * var) {
  const Type * fieldType = var->type();
  if (var->initValue() != NULL) {
    return var->initValue();
  }

  if (fieldType->nullInitValue() != NULL) {
    return fieldType->nullInitValue();
  }

  if (fieldType->typeClass() == Type::NArray) {
    // TODO: If this array is non-zero size, we have a problem I think.
    // Native arrays must be initialized in the constructor.
    const NativeArrayType * nat = static_cast<const NativeArrayType *>(fieldType);
    if (nat->size() != 0) {
      diag.error(var) << "Native array types cannot be default initialized";
    }

    return NULL;
  }

  if (fieldType->typeClass() == Type::FlexibleArray) {
    return NULL;
  }

  if (fieldType->typeClass() == Type::Struct) {
    // See if the struct has a no-arg constructor.
    ExprAnalyzer ea(module_, activeScope_, var, NULL);
    Expr * fieldCtorCall = ea.callConstructor(
        var->location(), fieldType->typeDefn(), ASTNodeList());
    if (fieldCtorCall != NULL) {
      return ea.inferTypes(var, fieldCtorCall, NULL, false);
    } else {
      diag.error(var) << "'" << var << "' cannot be default initialized";
      return NULL;
    }
  }

  if (const UnionType * utype = dyn_cast<UnionType>(fieldType)) {
    if (utype->isSingleOptionalType()) {
      const Type * nonNullType = utype->getFirstNonVoidType();
      return nonNullType->nullInitValue();
    }
  }

  // TODO: Write tests for this case (private instance variables
  // being initialized to default values.)
  diag.fatal(var) << "Unimplemented default initialization: " << var;
  DFAIL("Implement");
}

FunctionDefn * ClassAnalyzer::createConstructorFunc(ParameterDefn * selfParam,
    ParameterList & params, Expr * constructorBody) {

  FunctionType * funcType = new FunctionType(&VoidType::instance, params);
  funcType->setSelfParam(selfParam);

  FunctionDefn * constructorDef = new FunctionDefn(Defn::Function, module_, istrings.idConstruct);
  constructorDef->setFunctionType(funcType);
  constructorDef->setLocation(target->location());
  constructorDef->setStorageClass(Storage_Instance);
  constructorDef->setVisibility(Public);
  constructorDef->setFlag(FunctionDefn::Ctor);
  constructorDef->addTrait(Defn::Synthetic);
  constructorDef->setBody(constructorBody);
  constructorDef->passes().finished().addAll(
      FunctionDefn::PassSet::of(
          FunctionDefn::AttributePass,
          FunctionDefn::ControlFlowPass,
          FunctionDefn::ParameterTypePass,
          FunctionDefn::ReturnTypePass));

  //constructorDef->setBody(constructorBody);
  if (target->isSingular()) {
    constructorDef->addTrait(Defn::Singular);

    // If it's synthetic, then don't add the constructor unless someone
    // actually calls it.
    if (!target->isSynthetic()) {
      module_->addSymbol(constructorDef);
    }

    DASSERT_OBJ(constructorDef->isSingular(), constructorDef);
    if (!funcType->isSingular()) {
      diag.fatal(target) << "Default constructor type " << funcType << " is not singular";
      funcType->whyNotSingular();
    }
  }

  targetType()->addMember(constructorDef);
  constructorDef->createQualifiedName(target);
  return constructorDef;
}

bool ClassAnalyzer::analyzeFieldTypesRecursive() {
  CompositeType * type = targetType();
  if (type->passes().begin(CompositeType::RecursiveFieldTypePass, true)) {
    if (trace_) {
      diag.debug() << "Field types";
    }
    if (type->super() != NULL) {
      analyzeType(type->super(), Task_PrepTypeGeneration);
    }

    for (DefnList::iterator it = type->instanceFields_.begin(); it != type->instanceFields_.end();
        ++it) {
      VariableDefn * var = dyn_cast_or_null<VariableDefn>(*it);
      if (var != NULL) {
        analyzeVariable(var, Task_PrepTypeGeneration);
        analyzeType(var->type(), Task_PrepTypeGeneration);
      }
    }

    type->passes().finish(CompositeType::RecursiveFieldTypePass);
  }

  return true;
}

bool ClassAnalyzer::analyzeCompletely() {
  // In this case, it's OK if it's already running. All we care about is that it eventually
  // completes, not that it completes right now.
  CompositeType * type = targetType();
  if (type->passes().begin(CompositeType::CompletionPass, true)) {
    if (!type->isSingular()) {
      type->passes().finish(CompositeType::CompletionPass);
      return true;
    }

    if (trace_) {
      diag.debug() << "Analyze completely";
    }
    CompositeType * super = type->super();
    if (super != NULL) {
      analyzeType(super, Task_PrepCodeGeneration);
    }

    for (Defn * member = type->firstMember(); member != NULL; member = member->nextInScope()) {
      AnalyzerBase::analyzeCompletely(member);
    }

    // Make sure to add all ancestor types as a dependency, even if not explicitly referenced.
    ClassSet ancestors;
    type->ancestorClasses(ancestors);
    for (ClassSet::const_iterator it = ancestors.begin(); it != ancestors.end(); ++it) {
      const CompositeType * base = *it;
      module_->addModuleDependency(base->typeDefn());
    }

    type->passes().finish(CompositeType::CompletionPass);
  }

  return true;
}

} // namespace tart
