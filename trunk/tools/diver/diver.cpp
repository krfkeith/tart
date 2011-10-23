/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

// 'diver' is short for Debug Info VERifier

#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/DebugInfo.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/IRReader.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"

using namespace llvm;

static cl::list<std::string> optInputFilenames(cl::Positional, cl::OneOrMore,
    cl::desc("<input bitcode files>"));

#if 0
class Scope {
public:
  static Scope * get(MDNode * node) {
    Scope *& scopePtr = scopes_[node];
    if (scopePtr == NULL) {
      scopePtr = new Scope(node);
    }
    return scopePtr;
  }

  MDNode * node() const { return scope_; }
  DIScope di() const { return scope_; }

  typedef SmallVector<MDNode *, 8> MemberList;
  typedef MemberList::iterator iterator;
  typedef MemberList::const_iterator const_iterator;

  MemberList members() { return members_; }
  const MemberList members() const { return members_; }

  void addMember(MDNode * member) {
    members_.push_back(member);
  }

  size_t size() const { return members_.size(); }
  bool empty() const { return members_.empty(); }

  iterator begin() { return members_.begin(); }
  iterator end() { return members_.end(); }
  const_iterator begin() const { return members_.begin(); }
  const_iterator end() const { return members_.end(); }

private:
  Scope(MDNode * node) : scope_(node) {}

  DIScope scope_;
  MemberList members_;

  typedef DenseMap<MDNode *, Scope *> ScopeMap;
  static ScopeMap scopes_;
};

Scope::ScopeMap Scope::scopes_;
#endif

class DIVerifier {
public:
  DIVerifier(Module * module) : module_(module) {}
  void run() {
    diFinder_.processModule(*module_);

    validateCompileUnits();
    validateGlobalVariables();
    validateSubprograms();
    validateTypes();
  }

  void validateCompileUnits() {
    if (diFinder_.compile_unit_count() > 0) {
      outs() << diFinder_.compile_unit_count() << " compile units.\n";
    }

    for (DebugInfoFinder::iterator it = diFinder_.compile_unit_begin(),
        itEnd = diFinder_.compile_unit_end(); it != itEnd; ++it) {
      validateCompileUnit(DICompileUnit(*it));
    }
  }

  void validateCompileUnit(DICompileUnit cu) {
    switch (cu.getTag()) {
      case dwarf::DW_TAG_compile_unit: {
        break;
      }

      default:
        errs() << "Unrecognized tag: " << dwarf::TagString(cu.getTag());
        cu.dump();
        exit(-1);
        break;
    }

    if (!cu.Verify()) {
      errs() << "Compile unit failed to verify: ";
      cu.dump();
    }
    printCompileUnit(outs(), cu, 0);
    //!{i32 720913, i32 0, i32 2, metadata !"StreamDecoder.tart", metadata !"/Users/talin/Projects/tart/trunk/lib/std/tart/reflect", metadata !"0.1 tartc", i1 true, i1 false, metadata !"", i32 1, metadata <badref>, metadata <badref>, metadata <badref>, metadata <badref>} ; [ DW_TAG_compile_unit ]
  }

  void printCompileUnit(raw_ostream & out, DICompileUnit cu, int indent) {
    out.indent(indent) << "CompileUnit: " << cu.getFilename() << "\n";
    indent += 2;
    out.indent(indent) << "Directory: " << cu.getDirectory() << "\n";
    out.indent(indent) << "LanguageID: " << cu.getLanguage() << "\n";
    out.indent(indent) << "Producer: " << cu.getProducer() << "\n";
    if (cu.isMain()) {
      out.indent(indent) << "IsMain: true\n";
    }
    if (cu.isOptimized()) {
      out.indent(indent) << "IsOptimized: true\n";
    }
    if (cu.getEnumTypes().getNumElements() > 0) {
      out.indent(indent) << "#EnumTypes: " << cu.getEnumTypes().getNumElements() << "\n";
    }
    if (cu.getRetainedTypes().getNumElements() > 0) {
      out.indent(indent) << "#RetainedTypes: " << cu.getRetainedTypes().getNumElements() << "\n";
    }
    if (cu.getSubprograms().getNumElements() > 0) {
      out.indent(indent) << "#Subprograms: " << cu.getSubprograms().getNumElements() << "\n";
    }
    if (cu.getGlobalVariables().getNumElements() > 0) {
      out.indent(indent) << "#GlobalVariables: " <<
          cu.getGlobalVariables().getNumElements() << "\n";
    }
  }

  void validateGlobalVariables() {
    if (diFinder_.global_variable_count() > 0) {
      outs() << diFinder_.global_variable_count() << " global variables.\n";
    }

    for (DebugInfoFinder::iterator it = diFinder_.global_variable_begin(),
        itEnd = diFinder_.global_variable_end(); it != itEnd; ++it) {
      validateGlobalVariable(DIGlobalVariable(*it));
    }
  }

  void validateGlobalVariable(DIGlobalVariable gv) {
    validateCompileUnit(gv, gv.getCompileUnit());
    switch (gv.getTag()) {
      case dwarf::DW_TAG_variable:
        break;

      default:
        errs() << "Unrecognized global variable tag: " << dwarf::TagString(gv.getTag());
        gv.dump();
        exit(-1);
        break;
    }

    if (!gv.Verify()) {
      errs() << "Global variable failed to verify: ";
      gv.dump();
    }
  }

  void validateSubprograms() {
    if (diFinder_.subprogram_count() > 0) {
      outs() << diFinder_.subprogram_count() << " subprograms.\n";
    }

    for (DebugInfoFinder::iterator it = diFinder_.subprogram_begin(),
        itEnd = diFinder_.subprogram_end(); it != itEnd; ++it) {
      validateSubprogram(DISubprogram(*it));
    }
  }

  void validateSubprogram(DISubprogram sp) {
    validateCompileUnit(sp, sp.getCompileUnit());
    switch (sp.getTag()) {
      case dwarf::DW_TAG_subprogram: {
        break;
      }

      default:
        errs() << "Unrecognized tag: " << dwarf::TagString(sp.getTag());
        sp.dump();
        exit(-1);
        break;
    }

    if (!sp.Verify()) {
      errs() << "Subprogram failed to verify: ";
      sp.dump();
    }

    if (!sp.getType().isCompositeType()) {
      errs() << "Subprogram " << sp.getName() << " does not have subprogram type.\n";
      sp.dump();
      exit(-1);
    }

    if (DICompositeType(sp.getType()).getTypeArray().getNumElements() == 0) {
      errs() << "Subprogram " << sp.getName() << " has no return type.\n";
      sp.dump();
      exit(-1);
    }

    Function * fn = sp.getFunction();
    if (fn == NULL && sp.isDefinition()) {
      errs() << "Subprogram " << sp.getName() << " has no associated function.\n";
      printSubprogram(errs(), sp, 0);
      exit(-1);
    }

    if (fn != NULL) {
      errs() << "Subprogram " << sp.getName() << " has one.\n";
      printSubprogram(errs(), sp, 0);
      exit(-1);
    }
  }

  void printSubprogram(raw_ostream & out, DISubprogram sp, int indent) {
    out.indent(indent) << "Subprogram: " << sp.getName() << "\n";
    indent += 2;
    out.indent(indent) << "Type: ";
    formatSubroutineType(out, sp.getType());
    out << "\n";
    out.indent(indent) << "IsDefinition: " << (sp.isDefinition() ? "true" : "false") << "\n";
  }

  void validateTypes() {
    if (diFinder_.type_count() > 0) {
      outs() << diFinder_.type_count() << " types.\n";
    }

    for (DebugInfoFinder::iterator it = diFinder_.type_begin(),
        itEnd = diFinder_.type_end(); it != itEnd; ++it) {
      validateType(DIType(*it));
    }
  }

  void validateType(DIType ty) {
    switch (ty.getTag()) {
      case dwarf::DW_TAG_subroutine_type: {
        validateFile(ty, ty.getFile());
        validateCompileUnit(ty, ty.getCompileUnit());
        validateTypeScope(ty.getContext());
        break;
      }

      case dwarf::DW_TAG_base_type: {
        //validateFile(ty, ty.getFile());
        //validateCompileUnit(ty, ty.getCompileUnit());
        validateTypeScope(ty.getContext());
        break;
      }

      case dwarf::DW_TAG_enumeration_type: {
        validateFile(ty, ty.getFile());
        validateCompileUnit(ty, ty.getCompileUnit());
        validateTypeScope(ty.getContext());
        break;
      }

      case dwarf::DW_TAG_pointer_type: {
        validateTypeScope(ty.getContext());
        break;
      }

      case dwarf::DW_TAG_reference_type: {
        //validateFile(ty, ty.getFile());
        //validateCompileUnit(ty, ty.getCompileUnit());
        validateTypeScope(ty.getContext());
        break;
      }

      case dwarf::DW_TAG_array_type: {
        //validateFile(ty, ty.getFile());
        //validateCompileUnit(ty, ty.getCompileUnit());
        validateTypeScope(ty.getContext());
        break;
      }

      case dwarf::DW_TAG_class_type: {
        validateFile(ty, ty.getFile());
        validateCompileUnit(ty, ty.getCompileUnit());
        validateTypeScope(ty.getContext());
        ((MDNode *)ty)->dump();
        break;
      }

      case dwarf::DW_TAG_structure_type: {
        validateFile(ty, ty.getFile());
        validateCompileUnit(ty, ty.getCompileUnit());
        validateTypeScope(ty.getContext());
        break;
      }

      case dwarf::DW_TAG_inheritance: {
        validateFile(ty, ty.getFile());
        validateCompileUnit(ty, ty.getCompileUnit());
        validateMemberScope(ty.getContext());
        //Scope::get(ty.getContext())->addMember(ty);
        return;
        //break;
      }

      case dwarf::DW_TAG_member: {
        validateFile(ty, ty.getFile());
        validateCompileUnit(ty, ty.getCompileUnit());
        validateMemberScope(ty.getContext());
        //Scope::get(ty.getContext())->addMember(ty);
        break;
      }

      case dwarf::DW_TAG_union_type: {
        validateFile(ty, ty.getFile());
        validateCompileUnit(ty, ty.getCompileUnit());
        validateTypeScope(ty.getContext());
        break;
      }

      case dwarf::DW_TAG_const_type: {
        //validateFile(ty, ty.getFile());
        //validateCompileUnit(ty, ty.getCompileUnit());
        validateTypeScope(ty.getContext());
        break;
      }

      case dwarf::DW_TAG_typedef: {
        validateFile(ty, ty.getFile());
        validateCompileUnit(ty, ty.getCompileUnit());
        validateTypeScope(ty.getContext());
        break;
      }

      case dwarf::DW_TAG_friend: {
        validateFile(ty, ty.getFile());
        validateCompileUnit(ty, ty.getCompileUnit());
        break;
      }

      default:
        errs() << "Unrecognized tag: " << dwarf::TagString(ty.getTag());
        ty.dump();
        exit(-1);
        break;
    }

    //DIScope getContext() const          { return getFieldAs<DIScope>(1); }
    //StringRef getName() const           { return getStringField(2);     }
    //DICompileUnit getCompileUnit() const{
    //DIFile getFile() const              { return getFieldAs<DIFile>(3); }

    if (!ty.Verify()) {
      errs() << "Type failed to verify: ";
      ty.dump();
      exit(-1);
    }
  }

  void formatTypeExpression(raw_ostream & out, DIType ty) {
    switch (ty.getTag()) {
      case 0: {
        out << "<unknown>";
        break;
      }

      case dwarf::DW_TAG_subroutine_type: {
        DIDerivedType derived(ty);
        break;
      }

      case dwarf::DW_TAG_base_type: {
        DIBasicType basic(ty);
        switch (basic.getEncoding()) {
          case dwarf::DW_ATE_boolean: out << "bool"; break;
          case dwarf::DW_ATE_unsigned_char: out << "char"; break;
          case dwarf::DW_ATE_float:
            if (basic.getSizeInBits() == 32) {
              out << "float";
            } else {
              out << "double";
            }
            break;

          case dwarf::DW_ATE_signed:
            out << "int" << basic.getSizeInBits() << "_t";
            break;

          case dwarf::DW_ATE_unsigned:
            out << "uint" << basic.getSizeInBits() << "_t";
            break;

          case dwarf::DW_ATE_address:
            out << "void*";
            break;

          default:
            errs() << "Unknown basic type encoding: " << basic.getEncoding();
            ty.dump();
            exit(-1);
        }
        break;
      }

      case dwarf::DW_TAG_enumeration_type: {
        out << "enum " << ty.getName();
        break;
      }

      case dwarf::DW_TAG_pointer_type: {
        DIDerivedType derived(ty);
        formatTypeExpression(out, derived.getTypeDerivedFrom());
        out << "*";
        break;
      }

      case dwarf::DW_TAG_reference_type: {
        DIDerivedType derived(ty);
        formatTypeExpression(out, derived.getTypeDerivedFrom());
        out << "&";
        break;
      }

      case dwarf::DW_TAG_array_type: {
        DIDerivedType derived(ty);
        formatTypeExpression(out, derived.getTypeDerivedFrom());
        out << "[]";
        break;
      }

      case dwarf::DW_TAG_class_type: {
        out << "class " << ty.getName();
        break;
      }

      case dwarf::DW_TAG_structure_type: {
        out << "struct " << ty.getName();
        break;
      }

      case dwarf::DW_TAG_union_type: {
        break;
      }

      case dwarf::DW_TAG_const_type: {
        DIDerivedType derived(ty);
        out << "const ";
        formatTypeExpression(out, derived.getTypeDerivedFrom());
        break;
      }

      case dwarf::DW_TAG_typedef: {
        out << "typedef " << ty.getName();
        break;
      }

      case dwarf::DW_TAG_friend: {
        break;
      }

      default:
        errs() << "Unrecognized tag: " << dwarf::TagString(ty.getTag());
        ty.dump();
        exit(-1);
        break;
    }
  }

  void formatSubroutineType(raw_ostream & out, DICompositeType ty) {
    DIArray params(ty.getTypeArray());
    formatTypeExpression(out, DIType(params.getElement(0)));
    out << " (";
    for (unsigned i = 1; i < params.getNumElements(); ++i) {
      if (i != 1) {
        out << ", ";
      }
      formatTypeExpression(out, DIType(params.getElement(1)));
    }
    out << ")";
  }

  void validateTypeScope(DIScope scope) {
    switch (scope.getTag()) {
      case dwarf::DW_TAG_compile_unit: {
        break;
      }

      case dwarf::DW_TAG_file_type: {
        break;
      }

      case dwarf::DW_TAG_namespace: {
        break;
      }

      case dwarf::DW_TAG_class_type: {
        break;
      }

      default:
        errs() << "Unrecognized type scope: " << dwarf::TagString(scope.getTag());
        scope.dump();
        exit(-1);
        break;
    }
  }

  void validateMemberScope(DIScope scope) {
    switch (scope.getTag()) {
      case dwarf::DW_TAG_file_type: {
        break;
      }

      case dwarf::DW_TAG_class_type: {
        break;
      }

      default:
        errs() << "Unrecognized member scope: " << dwarf::TagString(scope.getTag());
        scope.dump();
        exit(-1);
        break;
    }
  }

  void validateFile(DIDescriptor owner, DIFile file) {
    if (!file.Verify()) {
      errs() << "File for ";
      owner.print(errs());
      errs() << " failed to verify: '" << file.getFilename() << "'\n";
      exit(-1);
    }
  }

  void validateCompileUnit(DIDescriptor target, DICompileUnit cu) {
    if (!cu.Verify()) {
      errs() << "Compile unit for ";
      target.print(errs());
      errs() << " failed to verify.'\n";
      exit(-1);
    }
  }

private:
  Module * module_;
  DebugInfoFinder diFinder_;
};

int main(int argc, char **argv, char **envp) {
  // Print a stack trace if we signal out.
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);

  LLVMContext & context = getGlobalContext();
  llvm_shutdown_obj Y; // Call llvm_shutdown() on exit.

  // Parse the command line options
  cl::ParseCommandLineOptions(argc, argv, "diver");

  typedef std::vector<sys::Path> Paths;
  Paths filePaths;
  for (unsigned i = 0; i < optInputFilenames.size(); ++i) {
    filePaths.push_back(sys::Path(optInputFilenames[i]));
  }

  // Sort the paths
  std::sort(filePaths.begin(), filePaths.end());

  SMDiagnostic smErr;
  std::auto_ptr<Module> module;
  for (Paths::iterator path = filePaths.begin(); path != filePaths.end(); ++path) {
    outs() << "Validating module: " << path->str() << "\n";
    module.reset(ParseIRFile(path->str(), smErr, context));
    if (module.get() != NULL) {
      DIVerifier(module.get()).run();
    }
  }
  return 0;
}
