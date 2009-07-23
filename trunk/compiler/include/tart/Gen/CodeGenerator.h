/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_GEN_CODEGENERATOR_H
#define TART_GEN_CODEGENERATOR_H

#include <llvm/Support/IRBuilder.h>
#include <llvm/Support/DebugInfoBuilder.h>
#include <llvm/PassManager.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/Analysis/DebugInfo.h>
#include <iostream>

namespace tart {

class Module;
class Expr;
class CastExpr;
class AssignmentExpr;
class BinaryOpcodeExpr;
class BinaryExpr;
class UnaryExpr;
class CompareExpr;
class InstanceOfExpr;
class InitVarExpr;
class FnCallExpr;
class NewExpr;
class LValueExpr;
class Defn;
class TypeDefn;
class ValueDefn;
class VariableDefn;
class Type;
class FunctionDefn;
class TypeDefn;
class CompositeType;
class EnumType;
class FunctionType;
class PrimitiveType;
class UnionType;
class Block;
class FormatStream;
class RuntimeTypeInfo;
class ConstantString;
class ProgramSource;
struct SourceLocation;

typedef llvm::SmallVector<FunctionDefn *, 32> MethodList;
typedef llvm::SmallVector<llvm::Value *, 16> ValueList;
typedef std::vector<llvm::Constant *> ConstantList;
typedef llvm::DenseMap<const CompositeType *, RuntimeTypeInfo *> RTTypeMap;
    
/// -------------------------------------------------------------------
/// Code generator class.
class CodeGenerator {
public:
  CodeGenerator(Module * mod);
  
  /** Return the builder object. */
  llvm::IRBuilder<true> & builder() { return builder_; }

#if 0

  /** Return the module being built. */
  llvm::Module * getIRModule() const { return irModule_; }
#endif
  
  /** Function to generate the module code. */
  void generate();

  /** Generate a global definition. */
  bool genXDef(Defn * de);

  // Methods to generate a reference to a definition

  llvm::Function * genFunctionValue(FunctionDefn * func);
  llvm::Value * genLetValue(const VariableDefn * let);
  llvm::Value * genVarValue(const VariableDefn * var);
  llvm::Value * genGlobalVar(const VariableDefn * var);

  // Methods to generate the contents of a definition

  bool genFunction(FunctionDefn * fdef);
  bool genLetDefn(VariableDefn * let);

  /** Generate IR types. */
  const llvm::Type * genTypeDefn(TypeDefn * typeDef);
  const llvm::Type * genPrimitiveType(PrimitiveType * tdef);
  const llvm::Type * genCompositeType(const CompositeType * tdef);
  const llvm::Type * genEnumType(EnumType * tdef);

    /** Generate the code that allocates storage for locals on the stack. */
  void genLocalStorage();
  void genLocalVar(VariableDefn * var);

  /** Generate the function body from the basic block list. */
  void genBlocks();
  void genStmt(Expr * in);
  void genBlockTerminator(Block * blk);
  void genReturn(Expr * returnVal);
  void genLocalReturn(Block * blk);
  bool genTestExpr(Expr * test, llvm::BasicBlock * trueBlk,
      llvm::BasicBlock * falseBlk);
  void genThrow(Block * blk);
  void genCatch(Block * blk);
  void genDoFinally(Block * blk);

  /** Generate an expression (an RValue). */
  llvm::Value * genExpr(const Expr * expr);
  llvm::Constant * genConstExpr(const Expr * expr);
  llvm::Value * genInitVar(InitVarExpr * in);
  llvm::Value * genBinaryOpcode(BinaryOpcodeExpr * expr);
  llvm::Value * genCompare(CompareExpr * in);
  llvm::Value * genNumericCast(CastExpr * in);
  llvm::Value * genUpCast(CastExpr * in);
  llvm::Value * genBitCast(CastExpr * in);
  llvm::Value * genUnionCtorCast(CastExpr * in);
  llvm::Value * genAssignment(AssignmentExpr * in);
  llvm::Value * genInstanceOf(InstanceOfExpr * in);
  llvm::Value * genRefEq(const BinaryExpr * in, bool invert);
  llvm::Value * genPtrDeref(const UnaryExpr * in);
  llvm::Value * genNot(const UnaryExpr * in);
  llvm::Value * genCall(FnCallExpr * in);
  llvm::Value * genNew(NewExpr * in);

  /** Load an expression */
  llvm::Value * genLoadLValue(const LValueExpr * lval);

  /** Get the address of a value. */
  llvm::Value * genLValueAddress(const Expr * in);

  /** Generate the address of a member field. */
  llvm::Value * genMemberFieldAddr(const LValueExpr * lval);

  /** Generate the address of an array element. */
  llvm::Value * genElementAddr(const UnaryExpr * in);

#if 0
  /** Generate the code to look up a method in a vtable. */
  llvm::Value * genVTableLookup(const FunctionDef * method, const Type * classType,
      llvm::Value * objectVal);
  
  /** Generate the code to look up a method in an itable. */
  llvm::Value * genITableLookup(const FunctionDef * method, const Type * interfaceType,
      llvm::Value * objectVal);
  
  /** Generate an array literal. */
  llvm::Value * genArrayLiteral(OpExpr * array);
  
  /** Generate a type cast. */
  llvm::Value * genCast(const SourceLocation & loc, llvm::Value * val,
      const Type * fromType, const Type * toType);
  
  /** Generate the attributes to this declaration. */
  bool genAttrs(const Declaration * de, Attributes & declAttrs);
#endif

  /** Given an expression referring to an LValue, return the base address
      and the GetElementPtr indices needed to access the value. */
  llvm::Value * genGEPIndices(const Expr * expr, ValueList & indices, FormatStream & labelStream);

  /** Generate the base address of a struct or array. */
  llvm::Value * genBaseExpr(const Expr * base, ValueList & indices, FormatStream & labelStream);

  /** Generate a function call instruction - either a call or invoke, depending
      on whether there's an enclosing try block. */
  llvm::Value * genCallInstr(llvm::Value * func,
      ValueList::iterator firstArg, ValueList::iterator lastArg, const char * name);

  /** Generate an upcast instruction. */
  llvm::Value * genUpCastInstr(llvm::Value * val, const Type * fromType, const Type * toType);

  /** Generate an 'isInstanceOf' test for composite types. */
  llvm::Value * genCompositeTypeTest(llvm::Value * val, CompositeType * fromType, CompositeType * toType);

  /** Generate an 'isInstance' test for union types. */
  llvm::Value * genUnionTypeTest(llvm::Value * val, UnionType * fromType, Type * toType);

  /** Generate a reference to the Type object for this type. */
  llvm::GlobalVariable * getTypeObjectPtr(const CompositeType * ctype);

  /** Generate a reference to the TypeInfoBlock for this type. */
  llvm::Constant * getTypeInfoPtr(const CompositeType * ctype);

  /** Generate a reference to the allocator function for this type. */
  llvm::Function * getTypeAllocator(const CompositeType * tdef);

  /** Generate a reference to the Type object for this type. */
  llvm::GlobalVariable * createTypeObjectPtr(RuntimeTypeInfo * rtype);

  /** Generate a reference to the TypeInfoBlock for this type. */
  llvm::Constant * createTypeInfoPtr(RuntimeTypeInfo * rtype);

  /** Generate the contents of the Type object for this type. */
  bool createTypeObject(RuntimeTypeInfo * rtype);

  /** Generate the contents of the TypeInfoBlock for this type. */
  bool createTypeInfoBlock(RuntimeTypeInfo * rtype);

  /** Generate a reference to the allocator function for this type. */
  llvm::Function * createTypeAllocator(RuntimeTypeInfo * tdef);

  /** Generate the method dispatch table for a type. */
  llvm::Constant * genMethodArray(const MethodList & methods);

  /** Generate the interface dispatcher function. */
  llvm::Function * genInterfaceDispatchFunc(const CompositeType * ctype);

  /** Generate the code to initialize the vtable pointer of a newly-allocated class instance. */
  void genInitObjVTable(const CompositeType * tdef, llvm::Value * instance);

  /** Given a type, generate a constant representing the size of that type. */
  llvm::Constant * genSizeOf(Type * type);

  /** Generate the program entry point. */
  void genEntryPoint();

  /** Given a type definition, create or find the generated runtime type
      information for that type. */
  RuntimeTypeInfo * getRTTypeInfo(const CompositeType * ctype);

#if 0
  /** generate the module initialization function if it doesn't already exist. */
  void genModuleInitFunc();

  /** Write out the module metadata. */
  void genModuleMetadata(std::ostream & out);

  /** Write out all the metadata for children of a declaration. */
  void genChildMetadata(Declaration * parent, std::ostream & out, int nesting);
#endif

  /** return a reference to the low-level _Unwind_RaiseException intrinsic. */
  llvm::Function * getUnwindRaiseException();

  /** return a reference to the low-level _Unwind_Resume intrinsic. */
  llvm::Function * getUnwindResume();

  /** return a reference to the exception personality function. */
  llvm::Function * getExceptionPersonality();

  /** Generate data structures for a string literal. */
  llvm::Value * genStringLiteral(const std::string & strval);

  /** Generate code to allocate an object, where the object size is not a compile-time constant. */
  llvm::Value * genVarSizeAlloc(const SourceLocation & loc, const Type * objType,
      const Expr * sizeExpr);

  /** Return the IR module being compiled. */
  llvm::Module * irModule() const { return irModule_; }

private:
  typedef llvm::DenseMap<const ProgramSource *, llvm::DICompileUnit> CompileUnitMap;

  /** Return the debug compile unit for the specified source file. */
  llvm::DICompileUnit getCompileUnit(const ProgramSource * source);
  llvm::DICompileUnit getCompileUnit(Defn * defn);
  unsigned getSourceLineNumber(const SourceLocation & loc);

  llvm::IRBuilder<true> builder_;    // LLVM builder
  
  Module * module;
  FunctionDefn * entryPoint_;        // The 'main' function
  llvm::Module * irModule_;

  FunctionDefn * currentFunction_;

#if 0
  llvm::Function * moduleInitFunc;
  FunctionType * moduleInitFuncType;
  llvm::BasicBlock * moduleInitBlock;
#endif
  
  llvm::PointerType * methodPtrType;

  // Debug information
  CompileUnitMap dbgCompileUnits_;
  llvm::DIFactory dbgFactory_;
  llvm::DICompileUnit dbgCompileUnit_;
  llvm::DISubprogram dbgFunction_;

#if 0
  struct Attributes {
      std::vector<llvm::Constant *> irAttrs;
      const Expr * entryPointAttr;
  };
#endif

  llvm::BasicBlock * unwindTarget_;
  llvm::Function * unwindRaiseException_;
  llvm::Function * unwindResume_;
  llvm::Function * exceptionPersonality_;

  RTTypeMap compositeTypeMap_;

  bool debug;
};

}

#endif
