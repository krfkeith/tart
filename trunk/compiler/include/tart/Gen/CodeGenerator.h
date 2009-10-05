/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GEN_CODEGENERATOR_H
#define TART_GEN_CODEGENERATOR_H

#include <tart/Common/SourceLocation.h>

#include <llvm/Support/IRBuilder.h>
#include <llvm/PassManager.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringMap.h>
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
class IndirectCallExpr;
class ArrayLiteralExpr;
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
class AddressType;
class NativePointerType;
class NativeArrayType;
class UnionType;
class Block;
class LocalScope;
class FormatStream;
class RuntimeTypeInfo;
class ConstantString;
class ConstantObjectRef;
class ConstantNativeArray;
class ProgramSource;
class TypeRef;

typedef llvm::SmallVector<Expr *, 4> ExprList;
typedef llvm::SmallVector<FunctionDefn *, 32> MethodList;
typedef llvm::SmallVector<llvm::Value *, 16> ValueList;
typedef std::vector<llvm::Constant *> ConstantList;
typedef llvm::DenseMap<const CompositeType *, RuntimeTypeInfo *> RTTypeMap;
typedef llvm::DenseMap<const ConstantObjectRef *, llvm::Constant *> ConstantObjectMap;
typedef llvm::DenseMap<const Type *, llvm::DIType> DITypeMap;
typedef llvm::StringMap<llvm::Constant *> StringLiteralMap;
typedef llvm::SmallVector<Block *, 16> BlockList;
typedef llvm::SmallVector<LocalScope *, 4> LocalScopeList;

/// -------------------------------------------------------------------
/// Reflected Member

/// -------------------------------------------------------------------
/// Code generator class.
class CodeGenerator {
public:
  CodeGenerator(Module * mod);

  /** Return the builder object. */
  llvm::IRBuilder<true> & builder() { return builder_; }

  /** Function to generate the module code. */
  void generate();

  llvm::LLVMContext & context() const { return context_; }

  /** Generate a global definition. */
  bool genXDef(Defn * de);

  // Methods to generate a reference to a definition

  llvm::Function * genFunctionValue(FunctionDefn * fn);
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
  void genLocalStorage(BlockList & blocks, LocalScopeList & lsl);
  void genLocalVar(VariableDefn * var);

  /** Generate the function body from the basic block list. */
  void genBlocks(BlockList & blocks);
  void genStmt(Expr * in);
  void genBlockTerminator(Block * blk);
  void genReturn(Expr * returnVal);
  void genLocalReturn(Block * blk);
  bool genTestExpr(const Expr * test, llvm::BasicBlock * trueBlk, llvm::BasicBlock * falseBlk);
  void genThrow(Block * blk);
  void genCatch(Block * blk);
  void genSwitch(Block * blk);
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
  llvm::Value * genUnionMemberCast(CastExpr * in);
  llvm::Value * genAssignment(AssignmentExpr * in);
  llvm::Value * genInstanceOf(InstanceOfExpr * in);
  llvm::Value * genRefEq(const BinaryExpr * in, bool invert);
  llvm::Value * genPtrDeref(const UnaryExpr * in);
  llvm::Value * genNot(const UnaryExpr * in);
  llvm::Value * genLogicalOper(const BinaryExpr * in);
  llvm::Value * genCall(FnCallExpr * in);
  llvm::Value * genIndirectCall(IndirectCallExpr * in);
  llvm::Value * genNew(NewExpr * in);

  /** Load an expression */
  llvm::Value * genLoadLValue(const LValueExpr * lval);

  /** Get the address of a value. */
  llvm::Value * genLValueAddress(const Expr * in);

  /** Generate the address of a member field. */
  llvm::Value * genMemberFieldAddr(const LValueExpr * lval);

  /** Generate the address of an array element. */
  llvm::Value * genElementAddr(const UnaryExpr * in);

  /** Generate the code to look up a method in a vtable. */
  llvm::Value * genVTableLookup(const FunctionDefn * method, const CompositeType * classType,
      llvm::Value * objectVal);

  /** Generate the code to look up a method in an itable. */
  llvm::Value * genITableLookup(const FunctionDefn * method, const CompositeType * interfaceType,
      llvm::Value * objectVal);

#if 0
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
  llvm::Value * genCallInstr(llvm::Value * fn,
      ValueList::iterator firstArg, ValueList::iterator lastArg, const char * name);

  /** Generate an upcast instruction. */
  llvm::Value * genUpCastInstr(llvm::Value * val, const Type * fromType, const Type * toType);

  /** Generate an 'isInstanceOf' test for composite types. */
  llvm::Value * genCompositeTypeTest(llvm::Value * val, CompositeType * fromType, CompositeType * toType);

  /** Generate an 'isInstance' test for union types. */
  llvm::Value * genUnionTypeTest(llvm::Value * val, UnionType * fromType, Type * toType);

  /** Generate a reference to the Type descriptor for this type. */
  llvm::GlobalVariable * getTypeDescriptorPtr(const Type * ctype);
  llvm::GlobalVariable * createTypeDescriptorPtr(RuntimeTypeInfo * rtype);
  void createTypeDescriptor(RuntimeTypeInfo * rtype);

  /** Generate a reference to the TypeInfoBlock for this type. */
  llvm::Constant * getTypeInfoBlockPtr(const CompositeType * ctype);
  llvm::Constant * createTypeInfoBlockPtr(RuntimeTypeInfo * rtype);
  bool createTypeInfoBlock(RuntimeTypeInfo * rtype);

  /** Generate a reference to the allocator function for this type. */
  llvm::Function * getTypeAllocator(const CompositeType * tdef);
  llvm::Function * createTypeAllocator(RuntimeTypeInfo * tdef);

  /** Generate the method dispatch table for a type. */
  llvm::Constant * genMethodArray(const MethodList & methods);

  /** Generate the interface dispatcher function. */
  llvm::Function * genInterfaceDispatchFunc(const CompositeType * ctype);

  /** Generate the code to initialize the vtable pointer of a newly-allocated class instance. */
  void genInitObjVTable(const CompositeType * tdef, llvm::Value * instance);

  /** Generate an array containing reflection data supplied by the specified array. */
  llvm::Constant * genReflectionDataArray(
      const std::string & baseName, const VariableDefn * var, const ConstantList & values);

  /** Given a type, generate a constant representing the size of that type.
      'memberSize' - return how much space the type would consume as a member of another
      type (which will be equal to the size of a reference for reference types.)
   */
  llvm::Constant * genSizeOf(Type * type, bool memberSize);

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
  llvm::Constant * genStringLiteral(const std::string & strval);

  /** Generate an array literal. */
  llvm::Value * genArrayLiteral(const ArrayLiteralExpr * in);

  /** Generate code to allocate an object, where the object size is not a compile-time constant. */
  llvm::Value * genVarSizeAlloc(const SourceLocation & loc, const Type * objType,
      const Expr * sizeExpr);

  /** Generate a constant object. */
  llvm::GlobalVariable * genConstantObjectPtr(const ConstantObjectRef * obj);
  llvm::Constant * genConstantObject(const ConstantObjectRef * obj);

  /** Generate a structure from the fields of a constant object. */
  llvm::Constant * genConstantObjectStruct(
      const ConstantObjectRef * obj, const CompositeType * type);

  /** Generate a constant array. */
  llvm::Constant * genConstantArray(const ConstantNativeArray * array);

  /** Return the IR module being compiled. */
  llvm::Module * irModule() const { return irModule_; }

  /** Return a 32-bit constant integer with the specified value. */
  llvm::ConstantInt * getInt32Val(int value);

  /** Return a 64-bit constant integer with the specified value. */
  llvm::ConstantInt * getInt64Val(int64_t value);

  /** Reflection methods. */
  llvm::GlobalVariable * createModuleObjectPtr();
  void createModuleObject();
  void createModuleTable();
  void createModuleCount();
  llvm::Constant * reflectMethod(const FunctionDefn * func);
  llvm::Constant * reflectMember(const CompositeType * structType, const ValueDefn * def);

    /** Return the debug compile unit for the specified source file. */
  llvm::DICompileUnit getCompileUnit(const ProgramSource * source);
  llvm::DICompileUnit getCompileUnit(Defn * defn);
  unsigned getSourceLineNumber(const SourceLocation & loc);
  void genStopPoint(const SourceLocation & loc);

  /** Generate debugging information for types. */
  llvm::DIType genDIType(const TypeRef & ref);
  llvm::DIType genDIType(const Type * type);
  llvm::DIBasicType genDIPrimitiveType(const PrimitiveType * type);
  llvm::DICompositeType genDICompositeType(const CompositeType * type);
  llvm::DIType genDIEnumType(const EnumType * type);
  llvm::DICompositeType genDINativeArrayType(const NativeArrayType * type);
  llvm::DIDerivedType genDIAddressType(const AddressType * type);
  llvm::DIDerivedType genDINativePointerType(const NativePointerType * type);
  llvm::DICompositeType genDIUnionType(const UnionType * type);
  llvm::DICompositeType genDIFunctionType(const FunctionType * type);
  llvm::DIDerivedType genDITypeBase(const CompositeType * type);
  llvm::DIDerivedType genDITypeMember(const VariableDefn * var, llvm::Constant * offset);
  llvm::DIType genDIEmbeddedType(const Type * type);
  llvm::DIType genDIEmbeddedType(const TypeRef & type);
  llvm::DIType genDIParameterType(const TypeRef & type);

private:
  typedef llvm::DenseMap<const ProgramSource *, llvm::DICompileUnit> CompileUnitMap;

  /** Find a static method of the given class, and also generate an external reference
      to it from this module. If it's a template, then also instantiate it. This is used
      to call various methods of core classes. */
  llvm::Function * findMethod(const CompositeType * type, const char * methodName);

  void verifyModule();
  void outputModule();

  void genModuleMetadata(std::ostream & strm);

  /** Return true if 'type' requires an implicit dereference, such as a struct. */
  bool requiresImplicitDereference(const Type * type);

  llvm::LLVMContext & context_;
  llvm::IRBuilder<true> builder_;    // LLVM builder

  Module * module_;
  llvm::Module * irModule_;
  llvm::Function * currentFn_;
  llvm::GlobalVariable * moduleObject_;
  llvm::GlobalVariable * moduleTable_;

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
  DITypeMap dbgTypeMap_;
  SourceLocation dbgLocation_;

  llvm::BasicBlock * unwindTarget_;
  llvm::Function * unwindRaiseException_;
  llvm::Function * unwindResume_;
  llvm::Function * exceptionPersonality_;

  RTTypeMap compositeTypeMap_;
  StringLiteralMap stringLiteralMap_;
  ConstantObjectMap constantObjectMap_;

  bool debug_;
  bool reflect_;
};

}

#endif
