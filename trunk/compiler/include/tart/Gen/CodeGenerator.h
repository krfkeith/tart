/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GEN_CODEGENERATOR_H
#define TART_GEN_CODEGENERATOR_H

#include "tart/Common/SourceLocation.h"
#include "tart/Common/Formattable.h"
#include "tart/Meta/NameTable.h"
#include "tart/Gen/Reflector.h"

#include "llvm/Support/IRBuilder.h"
#include "llvm/PassManager.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Analysis/DebugInfo.h"
#include "llvm/Target/TargetData.h"
#include <iostream>

// If true, means that structs are passed as first-class values internally within
// a function; If false, it means they are passed as pointers.
#define FC_STRUCTS_INTERNAL 1

namespace tart {

class Module;
class Expr;
class CastExpr;
class AssignmentExpr;
class MultiAssignExpr;
class BinaryOpcodeExpr;
class BinaryExpr;
class UnaryExpr;
class CompareExpr;
class InstanceOfExpr;
class InitVarExpr;
class ClearVarExpr;
class FnCallExpr;
class IndirectCallExpr;
class ArrayLiteralExpr;
class ClosureEnvExpr;
class NewExpr;
class TupleCtorExpr;
class LValueExpr;
class BoundMethodExpr;
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
class NativeArrayType;
class FlexibleArrayType;
class UnionType;
class TupleType;
class BoundMethodType;
class Block;
class LocalScope;
class FormatStream;
class RuntimeTypeInfo;
class ConstantString;
class ConstantObjectRef;
class ConstantNativeArray;
class ProgramSource;

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
  // Field indices for TypeInfoBlock
  enum TIBFields {
    TIB_META = 0,
    TIB_TRACE_TABLE,
    TIB_BASES,
    TIB_IDISPATCH,
    TIB_METHOD_TABLE,
  };

  CodeGenerator(Module * mod);

  /** Return the builder object. */
  llvm::IRBuilder<true> & builder() { return builder_; }

  /** Return the object containing the module constants table. */
  NameTable & nameTable() { return nameTable_; }

  /** Function to generate the module code. */
  void generate();

  llvm::LLVMContext & context() const { return context_; }
  Module * module() const { return module_; }

  /** Generate a global definition. */
  bool genXDef(Defn * de);

  // Methods to generate a reference to a definition

  llvm::Function * genFunctionValue(const FunctionDefn * fn);
  llvm::Value * genLetValue(const VariableDefn * let);
  llvm::Value * genVarValue(const VariableDefn * var);
  llvm::Constant * genGlobalVar(const VariableDefn * var);

  // Methods to generate the contents of a definition

  bool genFunction(FunctionDefn * fdef);
  bool genLetDefn(VariableDefn * let);

  /** Generate IR types. */
  const llvm::Type * genTypeDefn(TypeDefn * typeDef);
  const llvm::Type * genPrimitiveType(PrimitiveType * tdef);
  const llvm::Type * genCompositeType(const CompositeType * tdef);
  const llvm::Type * genEnumType(EnumType * tdef);

    /** Generate the code that allocates storage for locals on the stack. */
  void genLocalStorage(LocalScopeList & lsl);
  void genLocalRoots(LocalScopeList & lsl);
  void genLocalVar(VariableDefn * var);
  void genGCRoot(llvm::Value * lValue, const Type * varType);
  void initGCRoot(llvm::Value * allocaValue);

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
  llvm::GlobalVariable * genConstRef(const Expr * in, llvm::StringRef name, bool synthetic);
  llvm::Value * genInitVar(const InitVarExpr * in);
  llvm::Value * genClearVar(const ClearVarExpr * in);
  llvm::Value * genBinaryOpcode(const BinaryOpcodeExpr * expr);
  llvm::Value * genCompare(const CompareExpr * in);
  llvm::Value * genCast(llvm::Value * in, const Type * fromType, const Type * toType);
  llvm::Value * genNumericCast(const CastExpr * in);
  llvm::Value * genUpCast(const CastExpr * in, bool saveRoots);
  llvm::Value * genDynamicCast(const CastExpr * in, bool throwOnFailure, bool saveRoots);
  llvm::Value * genBitCast(const CastExpr * in, bool saveRoots);
  llvm::Value * genUnionCtorCast(const CastExpr * in, bool saveRoots);
  llvm::Value * genUnionMemberCast(const CastExpr * in);
  llvm::Value * genTupleCtor(const TupleCtorExpr * in);
  llvm::Value * genAssignment(const AssignmentExpr * in);
  llvm::Value * genMultiAssign(const MultiAssignExpr * in);
  llvm::Value * genInstanceOf(const InstanceOfExpr * in);
  llvm::Value * genRefEq(const BinaryExpr * in, bool invert);
  llvm::Value * genPtrDeref(const UnaryExpr * in);
  llvm::Value * genNot(const UnaryExpr * in);
  llvm::Value * genComplement(const UnaryExpr * in);
  llvm::Value * genLogicalOper(const BinaryExpr * in);
  llvm::Value * genCall(const FnCallExpr * in);
  llvm::Value * genIndirectCall(const IndirectCallExpr * in);
  llvm::Value * genNew(const NewExpr * in);
  llvm::Value * genCompositeCast(llvm::Value * in, const CompositeType * fromCls,
      const CompositeType * toCls, bool throwOnFailure);

  /** Load an expression */
  llvm::Value * genLoadLValue(const LValueExpr * lval);

  /** Get the address of a value. */
  llvm::Value * genLValueAddress(const Expr * in);

  /** Load the value of a member field. */
  llvm::Value * genLoadMemberField(const LValueExpr * lval);

  /** Generate the address of a member field. */
  llvm::Value * genMemberFieldAddr(const LValueExpr * lval);

  /** Load the value of an array element. */
  llvm::Value * genLoadElement(const BinaryExpr * in);

  /** Generate the address of an array element. */
  llvm::Value * genElementAddr(const BinaryExpr * in);

#if 0
  /** Generate the attributes to this declaration. */
  bool genAttrs(const Declaration * de, Attributes & declAttrs);
#endif

  /** Given an expression referring to an LValue, return the base address
      and the GetElementPtr indices needed to access the value. */
  llvm::Value * genGEPIndices(const Expr * expr, ValueList & indices, FormatStream & labelStream);

  /** Generate the base address of a struct or array. */
  llvm::Value * genBaseAddress(const Expr * base, ValueList & indices, FormatStream & labelStream);

  /** Generate the code to look up a method in a vtable. */
  llvm::Value * genVTableLookup(const FunctionDefn * method, const CompositeType * classType,
      llvm::Value * objectVal);

  /** Generate the code to look up a method in an itable. */
  llvm::Value * genITableLookup(const FunctionDefn * method, const CompositeType * interfaceType,
      llvm::Value * objectVal);

  /** Generate a function call instruction - either a call or invoke, depending
      on whether there's an enclosing try block. */
  llvm::Value * genCallInstr(llvm::Value * fn,
      ValueList::iterator firstArg, ValueList::iterator lastArg, const char * name);

  /** Get the address of a value. */
  llvm::Value * genBoundMethod(const BoundMethodExpr * in);

  /** Generate an upcast instruction. */
  llvm::Value * genUpCastInstr(llvm::Value * val, const Type * fromType, const Type * toType);

  /** Generate an 'isInstanceOf' test for composite types. */
  llvm::Value * genCompositeTypeTest(llvm::Value * val, const CompositeType * fromType,
      const CompositeType * toType);

  /** Generate an 'isInstance' test for union types. */
  llvm::Value * genUnionTypeTest(llvm::Value * val, const UnionType * fromType,
      const Type * toType, bool isLValValue);

  /** Generate a reference to the TypeInfoBlock for this type. */
  llvm::Constant * getTypeInfoBlockPtr(const CompositeType * ctype);
  llvm::Constant * createTypeInfoBlockPtr(RuntimeTypeInfo * rtype);
  bool createTypeInfoBlock(RuntimeTypeInfo * rtype);
  bool createTemplateTypeInfoBlock(const CompositeType * type);

  /** Generate a reference to the allocator function for this type. */
  llvm::Function * getTypeAllocator(const CompositeType * tdef);
  llvm::Function * createTypeAllocator(RuntimeTypeInfo * tdef);

  /** Generate the method dispatch table for a type. */
  llvm::Constant * genMethodArray(const MethodList & methods);

  /** Generate the interface dispatcher function. */
  llvm::Function * genInterfaceDispatchFunc(const CompositeType * ctype);

  /** Generate the code to initialize the vtable pointer of a newly-allocated class instance. */
  void genInitObjVTable(const CompositeType * tdef, llvm::Value * instance);

  /** Generate the table of pointer offsets for this type. */
  llvm::GlobalVariable * getTraceTable(const Type * type);
  llvm::GlobalVariable * createTraceTable(const Type * type);
  void createTraceTableEntries(const Type * type, llvm::Constant * basePtr,
      ConstantList & traceTable, ConstantList & fieldOffsets, ConstantList & indices);
  void createCompositeTraceTableEntries(const CompositeType * type, llvm::Constant * basePtr,
      ConstantList & traceTable, ConstantList & fieldOffsets, ConstantList & indices);
  llvm::Function * getUnionTraceMethod(const UnionType * utype);

  /** Generate a descriptor block for an enumerated type. */
  llvm::GlobalVariable * getEnumInfoBlock(const EnumType * etype);

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

  /** generate the module initialization function if it doesn't already exist. */
  void genModuleInitFunc();

  /** return a reference to the low-level _Unwind_RaiseException intrinsic. */
  llvm::Function * getUnwindRaiseException();

  /** return a reference to the low-level _Unwind_Resume intrinsic. */
  llvm::Function * getUnwindResume();

  /** return a reference to the exception personality function. */
  llvm::Function * getExceptionPersonality();

  /** return a reference to the exception personality function that handles stack back trace. */
  llvm::Function * getExceptionTracePersonality();

  /** return a reference to the global allocator function. */
  llvm::Function * getGlobalAlloc();

  /** Generate data structures for a string literal. */
  llvm::Constant * genStringLiteral(const llvm::StringRef & strval,
      const llvm::StringRef & symName = "");

  /** Generate an array literal. */
  llvm::Value * genArrayLiteral(const ArrayLiteralExpr * in);

  /** Generate a closure environment. */
  llvm::Value * genClosureEnv(const ClosureEnvExpr * in);

  /** Generate code to allocate an object, where the object size is not a compile-time constant. */
  llvm::Value * genVarSizeAlloc(const SourceLocation & loc, const Type * objType,
      const Expr * sizeExpr);

  /** Generate code to allocate an object, where the object size is not a compile-time constant. */
  llvm::Value * genVarSizeAlloc(const Type * objType, llvm::Value * sizeValue);

  /** Generate a constant object. */
  llvm::GlobalVariable * genConstantObjectPtr(const ConstantObjectRef * obj, llvm::StringRef name,
      bool synthetic);
  llvm::Constant * genConstantObject(const ConstantObjectRef * obj);

  /** Generate a structure from the fields of a constant object. */
  llvm::Constant * genConstantObjectStruct(
      const ConstantObjectRef * obj, const CompositeType * type);

  /** Generate a constant array. */
  llvm::Constant * genConstantArray(const ConstantNativeArray * array);

  /** Generate a constant union. */
  llvm::Constant * genConstantUnion(const CastExpr * array);

  /** Return the IR module being compiled. */
  llvm::Module * irModule() const { return irModule_; }

  /** Return a constant integer with the specified value. */
  llvm::ConstantInt * getIntVal(int value);

  /** Return a 16-bit constant integer with the specified value. */
  llvm::ConstantInt * getInt16Val(int value);

  /** Return a 32-bit constant integer with the specified value. */
  llvm::ConstantInt * getInt32Val(int value);

  /** Return a 64-bit constant integer with the specified value. */
  llvm::ConstantInt * getInt64Val(int64_t value);

  /** Return the debug compile unit for the specified source file. */
  llvm::DICompileUnit genDICompileUnit(const ProgramSource * source);
  llvm::DIFile genDIFile(const SourceRegion * source);
  llvm::DIFile genDIFile(const Defn * defn);
  llvm::DISubprogram genDISubprogram(const FunctionDefn * fn);
  void genDISubprogramStart(const FunctionDefn * fn);
  void genDIGlobalVariable(const VariableDefn * var, llvm::GlobalVariable * gv);
  unsigned getSourceLineNumber(const SourceLocation & loc);
  void setDebugLocation(const SourceLocation & loc);
  llvm::DIScope genRegionScope(const SourceRegion * region);

  /** Generate debugging information for types. */
  llvm::DIType genDIType(const Type * type);
  llvm::DIBasicType genDIPrimitiveType(const PrimitiveType * type);
  llvm::DICompositeType genDICompositeType(const CompositeType * type);
  llvm::DIType genDIEnumType(const EnumType * type);
  llvm::DICompositeType genDINativeArrayType(const NativeArrayType * type);
  llvm::DICompositeType genDIFlexibleArrayType(const FlexibleArrayType * type);
  llvm::DIDerivedType genDIAddressType(const AddressType * type);
  llvm::DICompositeType genDIUnionType(const UnionType * type);
  llvm::DICompositeType genDITupleType(const TupleType * type);
  llvm::DICompositeType genDIFunctionType(const FunctionType * type);
  llvm::DICompositeType genDIBoundMethodType(const BoundMethodType * type);
  llvm::DIDerivedType genDITypeBase(const CompositeType * type);
  llvm::DIDerivedType genDITypeMember(const VariableDefn * var, llvm::Constant * offset);
  llvm::DIDerivedType genDITypeMember(const Type * type,  const llvm::StructType * irtype,
      llvm::StringRef name, int index);
  llvm::DIType genDIEmbeddedType(const Type * type);
  llvm::DIType genDIParameterType(const Type * type);

  // Return the pointer to the reflection data for this module.
  llvm::GlobalVariable * createModuleObjectPtr();
  llvm::GlobalVariable * createModuleObjectPtr(Module * module);
  llvm::GlobalVariable * createPackageObjectPtr();
  llvm::GlobalVariable * createPackageObjectPtr(Module * module);
  llvm::Constant * createTypeObjectPtr(const Type * type);

  // Generate the function that unboxes arguments from reflection interfaces.
  llvm::Function * genCallAdapterFn(const FunctionType * fnType);

  // Generate the the type of an invoke function.
  const llvm::FunctionType * getCallAdapterFnType();

  /** Generate a reference to the TypeInfoBlock for a proxy type. */
  llvm::Constant * genProxyType(const CompositeType * ctype);

  // Generate the function that boxes arguments when a call is intercepted.
  llvm::Function * genInterceptFn(const FunctionDefn * fn);
private:
  typedef llvm::StringMap<llvm::DIFile> DIFileMap;
  typedef llvm::DenseMap<const FunctionDefn *, llvm::DISubprogram> SubprogramMap;
  typedef llvm::DenseMap<const Type *, llvm::GlobalVariable *> TraceTableMap;
  typedef llvm::DenseMap<const Type *, llvm::Function *> TraceMethodMap;

  /** Find a static method of the given class, and also generate an external reference
      to it from this module. If it's a template, then also instantiate it. This is used
      to call various methods of core classes. */
  llvm::Function * findMethod(const CompositeType * type, const char * methodName);

  void verifyModule();
  void outputModule();

  void addModuleDependencies();

  llvm::Constant * genReflectionDataArray(
      const std::string & baseName, const VariableDefn * var, const ConstantList & values);

  void addTypeName(const CompositeType * type);

  /** Generate code to throw a typecast exception at the current point. */
  void throwCondTypecastError(llvm::Value * typeTestResult);
  void throwTypecastError();

  llvm::Constant * getSizeOfInBits(const llvm::Type * ty);
  llvm::Constant * getAlignOfInBits(const llvm::Type * ty);
  llvm::Constant * getOffsetOfInBits(const llvm::StructType * st, unsigned fieldIndex);

  bool hasAddress(const Expr * expr);
  void ensureLValue(const Expr * expr, const llvm::Type * irType);
  void checkCallingArgs(const llvm::Value * fn,
      ValueList::const_iterator first, ValueList::const_iterator last);
  llvm::Value * loadValue(llvm::Value * value, const Expr * expr, llvm::StringRef name = "");
  llvm::Value * genArgExpr(const Expr * arg, bool saveIntermediateStackRoots);

  void markGCRoot(llvm::Value * value, llvm::Constant * metadata);

  llvm::Value * doAssignment(const AssignmentExpr * in, llvm::Value * lvalue, llvm::Value * rvalue);

  llvm::LLVMContext & context_;
  llvm::IRBuilder<true> builder_;    // LLVM builder

  Module * module_;
  llvm::Module * irModule_;
  llvm::Function * currentFn_;
  const llvm::FunctionType * invokeFnType_;
  llvm::Value * structRet_;
  const llvm::TargetData * targetData_;
  const llvm::IntegerType * intPtrType_;

  llvm::Function * moduleInitFunc_;
  llvm::BasicBlock * moduleInitBlock_;

  const llvm::PointerType * methodPtrType_;

  Reflector reflector_;
  NameTable nameTable_;

  // Debug information
  DIFileMap dbgFiles_;
  SubprogramMap dbgSubprograms_;
  llvm::DIFactory dbgFactory_;
  llvm::DICompileUnit dbgCompileUnit_;
  llvm::DIFile dbgFile_;
  llvm::DIScope dbgContext_;
  DITypeMap dbgTypeMap_;
  SourceLocation dbgLocation_;

  llvm::BasicBlock * unwindTarget_;
  llvm::Function * unwindRaiseException_;
  llvm::Function * unwindResume_;
  llvm::Function * exceptionPersonality_;
  llvm::Function * exceptionTracePersonality_;
  llvm::Function * globalAlloc_;

  RTTypeMap compositeTypeMap_;
  StringLiteralMap stringLiteralMap_;
  ConstantObjectMap constantObjectMap_;
  TraceTableMap traceTableMap_;
  TraceMethodMap traceMethodMap_;

  // Temporary roots generated for GC
  ValueList tempRoots_;

  bool debug_;
};

#ifdef NDEBUG
#define DASSERT_TYPE_EQ(loc, expected, actual)
#define DASSERT_TYPE_EQ_MSG(loc, expected, actual, msg)
#else
#define DASSERT_TYPE_EQ(expr, expected, actual) \
      if (expected != actual) {\
        diag.error(expr) << "Compiled type mismatch for expression:" << expr; \
        diag.info() << "Expected: '" << expected << "'"; \
        diag.info() << "Actual:   '" << actual << "'"; \
        DFAIL("Called from here"); \
      }

#define DASSERT_TYPE_EQ_MSG(loc, expected, actual, msg) \
      if (expected != actual) {\
        diag.error(loc) << msg; \
        diag.info() << "Expected: '" << expected << "'"; \
        diag.info() << "Actual:   '" << actual << "'"; \
        DFAIL("Called from here"); \
      }

#endif

FormatStream & operator<<(FormatStream & out, const llvm::Type * type);
FormatStream & operator<<(FormatStream & out, const llvm::Value * value);
FormatStream & operator<<(FormatStream & out, const ValueList & values);

}

#endif
