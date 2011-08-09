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
#include "llvm/Analysis/DIBuilder.h"
#include "llvm/Target/TargetData.h"

// If true, means that structs are passed as first-class values internally within
// a function; If false, it means they are passed as pointers.
#define FC_STRUCTS_INTERNAL 1

namespace llvm {
class MDString;
}

namespace tart {

class ASTNode;
class ASTDecl;
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
class SeqExpr;
class IfExpr;
class WhileExpr;
class ForExpr;
class ForEachExpr;
class SwitchExpr;
class MatchExpr;
class TryExpr;
class ThrowExpr;
class ReturnExpr;
class BranchExpr;
class LocalProcedureExpr;
class Defn;
class TypeDefn;
class ValueDefn;
class VariableDefn;
class Type;
class FunctionDefn;
class TypeDefn;
class PropertyDefn;
class IndexerDefn;
class CompositeType;
class EnumType;
class FunctionType;
class PrimitiveType;
class AddressType;
class NativeArrayType;
class FlexibleArrayType;
class UnionType;
class TupleType;
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
typedef llvm::DenseMap<llvm::GlobalVariable *, llvm::Constant *> StaticRootMap;
typedef llvm::StringMap<llvm::Constant *> StringLiteralMap;
typedef llvm::SmallVector<LocalScope *, 4> LocalScopeList;

/// -------------------------------------------------------------------
/// BlockExits - used to store information relating to an exit from a
/// block, such as cleanup handlers (for finally or with statements)
/// or break and continue jump targets.

class BlockExits {
public:
  typedef llvm::SmallPtrSet<llvm::BasicBlock *, 8> BlockSet;

  BlockExits(BlockExits * parent)
    : parent_(parent)
    , breakBlock_(NULL)
    , continueBlock_(NULL)
    , localReturnBlock_(NULL)
    , unwindBlock_(NULL)
    , cleanupBlock_(NULL)
    , breakUsed_(false)
    , returnVar_(NULL)
  {}

  /** Get the block exits surrounding this one. */
  BlockExits * parent() const { return parent_; }

  /** Get the block to which a break statement should transfer control. */
  llvm::BasicBlock * breakBlock() const { return breakBlock_; }
  BlockExits & setBreakBlock(llvm::BasicBlock * blk) { breakBlock_ = blk; return *this; }

  /** Get the block to which a continue statement should transfer control. */
  llvm::BasicBlock * continueBlock() const { return continueBlock_; }
  BlockExits & setContinueBlock(llvm::BasicBlock * blk) { continueBlock_ = blk; return *this; }

  /** Get the block to which a local (macro) return statement should transfer control. */
  llvm::BasicBlock * localReturnBlock() const { return localReturnBlock_; }
  BlockExits & setLocalReturnBlock(llvm::BasicBlock * blk) {
    localReturnBlock_ = blk; return *this;
  }

  /** Get the block containing the exception catch dispatch logic. */
  llvm::BasicBlock * unwindBlock() const { return unwindBlock_; }
  BlockExits & setUnwindBlock(llvm::BasicBlock * blk) { unwindBlock_ = blk; return *this; }

  /** Get the block containing cleanup code which should be run before exiting this scope. */
  llvm::BasicBlock * cleanupBlock() const { return cleanupBlock_; }
  BlockExits & setCleanupBlock(llvm::BasicBlock * blk) { cleanupBlock_ = blk; return *this; }

  /** Return 'true' if the continue block was ever used. */
  bool breakUsed() const { return breakUsed_; }
  void setBreakUsed(bool used) { breakUsed_ = used; }

  /** Get the variable that stores the address to return to after the cleanup has finished. */
  llvm::Value * returnVar() const { return returnVar_; }
  BlockExits & setReturnVar(llvm::Value * value) { returnVar_ = value; return *this; }

  /** Get the list of blocks to which we return after executing the cleanup block. */
  const BlockSet & returnBlocks() const { return returnBlocks_; }
  BlockSet & returnBlocks() { return returnBlocks_; }

private:
  BlockExits * parent_;
  llvm::BasicBlock * breakBlock_;
  llvm::BasicBlock * continueBlock_;
  llvm::BasicBlock * localReturnBlock_;
  llvm::BasicBlock * unwindBlock_;
  llvm::BasicBlock * cleanupBlock_;
  bool breakUsed_;
  llvm::Value * returnVar_;
  BlockSet returnBlocks_;
};

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
  llvm::Constant * genCallableDefn(const FunctionDefn * fn);

  // Methods to generate the contents of a definition

  bool genFunction(FunctionDefn * fdef);
  bool genLetDefn(VariableDefn * let);

  /** Generate IR types. */
  llvm::Type * genTypeDefn(TypeDefn * typeDef);
  llvm::Type * genPrimitiveType(PrimitiveType * tdef);
  llvm::Type * genCompositeType(const CompositeType * tdef);
  llvm::Type * genEnumType(EnumType * tdef);

    /** Generate the code that allocates storage for locals on the stack. */
  void genLocalStorage(LocalScopeList & lsl);
  void genLocalRoots(LocalScopeList & lsl);
  void genLocalVar(VariableDefn * var, llvm::Value * initialVal);
  void genGCRoot(llvm::Value * lValue, const Type * varType,
      StringRef rootName = StringRef());
  llvm::Value * addTempRoot(const Type * type, llvm::Value * value, const llvm::Twine & name);
  void initGCRoot(llvm::Value * allocaValue);
  size_t rootStackSize() const { return rootStack_.size(); }
  void pushGCRoot(llvm::Value * allocaValue, const Type * varType);
  void pushRoots(LocalScope * scope);
  void popRootStack(size_t level = 0);

  /** Generate the function body from the basic block list. */
  bool genTestExpr(const Expr * test, llvm::BasicBlock * trueBlk, llvm::BasicBlock * falseBlk);

  /** Statement expressions. */
  llvm::Value * genSeq(const SeqExpr * in);
  llvm::Value * genIf(const IfExpr * in);
  llvm::Value * genWhile(const WhileExpr * in);
  llvm::Value * genDoWhile(const WhileExpr * in);
  llvm::Value * genFor(const ForExpr * in);
  llvm::Value * genForEach(const ForEachExpr * in);
  llvm::Value * genSwitch(const SwitchExpr * in);
  llvm::Value * genIntegerSwitch(const SwitchExpr * in);
  llvm::Value * genEqSwitch(const SwitchExpr * in);
  llvm::Value * genMatch(const MatchExpr * in);
  llvm::Value * genTry(const TryExpr * in);
  llvm::Value * genThrow(const ThrowExpr * in);
  llvm::Value * genReturn(const ReturnExpr * in);
  llvm::Value * genYield(const ReturnExpr * in);
  llvm::Value * genBreak(const BranchExpr * in);
  llvm::Value * genContinue(const BranchExpr * in);
  llvm::Value * genLocalProcedure(const LocalProcedureExpr * in);
  llvm::Value * genLocalReturn(const BranchExpr * in);

  /** Generate an expression (an RValue). */
  llvm::Value * genExpr(const Expr * expr);
  llvm::Constant * genConstExpr(const Expr * expr);
  llvm::Constant * genConstRef(const Expr * in, StringRef name, bool synthetic);
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
  llvm::Value * defaultAlloc(const tart::Expr * size);
  llvm::Value * genCompositeCast(llvm::Value * in, const CompositeType * fromCls,
      const CompositeType * toCls, bool throwOnFailure);

  /** Load an expression */
  llvm::Value * genLoadLValue(const LValueExpr * lval, bool derefShared);

  /** Get the address of a value. */
  llvm::Value * genLValueAddress(const Expr * in);

  /** Load the value of a member field. */
  llvm::Value * genLoadMemberField(const LValueExpr * lval, bool derefShared);

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
      ValueList::iterator firstArg, ValueList::iterator lastArg, const llvm::Twine & name);

  /** Get the address of a value. */
  llvm::Value * genBoundMethod(const BoundMethodExpr * in);

  /** Generate an upcast instruction. */
  llvm::Value * genUpCastInstr(llvm::Value * val, const Type * fromType, const Type * toType);

  /** Generate an 'isa' test for either composite or union types. */
  llvm::Value * genTypeTest(llvm::Value * val, const Type * fromType, const Type * toType,
      bool valIsLval);

  /** Generate an 'isa' test for composite types. */
  llvm::Value * genCompositeTypeTest(llvm::Value * val, const CompositeType * fromType,
      const CompositeType * toType);

  /** Generate an 'isa' test for union types. */
  llvm::Value * genUnionTypeTest(llvm::Value * val, const UnionType * fromType,
      const Type * toType, bool isLValValue);

  /** Generate a reference to the TypeInfoBlock for this type. */
  llvm::Constant * getTypeInfoBlockPtr(const CompositeType * ctype);
  bool createTypeInfoBlock(RuntimeTypeInfo * rtype);
  bool createTemplateTypeInfoBlock(const CompositeType * type);

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

  /** return a reference to the global gc_alloc function (allocates memory in the nursery space). */
  llvm::Function * getGcAlloc();

  /** Generate data structures for a string literal. */
  llvm::Constant * genStringLiteral(StringRef strval, StringRef symName = "");

  /** Generate an array literal. */
  llvm::Value * genArrayLiteral(const ArrayLiteralExpr * in);

  /** Generate a closure environment. */
  llvm::Value * genClosureEnv(const ClosureEnvExpr * in);

  /** Generate code to allocate an object, where the object size is not a compile-time constant. */
  llvm::Value * genVarSizeAlloc(const Type * objType, llvm::Value * sizeValue);

  /** Generate a constant object. */
  llvm::GlobalVariable * genConstantObjectPtr(const ConstantObjectRef * obj, StringRef name,
      bool synthetic);

  /** Generate the contents of a constant object. */
  llvm::Constant * genConstantObject(const ConstantObjectRef * obj);

  /** Generate a structure from the fields of a constant object. */
  llvm::Constant * genConstantObjectStruct(
      const ConstantObjectRef * obj, const CompositeType * type);

  /** Generate a constant array. */
  llvm::Constant * genConstantNativeArray(const ConstantNativeArray * array);

  /** Generate a pointer to a constant array. */
  llvm::Constant * genConstantNativeArrayPtr(const ConstantNativeArray * array,
      StringRef name);

  /** Generate a constant union. */
  llvm::Constant * genConstantUnion(const CastExpr * array);

  /** Generate a constant object. */
  llvm::Constant * genConstantEmptyArray(const CompositeType * arrayType);

  /** Mark this variable as being a static root for garbage collection. */
  void addStaticRoot(llvm::GlobalVariable * var, const Type * type);

  /** Emit the table of static roots. */
  void emitStaticRoots();

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
  void genDICompileUnit();
  llvm::DIScope compileUnit();
  llvm::DIFile genDIFile(const ProgramSource * source);
  llvm::DIFile genDIFile(const Defn * defn);
  llvm::DISubprogram genDISubprogram(const FunctionDefn * fn);
  llvm::DIDescriptor genDefnScope(const Defn * de);
  llvm::DILexicalBlock genLexicalBlock(const SourceLocation & loc);
  void genDISubprogramStart(const FunctionDefn * fn);
  void genDIParameter(const ParameterDefn * param);
  void genDIGlobalVariable(const VariableDefn * var, llvm::GlobalVariable * gv);
  void genDILocalVariable(const VariableDefn * var, llvm::Value * value);
  unsigned getSourceLineNumber(const SourceLocation & loc);
  void setDebugLocation(const SourceLocation & loc);
  void clearDebugLocation();

  /** Generate debugging information for types. */
  llvm::DIType genDIType(const Type * type);
  llvm::DIType genDIPrimitiveType(const PrimitiveType * type);
  llvm::DIType genDICompositeType(const CompositeType * type);
  llvm::DIType genDIEnumType(const EnumType * type);
  llvm::DIType genDINativeArrayType(const NativeArrayType * type);
  llvm::DIType genDIFlexibleArrayType(const FlexibleArrayType * type);
  llvm::DIType genDIAddressType(const AddressType * type);
  llvm::DIType genDIUnionType(const UnionType * type);
  llvm::DIType genDITupleType(const TupleType * type);
  llvm::DIType genDIFunctionType(const FunctionType * type);
  llvm::DIType genDITypeMember(llvm::DIDescriptor scope, const VariableDefn * var,
      uint64_t & offset);
  llvm::DIType genDITypeMember(llvm::DIDescriptor Scope, llvm::Type * type,
      llvm::DIType memberType, StringRef name, unsigned sourceLine, uint64_t & offset);
  llvm::DIType genDIEmbeddedType(const Type * type);
  llvm::DIType genDIParameterType(const Type * type);

  // Return the pointer to the reflection data for this module.
  llvm::GlobalVariable * createModuleObjectPtr();
  llvm::GlobalVariable * createModuleObjectPtr(Module * module);
  llvm::GlobalVariable * createPackageObjectPtr();
  llvm::GlobalVariable * createPackageObjectPtr(Module * module);
  llvm::Value * getTypeObjectPtr(const Type * type);
  llvm::Constant * getCompositeTypeObjectPtr(const CompositeType * type);
  llvm::Constant * getPrimitiveTypeObjectPtr(const PrimitiveType * type);

  // Generate the function that unboxes arguments from reflection interfaces.
  llvm::Function * genCallAdapterFn(const FunctionType * fnType);

  // Generate the the type of an invoke function.
  llvm::FunctionType * getCallAdapterFnType();

  /** Generate a reference to the TypeInfoBlock for a proxy type. */
  llvm::Constant * genProxyType(const CompositeType * ctype);

  // Generate the function that boxes arguments when a call is intercepted.
  llvm::Function * genInterceptFn(const FunctionDefn * fn);

  /** Return the void * type for method pointers in a method table. */
  llvm::Type * getMethodPointerType() { return methodPtrType_; }

  // Module metadata methods
  void genModuleMetadata();
  llvm::MDNode * getFormatVersion();
  llvm::MDNode * getModuleSource();
  llvm::MDNode * getModuleDeps();
  llvm::MDNode * getModuleTimestamp();

private:
  typedef llvm::StringMap<llvm::DIFile> DIFileMap;
  typedef llvm::DenseMap<const FunctionDefn *, llvm::DISubprogram> SubprogramMap;
  typedef llvm::DenseMap<const Type *, llvm::GlobalVariable *> TraceTableMap;
  typedef llvm::DenseMap<const Type *, llvm::Function *> TraceMethodMap;

  /** Create a new basic block and append it to the current function. */
  llvm::BasicBlock * createBlock(const llvm::Twine & blkName);

  /** Create a new basic block and append it to the current function, but only if it doesn't
      already exist, otherwise just return the existing block. */
  llvm::BasicBlock * ensureBlock(const llvm::Twine & blkName, llvm::BasicBlock * blk) {
    return blk ? blk : createBlock(blkName);
  }

  /** Return the current unwind block, if any. */
  llvm::BasicBlock * getUnwindBlock() {
    return getUnwindBlockImpl(blockExits_);
  }

  /** Return the unwind block for a given BlockExits. This will return a block which,
      when branched to, will call any appropriate cleanup handlers before branching
      to the catch dispatcher. */
  llvm::BasicBlock * getUnwindBlockImpl(BlockExits * be);

  /** Move the block 'blk' to the current insertion point, or to the end of the function
      if the insertion point is clear. */
  void moveToEnd(llvm::BasicBlock * blk);

  /** Return true if the current instruction insertion point is at a terminator instruction. */
  bool atTerminator() const;

  /** Call a saved cleanup function. This terminates the current block.
      After cleanup, control will resume at blkNext, if it is non-NULL. If
      blkNext is NULL, then a new block will be created. The insertion point
      will be set to the beginning of this block. */
  void callCleanup(BlockExits * be, llvm::BasicBlock * blkNext = NULL);

  /** Find a static method of the given class, and also generate an external reference
      to it from this module. If it's a template, then also instantiate it. This is used
      to call various methods of core classes. */
  llvm::Function * findMethod(const CompositeType * type, const char * methodName);

  void verifyModule();
  void outputModule();

  void addModuleDependencies();

  llvm::Constant * genReflectionDataArray(
      const std::string & baseName, const VariableDefn * var, const ConstantList & values);

  /** Generate code to throw a typecast exception at the current point. */
  void throwCondTypecastError(llvm::Value * typeTestResult);
  void throwTypecastError();

  uint64_t getSizeOfInBits(llvm::Type * ty);
  uint64_t getAlignOfInBits(llvm::Type * ty);
  uint64_t align(uint64_t offset, uint64_t align);
  unsigned getDefnFlags(const Defn * de);

  bool hasAddress(const Expr * expr);
  void ensureLValue(const Expr * expr, llvm::Type * irType);
  void checkCallingArgs(const llvm::Value * fn,
      ValueList::const_iterator first, ValueList::const_iterator last);
  llvm::Value * loadValue(llvm::Value * value, const Expr * expr, StringRef name = "");
  llvm::Value * genArgExpr(const Expr * arg, bool saveIntermediateStackRoots);

  void markGCRoot(llvm::Value * value, llvm::Constant * metadata, StringRef rootName = StringRef());

  llvm::Value * doAssignment(const AssignmentExpr * in, llvm::Value * lvalue, llvm::Value * rvalue);

  llvm::LLVMContext & context_;
  llvm::IRBuilder<true> builder_;    // LLVM builder

  Module * module_;
  llvm::Module * irModule_;
  llvm::Function * currentFn_;
  llvm::FunctionType * invokeFnType_;
  llvm::Value * structRet_;
  const llvm::TargetData * targetData_;
  llvm::IntegerType * intPtrType_;
  llvm::Value * voidValue_;

  llvm::Function * moduleInitFunc_;
  llvm::BasicBlock * moduleInitBlock_;

  llvm::PointerType * methodPtrType_;

  Reflector reflector_;
  NameTable nameTable_;
  bool gcEnabled_;

  // Debug information
  DIFileMap dbgFiles_;
  SubprogramMap dbgSubprograms_;
  llvm::DIBuilder diBuilder_;
  llvm::DIFile dbgFile_;
  llvm::DIScope dbgContext_;
  llvm::DIScope dbgInlineContext_;
  DITypeMap dbgTypeMap_;
  SourceLocation dbgLocation_;

  BlockExits * blockExits_;
  bool isUnwindBlock_;
  llvm::Function * unwindRaiseException_;
  llvm::Function * unwindResume_;
  llvm::Function * exceptionPersonality_;
  llvm::Function * exceptionTracePersonality_;
  llvm::Function * globalAlloc_;
  llvm::Function * gcAlloc_;
  llvm::Value * gcAllocContext_;

  RTTypeMap compositeTypeMap_;
  StringLiteralMap stringLiteralMap_;
  ConstantObjectMap constantObjectMap_;
  TraceTableMap traceTableMap_;
  TraceMethodMap traceMethodMap_;
  StaticRootMap staticRoots_;

  // Temporary roots generated for GC
  ValueList rootStack_;

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

FormatStream & operator<<(FormatStream & out, llvm::Type * type);
FormatStream & operator<<(FormatStream & out, const llvm::Value * value);
FormatStream & operator<<(FormatStream & out, const ValueList & values);

} // namespace tart

#endif
