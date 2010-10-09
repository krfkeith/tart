/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_CFG_H
#define TART_CFG_CFG_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/LLVMContext.h"
#include "llvm/Support/Casting.h"

namespace tart {

using llvm::dyn_cast;
using llvm::dyn_cast_or_null;
using llvm::cast;
using llvm::cast_or_null;
using llvm::isa;

// -------------------------------------------------------------------
// Forward declarations
class Defn;
class TypeDefn;
class ValueDefn;
class FunctionDefn;
class ParameterDefn;
class TemplateParamDefn;
class Expr;
class ConstantExpr;
class Module;
class Type;
class Block;
class ParameterAssignments;
class CallCandidate;
class SpCandidate;

// -------------------------------------------------------------------
// Container types
typedef llvm::SmallVector<Expr *, 4> ExprList;
typedef llvm::SmallVector<Defn *, 8> DefnList;
typedef llvm::SmallVector<Type *, 8> TypeList;
typedef llvm::SmallVector<const Type *, 8> ConstTypeList;
typedef llvm::SmallVector<ParameterDefn *, 8> ParameterList;
typedef llvm::SmallVector<ConstantExpr *, 8> ConstantExprList;
typedef llvm::SmallVector<Block *, 16> BlockList;
typedef llvm::SmallVector<CallCandidate *, 8> Candidates;
typedef llvm::SmallVector<FunctionDefn *, 32> MethodList;

} // namespace tart

#endif
