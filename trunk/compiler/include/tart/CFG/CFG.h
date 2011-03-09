/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_CFG_H
#define TART_CFG_CFG_H

#ifndef LLVM_ADT_FOLDINGSET_H
#include "llvm/ADT/FoldingSet.h"
#endif

#ifndef LLVM_ADT_SMALLVECTOR_H
#include "llvm/ADT/SmallVector.h"
#endif

#ifndef LLVM_ADT_SMALLSTRING_H
#include "llvm/ADT/SmallString.h"
#endif

#ifndef LLVM_ADT_TWINE_H
#include "llvm/ADT/Twine.h"
#endif

#ifndef LLVM_LLVMCONTEXT_H
#include "llvm/LLVMContext.h"
#endif

#ifndef LLVM_SUPPORT_CASTING_H
#include "llvm/Support/Casting.h"
#endif

namespace tart {

using llvm::dyn_cast;
using llvm::dyn_cast_or_null;
using llvm::cast;
using llvm::cast_or_null;
using llvm::isa;
using llvm::SmallString;
using llvm::Twine;

// -------------------------------------------------------------------
// Forward declarations
class Defn;
class FunctionDefn;
class ParameterDefn;
class Expr;
class ConstantExpr;
class Type;
class CallCandidate;

// -------------------------------------------------------------------
// Container types
typedef llvm::SmallVector<Expr *, 4> ExprList;
typedef llvm::SmallVector<Defn *, 8> DefnList;
typedef llvm::SmallVector<Type *, 8> TypeList;
typedef llvm::SmallVector<const Type *, 8> ConstTypeList;
typedef llvm::SmallVector<ParameterDefn *, 8> ParameterList;
typedef llvm::SmallVector<ConstantExpr *, 8> ConstantExprList;
typedef llvm::SmallVector<CallCandidate *, 8> Candidates;
typedef llvm::SmallVector<FunctionDefn *, 32> MethodList;

} // namespace tart

#endif
