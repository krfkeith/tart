/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_CONSTRUCTORANALYZER_H
#define TART_SEMA_CONSTRUCTORANALYZER_H

#ifndef TART_CFG_VARIABLEDEFN_H
#include "tart/CFG/VariableDefn.h"
#endif

#ifndef TART_CFG_BLOCK_H
#include "tart/CFG/Block.h"
#endif

#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/DenseMap.h"

namespace tart {

class CompositeType;
class FunctionDefn;

/// -------------------------------------------------------------------
/// Analyzer for constructors - determines which instance vars have
/// been properly initialized.
struct BlockState {
  llvm::BitVector initialized_;
};

typedef llvm::DenseMap<VariableDefn *, int> VarIndexMap;
typedef llvm::DenseMap<Block *, BlockState> BlockStateMap;

/// -------------------------------------------------------------------
/// Analyzer for constructors - determines which instance vars have
/// been properly initialized.
class ConstructorAnalyzer {
public:
  ConstructorAnalyzer(CompositeType * cls_);

  void run(FunctionDefn * ctor);

private:
  CompositeType * cls_;
  VarIndexMap varIndices_;
  int varCount_;
  BlockStateMap blockStates_;
};

}

#endif
