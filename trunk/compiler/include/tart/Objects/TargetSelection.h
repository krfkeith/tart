/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_OBJECTS_TARGETSELECTION_H
#define TART_OBJECTS_TARGETSELECTION_H

#include "llvm/ADT/Triple.h"
#include "llvm/Target/TargetData.h"

namespace tart {

class TargetSelection {
public:
  TargetSelection()
    : initialized_(false)
    , targetData_(NULL)
    , targetMachine_(NULL)
  {}

  /** Select a target based on the command-line parameters. */
  bool selectTarget();

  /** Add the target selection information to the module. */
  void addToModule(llvm::Module * module);

  /** The TargetData for the selected target. */
  const llvm::TargetData * targetData() const { return targetData_; }

  /** Static initialization function. */
  static void init();

  // Static instance of target selection.
  static TargetSelection instance;

private:
  bool initialized_;
  llvm::Triple targetTriple_;
  const llvm::TargetData * targetData_;
  const llvm::TargetMachine * targetMachine_;
};

} // namespace tart

#endif // TART_GEN_TARGETSELECTOR_H
