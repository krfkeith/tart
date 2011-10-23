/** LLVM pass to generate link-time reflection data. */

#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Target/TargetData.h"

#include "tart/Common/ConstantBuilder.h"

namespace tart {
using namespace llvm;

/** Static Roots pass. */
class StaticRoots : public ModulePass {
public:
  static char ID;

  StaticRoots() : ModulePass(ID) {}
  virtual ~StaticRoots();

  void getAnalysisUsage(AnalysisUsage & AU) const;
  bool runOnModule(Module & module);
};

}
