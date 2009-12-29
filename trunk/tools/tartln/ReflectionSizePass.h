/** LLVM pass to measure the size of reflection data. */

#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace tart {
using namespace llvm;

class ReflectionSizePass : public ModulePass {
public:
  static char ID;

  ReflectionSizePass()
    : ModulePass(&ID)
    , globalSize_(0)
    , methodCount_(0)
  {

  }

  bool runOnModule(Module & module);
  void measureGlobal(const GlobalValue * val);
  size_t getSizeofGlobalValue(const GlobalValue * val);
  size_t getSizeofConstant(const Constant * c);
  void getSizeofType(const llvm::Type * ty, size_t & size);
  void report() const;

private:
  size_t globalSize_;
  size_t methodCount_;
  SmallPtrSet<const GlobalValue *, 64> globals_;
};

}
