/** Similar to StructBuilder, but works purely with LLVM types. */

#include <llvm/DerivedTypes.h>
#include <llvm/Constant.h>

typedef std::vector<llvm::Constant *> ConstantList;

namespace tart {
using namespace llvm;

class ConstantBuilder {
public:
  ConstantBuilder(Module & module)
    : module_(module)
    , context_(module.getContext())
  {
  }

private:
  ConstantList members_;
  Module & module_;
  LLVMContext & context_;
};

}
