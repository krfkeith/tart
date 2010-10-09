/** Similar to StructBuilder, but works purely with LLVM types. */

#include <llvm/DerivedTypes.h>
#include <llvm/Constants.h>
#include <llvm/Module.h>

typedef std::vector<llvm::Constant *> ConstantList;

namespace tart {
using namespace llvm;

class ConstantRef {
public:
  ConstantRef() : value_(NULL) {}
  ConstantRef(llvm::Constant * value) : value_(value) {}

  llvm::Constant * value() const { return value_; }
  ConstantRef & setValue(llvm::Constant * value) {
    value_ = value;
    return *this;
  }

  const llvm::Type * type() const { return value_->getType(); }

  ConstantRef operand(unsigned index) {
    return ConstantRef(cast<llvm::Constant>(value_->getOperand(index)));
  }

  ConstantRef operand(unsigned i0, unsigned i1) {
    return operand(i0).operand(i1);
  }

  ConstantRef operand(unsigned i0, unsigned i1, unsigned i2) {
    return operand(i0).operand(i1).operand(i2);
  }

private:
  llvm::Constant * value_;
};

class ConstantBuilder {
public:
  ConstantBuilder(Module & module)
    : module_(module)
    , context_(module.getContext())
  {
  }

  ConstantBuilder & addField(const ConstantRef & ref) {
    addField(ref.value());
    return *this;
  }

  ConstantBuilder & addField(llvm::Constant * value);

  /** Build a constant whose type is the same as 'type' */
  Constant * buildStruct(const llvm::Type * type);

  /** Build a struct constant. */
  Constant * buildStruct();

private:
  ConstantList members_;
  Module & module_;
  LLVMContext & context_;
};

}
