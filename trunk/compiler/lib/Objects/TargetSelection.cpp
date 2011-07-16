/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Objects/TargetSelection.h"

#include "tart/Common/Diagnostics.h"

#include "llvm/Module.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegistry.h"
#include "llvm/Target/SubtargetFeature.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Host.h"

static llvm::cl::opt<std::string>
optTargetTriple("mtriple", llvm::cl::desc("Override target triple for module"));

static llvm::cl::opt<std::string>
optMArch("march", llvm::cl::desc("Architecture to generate code for (see --version)"));

static llvm::cl::opt<std::string>
optMCPU("mcpu",
    llvm::cl::desc("Target a specific cpu type (-mcpu=help for details)"),
    llvm::cl::value_desc("cpu-name"),
    llvm::cl::init(""));

static llvm::cl::list<std::string>
optMAttrs("mattr",
    llvm::cl::CommaSeparated,
    llvm::cl::desc("Target specific attributes (-mattr=help for details)"),
    llvm::cl::value_desc("a1,+a2,-a3,..."));

namespace tart {

TargetSelection TargetSelection::instance;

void TargetSelection::init() {
  instance.selectTarget();
}

bool TargetSelection::selectTarget() {
  if (initialized_) {
    return true;
  }

  initialized_ = true;

  if (!optTargetTriple.empty()) {
    targetTriple_.setTriple(optTargetTriple);
  } else {
    targetTriple_.setTriple(llvm::sys::getHostTriple());
  }

  // Allocate target machine.  First, check whether the user has explicitly
  // specified an architecture to compile for. If so we have to look it up by
  // name, because it might be a backend that has no mapping to a target triple.
  const llvm::Target * target = 0;
  if (!optMArch.empty()) {
    for (llvm::TargetRegistry::iterator it = llvm::TargetRegistry::begin(),
           ie = llvm::TargetRegistry::end(); it != ie; ++it) {
      if (optMArch == it->getName()) {
        target = &*it;
        break;
      }
    }

    if (!target) {
      diag.error() << "Invalid target '" << optMArch << "'";
      return false;
    }

    // Adjust the triple to match (if known), otherwise stick with the
    // module/host triple.
    llvm::Triple::ArchType archType = llvm::Triple::getArchTypeForLLVMName(optMArch);
    if (archType != llvm::Triple::UnknownArch) {
      targetTriple_.setArch(archType);
    }
  } else {
    std::string err;
    target = llvm::TargetRegistry::lookupTarget(targetTriple_.getTriple(), err);
    if (target == 0) {
      diag.error() << "Error auto-selecting target for module '"
             << err << "'.  Please use the -march option to explicitly "
             << "pick a target.";
      return false;
    }
  }

  // Package up features to be passed to target/subtarget
  std::string featuresStr;
  if (optMAttrs.size()) {
    DFAIL("Features disabled due to LLVM link error - investigate.");
#if 0
    llvm::SubtargetFeatures features;
    for (unsigned i = 0; i != optMAttrs.size(); ++i) {
      features.AddFeature(optMAttrs[i]);
    }

    featuresStr = features.getString();
#endif
  }

  targetMachine_ = target->createTargetMachine(targetTriple_.getTriple(), optMCPU, featuresStr);
  assert(targetMachine_ && "Could not allocate target machine!");
  targetData_ = targetMachine_->getTargetData();
  return true;
}

void TargetSelection::addToModule(llvm::Module * module) {
  if (initialized_) {
    module->setTargetTriple(targetTriple_.getTriple());
  }
}

} // namespace tart
