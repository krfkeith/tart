/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Objects/TargetSelection.h"

#include "tart/Common/Diagnostics.h"

#include "llvm/Module.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Host.h"

using namespace llvm;

static cl::opt<std::string>
optTargetTriple("mtriple", cl::desc("Override target triple for module"));

static cl::opt<std::string>
optMArch("march", cl::desc("Architecture to generate code for (see --version)"));

static cl::opt<std::string>
optMCPU("mcpu",
    cl::desc("Target a specific cpu type (-mcpu=help for details)"),
    cl::value_desc("cpu-name"),
    cl::init(""));

static cl::list<std::string>
optMAttrs("mattr",
    cl::CommaSeparated,
    cl::desc("Target specific attributes (-mattr=help for details)"),
    cl::value_desc("a1,+a2,-a3,..."));

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
    targetTriple_.setTriple(sys::getDefaultTargetTriple());
  }

  // Allocate target machine.  First, check whether the user has explicitly
  // specified an architecture to compile for. If so we have to look it up by
  // name, because it might be a backend that has no mapping to a target triple.
  const Target * target = 0;
  if (!optMArch.empty()) {
    for (TargetRegistry::iterator it = TargetRegistry::begin(),
           ie = TargetRegistry::end(); it != ie; ++it) {
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
    Triple::ArchType archType = Triple::getArchTypeForLLVMName(optMArch);
    if (archType != Triple::UnknownArch) {
      targetTriple_.setArch(archType);
    }
  } else {
    std::string err;
    target = TargetRegistry::lookupTarget(targetTriple_.getTriple(), err);
    if (target == 0) {
      diag.error() << "Error auto-selecting target for module '"
             << err << "'.  Please use the -march option to explicitly "
             << "pick a target.";
      return false;
    }
  }

  // Package up features to be passed to target/subtarget
  SmallString<0> featuresStr;
  if (optMAttrs.size()) {
    DFAIL("Features disabled due to LLVM link error - investigate.");
#if 0
    SubtargetFeatures features;
    for (unsigned i = 0; i != optMAttrs.size(); ++i) {
      features.AddFeature(optMAttrs[i]);
    }

    featuresStr = features.getString();
#endif
  }

  TargetOptions options;
  targetMachine_ = target->createTargetMachine(targetTriple_.getTriple(), optMCPU,
      StringRef(featuresStr), options);
  assert(targetMachine_ && "Could not allocate target machine!");
  targetData_ = targetMachine_->getTargetData();
  return true;
}

void TargetSelection::addToModule(Module * module) {
  if (initialized_) {
    module->setTargetTriple(targetTriple_.getTriple());
  }
}

} // namespace tart
