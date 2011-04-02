/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_ABSTRACTCOMPILER_H
#define TART_COMMON_ABSTRACTCOMPILER_H

#ifndef TART_COMMON_DIAGNOSTICS_H
#include "tart/Common/Diagnostics.h"
#endif

namespace tart {

class Module;

/// -------------------------------------------------------------------
/// Abstract base class of compilers. Parses and analyzes input files,
/// but doesn't generate code.
class AbstractCompiler {
public:
  virtual ~AbstractCompiler() {}

  void processInputFile(llvm::StringRef infile);

protected:
  virtual void generate(Module * mod) = 0;
};

}

#endif // TART_COMMON_COMPILER_H
