/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_COMPILER_H
#define TART_COMMON_COMPILER_H

#ifndef TART_COMMON_ABSTRACTCOMPILER_H
#include "tart/Common/AbstractCompiler.h"
#endif

namespace tart {

/// ---------------------------------------------------------------
/// The compiler class
class Compiler : public AbstractCompiler {
protected:
  virtual void generate(Module * mod);
};

}

#endif // TART_COMMON_COMPILER_H
