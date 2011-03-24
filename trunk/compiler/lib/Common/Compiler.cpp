/* ================================================================ *
   TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Compiler.h"
#include "tart/Gen/CodeGenerator.h"

namespace tart {

void Compiler::generate(Module * mod) {
  CodeGenerator codeGen(mod);
  codeGen.generate();
}

}
