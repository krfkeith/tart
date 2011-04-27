/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

//#include <llvm/Support/DataTypes.h>
#include "tart/Lex/Tokens.h"
#include "llvm/Support/DataTypes.h"
#include <sys/types.h>

#ifdef DEFINE_TOKEN
#undef DEFINE_TOKEN
#endif

#define DEFINE_TOKEN(x) #x,

namespace tart {
  const char * TokenNames[] = {
    #include "tart/Lex/Tokens.def"
  };

  const char * GetTokenName(TokenType tt) {
    uint32_t index = (uint32_t)tt;
    if (index < Token_Last) {
      return TokenNames[index];
    }

    return "<Invalid Token>";
  }
}
