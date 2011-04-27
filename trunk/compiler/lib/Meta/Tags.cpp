/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Meta/Tags.h"

namespace tart {

// ---------------------------------------------------------------
// AST tag values

#ifdef DEFTAG_AST
#undef DEFTAG_AST
#endif

namespace {
  const char * astTagNames[] = {
    #define DEFTAG_AST(x) #x,
    #include "tart/Meta/ASTTags.def"
    #undef DEFTAG_AST
  };
}

const char * meta::AST::str(Tag tag) {
  uint32_t index = (uint32_t)tag;
  return (index < COUNT) ? astTagNames[index] : "<Invalid Tag Value>";
}

}
