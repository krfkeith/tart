/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_INTERNEDSTRING_H
#define TART_COMMON_INTERNEDSTRING_H

#include <llvm/ADT/StringSet.h>
#include <string>

namespace tart {

// -------------------------------------------------------------------
// Table of interned strings
class InternedStrings {

public:
  InternedStrings();
  ~InternedStrings();

  const char * intern(const char * str);
  const char * intern(const std::string & str);

  // Language variables
  const char * idSelf;    // The self parameter
  const char * idValue;   // Name of default setter argument

  // Special IDs, all of which begin with a dot.
  const char * idConstruct; // Name of constructors
  const char * idCreate;    // Name of creators
  const char * idCoerce;    // Name of coercers
  const char * idIndex;     // Name of 'def []'.
  const char * idCall;      // Name of 'def ()'.

private:
  llvm::StringSet<> data_;
};

extern InternedStrings istrings;

}

#endif
