/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_COMMON_INTERNEDSTRING_H
#define TART_COMMON_INTERNEDSTRING_H

#include <llvm/ADT/hash_set.h>
#include <string>

namespace tart {

// -------------------------------------------------------------------
// Table of interned strings
class InternedStrings {
  struct Hash {
    size_t operator()(const char * s0) const;
  };
  
  struct Equal {
    bool operator()(const char * s0, const char * s1) const {
      return strcmp(s0, s1) == 0;
    }
  };
  
  typedef hash_set<const char *, Hash, Equal> string_set_t;
  string_set_t data;

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
  const char * idCreate;  // Name of creators
  const char * idIndex;   // Name of 'def []'.
  const char * idCall;    // Name of 'def ()'.
};

extern InternedStrings istrings;

}

#endif
