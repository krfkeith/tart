/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/InternedString.h"
#include <string.h>

namespace tart {

size_t InternedStrings::Hash::operator()(const char * s) const {
  // Standard FNV1
  static const size_t FNV_Offset = 2166136261u;
  static const size_t FNV_Prime = 16777619;
  size_t hash = FNV_Offset;
  while (const char p = *s++) {
    hash = (hash * FNV_Prime) ^(p & 0xff);
    hash = (hash * FNV_Prime) ^(p >> 8);
  }

  return hash;
}

InternedStrings::InternedStrings() {
  idSelf = intern("self");
  idValue = intern("value");
  idConstruct = intern("construct");
  idCreate = intern("create");
  idIndex = intern(".index");
  idCall = intern(".call");
}

InternedStrings::~InternedStrings() {
  for (string_set_t::iterator it = data.begin(); it != data.end(); ++it) {
    delete *it;
  }
}

const char * InternedStrings::intern(const char * str) {
  string_set_t::iterator it = data.find(str);
  if (it == data.end()) {
    size_t len = strlen(str);
    char * new_str = new char[len+1];
    memcpy(new_str, str, len + 1);
    data.insert(new_str);
    return new_str;
  }

  return *it;
}

const char * InternedStrings::intern(const std::string & str) {
  return intern(str.c_str());
}

InternedStrings istrings;

}
