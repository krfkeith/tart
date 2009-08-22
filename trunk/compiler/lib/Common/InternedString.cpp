/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/InternedString.h"
#include <string.h>

namespace tart {

InternedStrings::InternedStrings() {
  idSelf = intern("self");
  idValue = intern("value");
  idConstruct = intern("construct");
  idCreate = intern("create");
  idIndex = intern(".index");
  idCall = intern(".call");
}

InternedStrings::~InternedStrings() {
}

const char * InternedStrings::intern(const char * str) {
  llvm::StringSet<>::iterator it = data_.find(str);
  if (it == data_.end()) {
    data_.insert(str);
    it = data_.find(str);
  }

  return it->first();
}

const char * InternedStrings::intern(const std::string & str) {
  return intern(str.c_str());
}

InternedStrings istrings;

}
