/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_STRINGTABLE_H
#define TART_COMMON_STRINGTABLE_H

#ifndef LLVM_ADT_STRINGMAP_H
#include <llvm/ADT/StringMap.h>
#endif

#ifndef LLVM_SUPPORT_ALLOCATOR_H
#include <llvm/Support/Allocator.h>
#endif

namespace tart {

/// ---------------------------------------------------------------
/// Class that handles interning of strings.
class StringTable : public llvm::StringMap<char, llvm::BumpPtrAllocator> {
public:
  /** Given a string, search for a pre-existing string that is the same. If found,
      return it, otherwise insert the new string and return that. */
  StringRef intern(StringRef str) {
    return GetOrCreateValue(str, 0).getKey();
  }
};

}

#endif // TART_COMMON_COMPILER_H
