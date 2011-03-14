/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_SOURCEREGION_H
#define TART_COMMON_SOURCEREGION_H

#ifndef TART_COMMON_GC_H
#include "tart/Common/GC.h"
#endif

#ifndef TART_COMMON_SOURCELOCATION_H
#include "tart/Common/SourceLocation.h"
#endif

#include "llvm/Analysis/DebugInfo.h"

#include <string>

namespace tart {

/// -------------------------------------------------------------------
/// Defines the file, function, or local scope within which a source
/// location is located.
class SourceRegion : public GC {
public:
  enum RegionType {
    FILE,
    FUNCTION,
    LEXICAL_BLOCK,
    MACRO_EXPANSION,
  };

  const llvm::DIScope & dbgScope() const { return dbgScope_; }
  llvm::DIScope & dbgScope() { return dbgScope_; }

  /** Type of this region. */
  virtual RegionType regionType() const = 0;

  /** The region enclosing this one. */
  virtual SourceRegion * parentRegion() const = 0;

  /** Return true if region 'other' encloses this one. */
  bool hasParent(const SourceRegion * other) const {
    const SourceRegion * self = this;
    while (self != NULL) {
      if (self == other) {
        return true;
      }
      self = self->parentRegion();
    }
    return false;
  }

  /** If this region was inlined into some other function body, here's the location
      of that point in the function body where it was inlined. */
  virtual SourceLocation inlinedAt() const { return SourceLocation(); }

  /** Return the path of the file containing this region. */
  virtual llvm::StringRef getFilePath() const = 0;

  /** Calculate the token position for the given source location. */
  virtual TokenPosition tokenPosition(const SourceLocation & loc) = 0;

  /** Dump a description of this region. */
  virtual void dump() const = 0;

  /** LLVM dynamic casting primitive. */
  static inline bool classof(const SourceRegion *) { return true; }

private:
  llvm::DIScope dbgScope_;
};

} // namespace tart

#endif // TART_COMMON_SOURCEREGION_H
