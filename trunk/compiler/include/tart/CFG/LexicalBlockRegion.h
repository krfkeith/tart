/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_LOCALSCOPEREGION_H
#define TART_CFG_LOCALSCOPEREGION_H

#ifndef TART_COMMON_SOURCEREGION_H
#include "tart/Common/SourceRegion.h"
#endif

namespace tart {

class FunctionDefn;

/// -------------------------------------------------------------------
/// A region of code which comprises a function definition.
class LexicalBlockRegion : public SourceRegion {
public:
  LexicalBlockRegion(SLC location, SLC inlinedAt = SourceLocation())
    : location_(location)
    , inlinedAt_(inlinedAt)
  {}

  const SourceLocation & location() const { return location_; }

  // Overrides

  RegionType regionType() const { return LEXICAL_BLOCK; }
  SourceRegion * parentRegion() const { return location_.region; }
  const std::string & getFilePath() const { return location_.region->getFilePath(); }
  TokenPosition tokenPosition(SLC & loc) { return location_.region->tokenPosition(loc); }
  SourceLocation inlinedAt() const { return inlinedAt_; }
  void trace() const;
  void dump() const;

  // Casting

  static inline bool classof(const LexicalBlockRegion *) { return true; }
  static inline bool classof(const SourceRegion * ss) {
    return ss->regionType() == LEXICAL_BLOCK;
  }

private:
  SourceLocation location_;
  SourceLocation inlinedAt_;
};

} // namespace tart

#endif // TART_COMMON_SOURCEREGION_H
