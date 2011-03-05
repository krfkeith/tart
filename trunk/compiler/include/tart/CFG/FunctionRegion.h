/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_FUNCTIONREGION_H
#define TART_CFG_FUNCTIONREGION_H

#ifndef TART_COMMON_SOURCEREGION_H
#include "tart/Common/SourceRegion.h"
#endif

namespace tart {

class FunctionDefn;

/// -------------------------------------------------------------------
/// A region of code which comprises a function definition.
class FunctionRegion : public SourceRegion {
public:
  FunctionRegion(FunctionDefn * function, SourceRegion * parentRegion)
    : function_(function)
    , parentRegion_(parentRegion)
  {}

  FunctionDefn * function() const { return function_; }

//  static SLC create(FunctionDefn * function, SLC & parentLoc);

  // Overrides

  RegionType regionType() const { return FUNCTION; }
  SourceRegion * parentRegion() const { return parentRegion_; }
  llvm::StringRef getFilePath() const { return parentRegion_->getFilePath(); }
  TokenPosition tokenPosition(SLC & loc) { return parentRegion_->tokenPosition(loc); }
  void trace() const;
  void dump() const;

  // Casting

  static inline bool classof(const FunctionRegion *) { return true; }
  static inline bool classof(const SourceRegion * ss) {
    return ss->regionType() == FUNCTION;
  }

private:
  FunctionDefn * function_;
  SourceRegion * parentRegion_;
};

} // namespace tart

#endif // TART_COMMON_SOURCEREGION_H
