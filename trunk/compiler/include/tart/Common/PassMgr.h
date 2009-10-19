/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_PASSMGR_H
#define TART_COMMON_PASSMGR_H

#ifndef TART_COMMON_SMALLENUMSET_H
#include "tart/Common/SmallEnumSet.h"
#endif

#ifndef TART_COMMON_DIAGNOSTICS_H
#include "tart/Common/Diagnostics.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Tracks the state of analysis of a definition.

template<class PassType, PassType numPasses>
class PassMgr {
public:
  typedef SmallEnumSet<PassType, numPasses> PassSet;

  /** Return the set of passes in progress. */
  const PassSet & running() const { return running_; }
  PassSet & running() { return running_; }

  /** Return the set of analysis passes completed so far. */
  const PassSet & finished() const { return finished_; }
  PassSet & finished() { return finished_; }

  /** Return true if the specified pass is running. */
  bool isRunning(PassType pass) const { return running_.contains(pass); }

  /** Return true if the specified pass is finished. */
  bool isFinished(PassType pass) const { return finished_.contains(pass); }

  /** Mark a pass has started. */
  bool begin(PassType pass, bool quiet = false) {
    if (finished_.contains(pass)) {
      return false;
    }

    if (running_.contains(pass)) {
      if (!quiet) {
        //diag.fatal(this) << "Infinite recursion during " << pass << " of " << this;
        diag.fatal() << "Infinite recursion during analysis";
      }

      return false;
    }

    running_.add(pass);
    return true;
  }

  /** Mark a pass as ended. */
  void finish(PassType pass) {
    running_.remove(pass);
    finished_.add(pass);
  }

private:
  PassSet running_;
  PassSet finished_;
};

} // namespace tart

#endif // TART_COMMON_PASSMGR_H
