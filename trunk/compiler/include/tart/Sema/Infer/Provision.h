/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_INFER_PROVISION_H
#define TART_SEMA_INFER_PROVISION_H

#ifndef TART_COMMON_GC_H
#include "tart/Common/GC.h"
#endif

#ifndef TART_COMMON_FORMATTABLE_H
#include "tart/Common/Formattable.h"
#endif

#ifndef LLVM_ADT_SMALLPTRSET_H
#include "llvm/ADT/SmallPtrSet.h"
#endif

#ifndef LLVM_ADT_STLEXTRAS_H
#include "llvm/ADT/STLExtras.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Interface defining a precondition for a constraint. All provisions
/// must be met, otherwise the constraint is ignored.
class Provision : public GC, public Formattable {
public:
  virtual bool isType(uint32_t iid) const = 0;

  /** Check whether the provision holds. Returns true if it does. */
  virtual bool check() const = 0;

  /** Returns true if this provision implies the consequent provision. */
  virtual bool implies(const Provision * consequent) const {
    // For the moment, this is just an identity comparison.
    return this == consequent;
  }

  /** Returns true if this provision contradicts another, meaning that both
      cannot be true at the same time. */
  virtual bool contradicts(const Provision * p) const {
    return false;
  }

  // Casting support

  static inline bool classof(const Provision *) { return true; }
};

/// -------------------------------------------------------------------
/// A set of provisions, all of which must be true.
class ProvisionSet : public llvm::SmallPtrSet<const Provision *, 4> {
public:
  /** Insert the provision 'p' into this set if it is non-NULL. */
  bool insertIfValid(const Provision * p) {
    if (p != NULL) {
      return insert(p);
    } else {
      return false;
    }
  }

  /** Return true if all of the provisions in this set are true. */
  bool check() const {
    for (ProvisionSet::const_iterator it = begin(), itEnd = end(); it != itEnd; ++it) {
      if (!(*it)->check()) {
        return false;
      }
    }

    return true;
  }

  /** Return true if every element in 'ps' is also in this, in other words if this
      provision set holds true, then 'ps' must be true as well.
   */
  bool implies(const ProvisionSet & ps) const {
    for (ProvisionSet::const_iterator it = ps.begin(), itEnd = ps.end(); it != itEnd; ++it) {
      if (!count(*it)) {
        return false;
      }
    }

    return true;
  }

  /** The converse of 'implies' - this set is true whenever 'ps' is true. */
  bool subsumes(const ProvisionSet & ps) const {
    return ps.implies(*this);
  }

  /** Return true if two provision sets are equal. */
  bool equals(const ProvisionSet & ps) const {
    if (size() == ps.size()) {
      return implies(ps) && ps.implies(*this);
    }
    return false;
  }

  /** Return true if no provision in the set contradicts any other provision in the set. */
  bool isConsistent() const {
    ProvisionSet::const_iterator itEnd = end();
    for (ProvisionSet::const_iterator it = begin(); it != itEnd; ++it) {
      for (ProvisionSet::const_iterator jt = llvm::next(it); jt != itEnd; ++jt) {
        if ((*it)->contradicts(*jt)) {
          return false;
        }
      }
    }

    return true;
  }
};

} // namespace tart

#endif // TART_SEMA_INFER_PROVISION_H
