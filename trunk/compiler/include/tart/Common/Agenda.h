/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_AGENDA_H
#define TART_COMMON_AGENDA_H

#include <llvm/ADT/SetVector.h>

namespace tart {

/// -------------------------------------------------------------------
/// An agenda is a queue of items that need to be processed in some way.
/// Once added to the queue, items are never removed, however they can
/// be marked as "finished". Adding an item a second time has no effect.
template<class T>
class Agenda {
public:
  Agenda() : pos_(0) {}

  /** Append an item to the end of the agenda. Returns true if the
      item is new to the agenda, false if it is already on the agenda. */
  bool append(T * item) {
    return items_.insert(item);
  }

  /** Get the next unfinished item. */
  T * next() {
    return items_[pos_++];
  }

  /** Return true if there are no more unfinished items. */
  bool empty() const { return pos_ >= items_.size(); }

private:
  typedef llvm::SmallSetVector<T *, 128> ItemSet;

  ItemSet items_;
  size_t pos_;
};

} // tart

#endif // TART_COMMON_AGENDA
