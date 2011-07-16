/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_LEXICALTYPEORDERING_H
#define TART_TYPE_LEXICALTYPEORDERING_H

namespace tart {

class Type;

/// -------------------------------------------------------------------
/// Defines a total ordering of all unique types. The ordering is
/// somewhat arbitrary, but it's enough to sort type expressions
/// in a deterministic way.
class LexicalTypeOrdering {
public:
  bool operator()(const Type * t0, const Type * t1) const;

  static int compare(const Type * t0, const Type * t1);

  template <class T>
  static int compare(T t0, T t1) {
    if (t0 < t1) { return -1; }
    if (t1 < t0) { return 1; }
    return 0;
  }
};

} // namespace tart

#endif // TART_TYPE_LEXICALTYPEORDERING_H
