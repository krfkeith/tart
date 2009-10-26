/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_SMALLENUMSET_H
#define TART_COMMON_SMALLENUMSET_H

/// -------------------------------------------------------------------
/// A type that represents a set of enumerated values as a bit vector.
/// The size of the bit vector is fixed, so operations are very fast.
///
/// Example usage:
///
///   TypeDef SmallEnumSet<InstrType, InstrCount> InstrTypeSet;
///   InstrTypeSet valueTypes = InstrTypeSet.of(Load, Store);
///   if (valueTypes.contains(insType)) { ... }
///
template <class EnumType, EnumType numValues>
class SmallEnumSet {
private:
  typedef uint32_t word_t;
  enum {
    WORD_SIZE = (unsigned)sizeof(word_t) * 8,
  };

  static const unsigned NumWords = (numValues + WORD_SIZE - 1) / WORD_SIZE;

  word_t  bits[NumWords];

  static inline unsigned wordForIndex(EnumType index) {
    return unsigned(index) / WORD_SIZE;
  }

  static inline word_t bitForIndex(EnumType index) {
    return 1 << (unsigned(index) & (WORD_SIZE - 1));
  }

  static inline word_t maskForIndex(EnumType index) {
    return -1 << (unsigned(index) & (WORD_SIZE - 1));
  }

public:

  SmallEnumSet() {
    clear();
  }

  SmallEnumSet(EnumType val) {
    clear();
    add(val);
  }

  SmallEnumSet(const SmallEnumSet & src) {
    for (unsigned i = 0; i < NumWords; ++i) {
      bits[i] = src.bits[i];
    }
  }

  void clear() {
    for (unsigned i = 0; i < NumWords; ++i) {
      bits[i] = 0;
    }
  }

  bool empty() const {
    for (unsigned i = 0; i < NumWords; ++i) {
      if (bits[i] != 0) {
        return false;
      }
    }

    return true;
  }

  bool operator==(const SmallEnumSet & in) const {
    for (unsigned i = 0; i < NumWords; ++i) {
      if (bits[i] != in.bits[i]) {
        return false;
      }
    }

    return true;
  }

  bool operator!=(const SmallEnumSet & in) const {
    return !(*this == in);
  }

  bool contains(EnumType val) const {
    return (bits[wordForIndex(val)] & bitForIndex(val)) != 0;
  }

  bool containsAll(const SmallEnumSet & in) const {
    for (unsigned i = 0; i < NumWords; ++i) {
      if ((~bits[i] & in.bits[i]) != 0) {
        return false;
      }
    }

    return true;
  }

  bool containsAny(const SmallEnumSet & in) const {
    for (unsigned i = 0; i < NumWords; ++i) {
      if ((bits[i] & in.bits[i]) != 0) {
        return true;
      }
    }

    return false;
  }

  SmallEnumSet & add(EnumType val) {
    bits[wordForIndex(val)] |= bitForIndex(val);
    return * this;
  }

  SmallEnumSet & addAll(const SmallEnumSet & in) {
    for (unsigned i = 0; i < NumWords; ++i) {
      bits[i] |= in.bits[i];
    }
    return * this;
  }

  SmallEnumSet & addRange(EnumType first, EnumType last) {
    unsigned firstWord = wordForIndex(first);
    unsigned lastWord = wordForIndex(last);
    if (firstWord == lastWord) {
      bits[firstWord] |= maskForIndex(first) & ~maskForIndex(last);
    } else {
      bits[firstWord] |= maskForIndex(first);
      for (unsigned i = firstWord + 1; i < lastWord; ++i) {
        bits[i] = -1;
      }
      bits[lastWord] |= ~maskForIndex(last);
    }

    return * this;
  }

  SmallEnumSet & remove(EnumType val) {
    bits[wordForIndex(val)] &= ~bitForIndex(val);
    return * this;
  }

  SmallEnumSet & removeAll(const SmallEnumSet & in) {
    for (unsigned i = 0; i < NumWords; ++i) {
      bits[i] &= ~in.bits[i];
    }
    return * this;
  }

  SmallEnumSet & removeRange(EnumType first, EnumType last) {
    unsigned firstWord = wordForIndex(first);
    unsigned lastWord = wordForIndex(last);
    if (firstWord == lastWord) {
      bits[firstWord] &= ~maskForIndex(first) | maskForIndex(last);
    } else {
      bits[firstWord] &= ~maskForIndex(first);
      for (unsigned i = firstWord + 1; i < lastWord; ++i) {
        bits[i] = 0;
      }
      bits[lastWord] &= maskForIndex(last);
    }

    return * this;
  }

  SmallEnumSet & intersectWith(const SmallEnumSet & in) {
    for (unsigned i = 0; i < NumWords; ++i) {
      bits[i] &= in.bits[i];
    }
    return * this;
  }

  SmallEnumSet & invert() {
    for (unsigned i = 0; i < NumWords; ++i) {
      bits[i] = ~bits[i];
    }
    return * this;
  }

  static SmallEnumSet noneOf() {
    return SmallEnumSet();
  }

  static SmallEnumSet of(EnumType v0) {
    return SmallEnumSet().add(v0);
  }

  static SmallEnumSet of(EnumType v0, EnumType v1) {
    return SmallEnumSet().add(v0).add(v1);
  }

  static SmallEnumSet of(EnumType v0, EnumType v1, EnumType v2) {
    return SmallEnumSet().add(v0).add(v1).add(v2);
  }

  static SmallEnumSet of(EnumType v0, EnumType v1, EnumType v2, EnumType v3) {
    return SmallEnumSet().add(v0).add(v1).add(v2).add(v3);
  }

  static SmallEnumSet of(
      EnumType v0, EnumType v1, EnumType v2, EnumType v3, EnumType v4) {
    return SmallEnumSet().add(v0).add(v1).add(v2).add(v3).add(v4);
  }

  static SmallEnumSet of(
      EnumType v0, EnumType v1, EnumType v2, EnumType v3, EnumType v4,
      EnumType v5) {
    return SmallEnumSet().add(v0).add(v1).add(v2).add(v3).add(v4).add(v5);
  }

  static SmallEnumSet of(
      EnumType v0, EnumType v1, EnumType v2, EnumType v3, EnumType v4,
      EnumType v5, EnumType v6) {
    return SmallEnumSet().add(v0).add(v1).add(v2).add(v3).add(v4).add(v5)
        .add(v6);
  }

  static SmallEnumSet of(
      EnumType v0, EnumType v1, EnumType v2, EnumType v3, EnumType v4,
      EnumType v5, EnumType v6, EnumType v7) {
    return SmallEnumSet().add(v0).add(v1).add(v2).add(v3).add(v4).add(v5)
        .add(v6).add(v7);
  }

  static SmallEnumSet of(
      EnumType v0, EnumType v1, EnumType v2, EnumType v3, EnumType v4,
      EnumType v5, EnumType v6, EnumType v7, EnumType v8) {
    return SmallEnumSet().add(v0).add(v1).add(v2).add(v3).add(v4).add(v5)
        .add(v6).add(v7).add(v8);
  }

  static SmallEnumSet of(
      EnumType v0, EnumType v1, EnumType v2, EnumType v3, EnumType v4,
      EnumType v5, EnumType v6, EnumType v7, EnumType v8, EnumType v9) {
    return SmallEnumSet().add(v0).add(v1).add(v2).add(v3).add(v4).add(v5)
        .add(v6).add(v7).add(v8).add(v9);
  }

  static SmallEnumSet of(
      EnumType v0, EnumType v1, EnumType v2, EnumType v3, EnumType v4,
      EnumType v5, EnumType v6, EnumType v7, EnumType v8, EnumType v9,
      EnumType v10) {
    return SmallEnumSet().add(v0).add(v1).add(v2).add(v3).add(v4).add(v5)
        .add(v6).add(v7).add(v8).add(v9).add(v10);
  }

  static SmallEnumSet ofRange(EnumType first, EnumType last) {
    return SmallEnumSet().addRange(first, last);
  }

  static SmallEnumSet unionOf(const SmallEnumSet & a, const SmallEnumSet & b) {
    return SmallEnumSet(a).addAll(b);
  }

  static SmallEnumSet complement(const SmallEnumSet & a) {
    return SmallEnumSet(a).invert();
  }

  friend SmallEnumSet operator |(
      const SmallEnumSet & a, const SmallEnumSet & b) {
    return unionOf(a, b);
  }

  friend SmallEnumSet operator &(
      const SmallEnumSet & a, const SmallEnumSet & b) {
    return SmallEnumSet(a).intersectWith(b);
  }

  SmallEnumSet & operator |=(const SmallEnumSet & a) {
    addAll(a);
    return * this;
  }

  SmallEnumSet & operator &=(const SmallEnumSet & a) {
    intersectWith(a);
    return * this;
  }
};

#endif
