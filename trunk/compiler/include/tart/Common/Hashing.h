/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_HASHING_H
#define TART_COMMON_HASHING_H

namespace tart {

/// -------------------------------------------------------------------
/// Incremental hash function, based on MurmurHash2.

class IncrementalHash {
public:
  IncrementalHash() {
    begin();
  }

  void begin(unsigned seed = 0) {
    hash_ = seed;
    tail_ = 0;
    count_ = 0;
    size_ = 0;
  }

  inline void add(int32_t n) { addImpl(reinterpret_cast<const char *>(&n), sizeof(n)); }
  inline void add(int64_t n) { addImpl(reinterpret_cast<const char *>(&n), sizeof(n)); }
  inline void add(uint32_t n) { addImpl(reinterpret_cast<const char *>(&n), sizeof(n)); }
  inline void add(uint64_t n) { addImpl(reinterpret_cast<const char *>(&n), sizeof(n)); }

  template<class T>
  inline void add(const T * ptr) { addImpl(static_cast<const char *>(&ptr), sizeof(ptr)); }

  unsigned end() {
    mmix(hash_, tail_);
    mmix(hash_, size_);

    hash_ ^= hash_ >> 13;
    hash_ *= m;
    hash_ ^= hash_ >> 15;
    return hash_;
  }

private:

  void addImpl(const char * data, size_t len) {
    size_ += len;

    mixTail(data,len);
    while (len >= 4) {
      uint32_t k = *(uint32_t *)data;
      mmix(hash_, k);
      data += 4;
      len -= 4;
    }
    mixTail(data,len);
  }

  static const unsigned m = 0x5bd1e995;
  static const int r = 24;

  inline void mmix(unsigned & h, unsigned & k) {
    k *= m;
    k ^= k >> r;
    k *= m;
    h *= m;
    h ^= k;
  }

  void mixTail(const char * & data, size_t & len) {
    while (len && ((len<4) || count_)) {
      tail_ |= (*data++) << (count_ * 8);
      count_++;
      len--;

      if(count_ == 4) {
        mmix(hash_,tail_);
        tail_ = 0;
        count_ = 0;
      }
    }
  }

private:
  unsigned hash_;
  unsigned tail_;
  unsigned count_;
  unsigned size_;
};

} // tart

#endif // TART_COMMON_HASHING_H
