/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_TYPECONVERSION_H
#define TART_TYPE_TYPECONVERSION_H

#ifndef TART_TYPE_QUALIFIEDTYPE_H
#include "tart/Type/QualifiedType.h"
#endif

namespace tart {

class Expr;

/// -------------------------------------------------------------------
/// Represents the degrees of compatibility between two types. Higher
/// numerical values are considered 'more compatible' than lower ones.
/// Compatibility values within the same 'rank' are considered to be
/// equally good (or bad). So for example, PrecisionLoss is considered
/// as 'bad' as SignedUnsigned.
enum ConversionRank {
  // Rank 0: Impossible conversions
  Incompatible,      // let x:String = 1       No conversion possible
  QualifierLoss,     // let x:T = immutable(T) Conversion would lose qualifiers

  // Rank 1: Lossy conversions (cause warning message to be emitted.)
  Truncation,        // let x:ubyte = 256      Value will be truncated
  SignedUnsigned,    // let x:uint = -1        Signed / unsigned mismatch
  PrecisionLoss,     // let x:int = 1.2        Loss of decimal precision
  IntegerToBool,     // let x:bool = 256       Compare with 0

  // Rank 2: Non-lossy conversions
  NonPreferred,      // let x:int = 1.0        Requires transformation

  // Rank 3-4: Trivial conversions
  ExactConversion,   // let x:byte = int(1)    Lossless conversion

  // Rank 5: Identity conversions
  IdenticalTypes,     // let x:int = int(1)   No conversion, same type
};

inline bool isConversionWarning(ConversionRank rank) {
  return rank < NonPreferred;
}

FormatStream & operator<<(FormatStream & out, ConversionRank tc);

/// -------------------------------------------------------------------
/// Input parameters for a type conversion operation.
struct Conversion {
  enum Options {
    CoerceToBool = (1<<0),  // Allow implicit coercion to boolean
    DynamicNull = (1<<1),   // Allow dynamic casts (null if fail)
    Checked = (1<<2),       // Allow dynamic casts (exception if fail)
    Explicit = (1<<3),      // Explicit conversion - allow int to float for example
  };

  const Type * fromType;
  Expr * fromValue;
  Expr ** resultValue;
  int options;

  /** Test conversion from type to type. */
  explicit Conversion(const Type * from, int opts = 0)
    : fromType(from)
    , fromValue(NULL)
    , resultValue(NULL)
    , options(opts)
  {}

  /** Test conversion from expression to type. */
  explicit Conversion(Expr * from, int options = 0);

  /** Convert expression. */
  Conversion(Expr * from, Expr ** to, int options = 0);

  /** Returns the 'from' type, with aliases and type parameters resolved. */
  const Type * getFromType() const;

  /** Set conversion option. */
  Conversion & setOption(int option) {
    options |= option;
    return *this;
  }

  bool isExplicit() const { return (options & Explicit) != 0; }
  bool isChecked() const { return (options & Checked) != 0; }
};

/// -------------------------------------------------------------------
/// Type conversion functions.
namespace TypeConversion {

  enum Options {
    COERCE = (1<<0),        // Allow coercive casts
    DYNAMIC_NULL = (1<<1),  // Allow dynamic casts (null if fail)
    CHECKED = (1<<2),       // Allow dynamic casts (exception if fail)
    EXPLICIT = (1<<3),      // Explicit conversion - allow int to float for example
    UNQUAL = (1<<4),        // Ignore qualifiers (for comparison operations)
  };

  /** Do a type conversion, and return both the conversion rank and the converted type. */
  ConversionRank convert(
      const QualifiedType & srcType, Expr * srcExpr,
      const QualifiedType & dstType, Expr ** dstExpr, int options = 0);

  /** Do a type conversion, and return both the conversion rank and the converted expression. */
  inline ConversionRank convert(
      Expr * srcExpr, const Type * dstType, Expr ** dstExpr, int options = 0) {
    return convert(srcExpr->type(), srcExpr, dstType, dstExpr, options);
  }

  /** Do a type conversion, and return both the conversion rank and the converted expression. */
  inline std::pair<ConversionRank, Expr *> convert(
      Expr * srcExpr, const Type * dstType, int options = 0) {
    Expr * dstExpr = NULL;
    ConversionRank rank = convert(srcExpr->type(), srcExpr, dstType, &dstExpr, options);
    return std::make_pair(rank, dstExpr);
  }

  /** Do a type conversion, and return both the conversion rank and the converted expression. */
  inline std::pair<ConversionRank, Expr *> convert(
      Expr * srcExpr, const QualifiedType & dstType, int options = 0) {
    Expr * dstExpr = NULL;
    ConversionRank rank = convert(srcExpr->type(), srcExpr, dstType, &dstExpr, options);
    return std::make_pair(rank, dstExpr);
  }

  /** Check if a conversion is possible, and return a conversion ranking. */
  inline ConversionRank check(const Type * srcType, const Type * dstType, int options = 0) {
    return convert(srcType, NULL, dstType, NULL, options);
  }

  /** Check if a conversion is possible, and return a conversion ranking. */
  inline ConversionRank check(const QualifiedType & srcType, const QualifiedType & dstType,
      int options = 0) {
    return convert(srcType, NULL, dstType, NULL, options);
  }

  /** Check if a conversion is possible, and return a conversion ranking. */
  inline ConversionRank check(Expr * srcExpr, const QualifiedType & dstType, int options = 0) {
    return convert(srcExpr->type(), srcExpr, dstType, NULL, options);
  }
};

} // namespace tart

#endif // TART_TYPE_TYPECONVERSION_H
