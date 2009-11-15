/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_TYPECONVERSION_H
#define TART_CFG_TYPECONVERSION_H

#ifndef TART_COMMON_FORMATTABLE_H
#include "tart/Common/Formattable.h"
#endif

namespace tart {

class Type;
class Expr;

/// -------------------------------------------------------------------
/// Represents the degrees of compatibility between two types. Higher
/// numerical values are considered 'more compatible' than lower ones.
/// Compatibility values within the same 'rank' are considered to be
/// equally good (or bad). So for example, PrecisionLoss is considered
/// as 'bad' as SignedUnsigned.
enum ConversionRank {
  // Rank 0: Impossible conversions
  Incompatible,      // let x:String = 1     No conversion possible

  // Rank 1: Lossy conversions (cause warning message to be emitted.)
  Truncation,        // let x:ubyte = 256    Value will be truncated
  SignedUnsigned,    // let x:uint = -1      Signed / unsigned mismatch
  PrecisionLoss,     // let x:int = 1.2      Loss of decimal precision
  IntegerToBool,     // let x:bool = 256     Compare with 0

  // Rank 2: Non-lossy conversions
  NonPreferred,      // let x:int = 1.0      Requires transformation

  // Rank 3-4: Trivial conversions
  ExactConversion,   // let x:byte = int(1)  Lossless conversion

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
    Coerce = (1<<0),        // Allow coercive casts
    Dynamic = (1<<1),       // Allow dynamic casts
  };

  const Type * fromType;
  Expr * fromValue;
  Expr ** resultValue;
  int options;

  /** Test conversion from type to type. */
  Conversion(const Type * from);

  /** Test conversion from expression to type. */
  Conversion(Expr * from);

  /** Convert expression. */
  Conversion(Expr * from, Expr ** to, int options = 0);

  /** Returns the 'from' type, with aliases and type parameters resolved. */
  const Type * getFromType() const;

  /** Set conversion option. */
  Conversion & setOption(int option) {
    options |= option;
    return *this;
  }
};


} // namespace tart

#endif
