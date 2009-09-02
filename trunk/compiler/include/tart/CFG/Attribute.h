/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_CFG_ATTRIBUTE_H
#define TART_CFG_ATTRIBUTE_H

namespace tart {
  
class Defn;
  
/// -------------------------------------------------------------------
/// Information about classes which are attributes.
class AttributeInfo {
public:
  /// Attribute targets - constraints on where attributes can be placed.
  /// This should be kept in sync with Attribute.tart
  enum Target {
    CLASS       = (1<<0),
    STRUCT      = (1<<1),
    INTERFACE   = (1<<2),
    ENUM        = (1<<3),
    NAMESPACE   = (1<<4),
    FUNCTION    = (1<<5),
    CONSTRUCTOR = (1<<6),
    PARAMETER   = (1<<7),
    VARIABLE    = (1<<8),
    PROPERTY    = (1<<9),
    MACRO       = (1<<10),
  };
  
  AttributeInfo()
    : target_(0)
    , retained_(false)
  {
  }

  /** The set of bits indicating the acceptable targets. */
  short target() const { return target_; }
  void setTarget(short value) { target_ = value; }

  /** Whether this attribute is retained in the compiled binary. */
  bool isRetained() const { return retained_; }
  void setRetained(bool value) { retained_ = value; }

  /** Returns true if this attribute is allowed to attach to this defn. */
  bool canAttachTo(Defn * de);

private:
  short target_;
  bool retained_;
};

} // namespace tart

#endif
