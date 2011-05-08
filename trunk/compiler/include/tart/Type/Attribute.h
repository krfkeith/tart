/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_ATTRIBUTE_H
#define TART_TYPE_ATTRIBUTE_H

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

  enum Propagation {
    SUBTYPES    = (1<<0),
    MEMBERS     = (1<<1),
    CALLERS     = (1<<2),
  };

  enum Retention {
    NONE,
    RUNTIME,
    RUNTIME_ITERABLE,
  };

  AttributeInfo()
    : target_(0)
    , propagation_(0)
    , retention_(NONE)
  {
  }

  /** The set of bits indicating the acceptable targets. */
  short target() const { return target_; }
  void setTarget(short value) { target_ = value; }

  /** Whether this attribute is retained in the compiled binary. */
  short retention() const { return retention_; }
  void setRetention(short value) { retention_ = value; }
  bool isRetained() const { return retention_ != NONE; }

  /** How this attribute is propagated. */
  short propagation() const { return propagation_; }
  void setPropagation(short value) { propagation_ = value; }

  /** Returns true if this attribute is allowed to attach to this defn. */
  bool canAttachTo(const Defn * de) const;

private:
  short target_;
  short propagation_;
  short retention_;
};

} // namespace tart

#endif
