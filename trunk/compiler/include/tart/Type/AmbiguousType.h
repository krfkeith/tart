/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_AMBIGUOUSTYPE_H
#define TART_TYPE_AMBIGUOUSTYPE_H

#ifndef TART_TYPE_TYPECONSTRAINT_H
#include "tart/Type/TypeConstraint.h"
#endif

#ifndef TART_SEMA_INFER_PROVISION_H
#include "tart/Sema/Infer/Provision.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// A prospect is one possibility for an ambiguous type.
class Prospect {
public:
  Prospect(QualifiedType type) : type_(type) {}
  Prospect(QualifiedType type, const ProvisionSet & provisions)
    : type_(type), provisions_(provisions) {}
  Prospect(const Prospect & src) : type_(src.type_), provisions_(src.provisions_) {}

  Prospect & operator=(const Prospect & src) {
    type_ = src.type_;
    provisions_ = src.provisions_;
    return *this;
  }

  /** The type of this prospect. */
  const QualifiedType type() const { return type_; }

  /** The set of provisions associated with this type. */
  const ProvisionSet & provisions() const { return provisions_; }
  ProvisionSet & provisions() { return provisions_; }

private:
  QualifiedType type_;
  ProvisionSet provisions_;
};

typedef llvm::SmallVector<Prospect, 16> ProspectList;

/// -------------------------------------------------------------------
/// An ambiguous type representing the Nth type param of the base
/// type, which may itself be ambiguous.
class AmbiguousType : public TypeConstraint {
public:
  AmbiguousType(TypeClass cls) : TypeConstraint(cls) {}

  /** Return the list of prospects for this type.
      Parameters:
        out: The output list.
        add: Additional provisions to add to each output record.
   */
  virtual void listProspects(
      ProspectList & out,
      const ProvisionSet & add = ProvisionSet()) const = 0;

  // Statics

  /** Static version of getProspects() which handles non-ambi types as well.
      Parameters:
        out: the output list.
         ty: the type to get prospects for. For ordinary types, this just appends
             the type to the list.
        add: Additional provisions to add to each output record.
   */
  static void listProspects(
      ProspectList & out,
      QualifiedType ty,
      const ProvisionSet & add = ProvisionSet());

  // Overrides

  llvm::Type * irType() const;
  TypeShape typeShape() const { return Shape_Unset; }

  static inline bool classof(const AmbiguousType *) { return true; }
  static inline bool classof(const Type * type) {
    return type->typeClass() >= AmbiguousTypesBegin && type->typeClass() <= AmbiguousTypesEnd;
  }

protected:
  /** Format the list of currently possible types. */
  void formatImpl(FormatStream & out) const;
};

} // namespace tart

#endif // TART_TYPE_AMBIGUOUSTYPE_H
