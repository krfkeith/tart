/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DEFN_TEMPLATECONDITIONS_H
#define TART_DEFN_TEMPLATECONDITIONS_H

#ifndef TART_DEFN_TEMPLATE_H
#include "tart/Defn/Template.h"
#endif

namespace tart {

class TypeTransform;

/// -------------------------------------------------------------------
/// Defines a guard condition for a template.
class TemplateCondition : public GC, public Formattable {
public:
  virtual bool eval() const = 0;
  virtual TemplateCondition * transform(TypeTransform & transform) = 0;
};

/// -------------------------------------------------------------------
/// Defines a guard condition that compares two types.
class TypeComparisonCondition : public TemplateCondition {
public:

  TypeComparisonCondition(QualifiedType first, QualifiedType second)
    : first_(first)
    , second_(second)
  {}

  QualifiedType first() const { return first_; }
  QualifiedType second() const { return second_; }

  void trace() const;

protected:
  QualifiedType first_;
  QualifiedType second_;
};

/// -------------------------------------------------------------------
/// Defines a subclass test.
class IsSubtypeCondition : public TypeComparisonCondition {
public:

  IsSubtypeCondition(QualifiedType first, QualifiedType second)
    : TypeComparisonCondition(first, second)
  {}

  bool eval() const;
  TemplateCondition * transform(TypeTransform & transform);
  void format(FormatStream & out) const;
};

} // namespace tart

#endif // TART_DEFN_TEMPLATECONDITIONS_H
