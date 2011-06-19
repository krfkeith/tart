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

  TypeComparisonCondition(const Type * first, const Type * second)
    : first_(first)
    , second_(second)
  {}

  const Type * first() const { return first_; }
  const Type * second() const { return second_; }

  void trace() const;

protected:
  const Type * first_;
  const Type * second_;
};

/// -------------------------------------------------------------------
/// Defines a subclass test.
class IsSubtypeCondition : public TypeComparisonCondition {
public:

  IsSubtypeCondition(const Type * first, const Type * second)
    : TypeComparisonCondition(first, second)
  {}

  bool eval() const;
  TemplateCondition * transform(TypeTransform & transform);
  void format(FormatStream & out) const;
};

} // namespace tart

#endif // TART_DEFN_TEMPLATECONDITIONS_H
