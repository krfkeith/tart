/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "tart/Sema/Infer/ConstraintExpansion.h"
#include "tart/Sema/Infer/TypeAssignment.h"

#include "tart/Type/PrimitiveType.h"

#include "MockProvision.h"
#include "TestHelpers.h"

namespace {
using namespace tart;
using namespace testing;

class ConstraintTest : public testing::Test {
protected:
  ConstraintTest() {}

  Constraint * makeConstraint(const Type * value, Constraint::Kind kind,
      const ProvisionSet & provisions = ProvisionSet()) {
    return new Constraint(SourceLocation(), value, kind, provisions);
  }
};

TEST_F(ConstraintTest, ConstraintAccept) {
  Constraint * c = makeConstraint(&Int8Type::instance, Constraint::EXACT);
  EXPECT_TRUE(c->accepts(&Int8Type::instance));
  EXPECT_FALSE(c->accepts(&Int16Type::instance));

  c = makeConstraint(&Int16Type::instance, Constraint::UPPER_BOUND);
  EXPECT_TRUE(c->accepts(&Int8Type::instance));
  EXPECT_TRUE(c->accepts(&Int16Type::instance));
  EXPECT_FALSE(c->accepts(&Int32Type::instance));

  c = makeConstraint(&Int16Type::instance, Constraint::LOWER_BOUND);
  EXPECT_FALSE(c->accepts(&Int8Type::instance));
  EXPECT_TRUE(c->accepts(&Int16Type::instance));
  EXPECT_TRUE(c->accepts(&Int32Type::instance));
}

TEST_F(ConstraintTest, ConstraintSetInsert) {
  ConstraintSet cs;
  MockProvision prov;

  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::EXACT);
  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::EXACT);
  EXPECT_EQ(1u, cs.size());

  cs.insert(SourceLocation(), &Int64Type::instance, Constraint::EXACT);
  EXPECT_EQ(2u, cs.size());

  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::UPPER_BOUND);
  EXPECT_EQ(3u, cs.size());

  ProvisionSet ps;
  ps.insert(&prov);
  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::EXACT, ps);
  EXPECT_EQ(4u, cs.size());

  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::EXACT, ps);
  EXPECT_EQ(4u, cs.size());
}

TEST_F(ConstraintTest, ConstraintSetEquality) {
  ConstraintSet cset1;
  ConstraintSet cset2;

  EXPECT_TRUE(cset1.equals(cset2));

  cset1.insert(SourceLocation(), &Int32Type::instance, Constraint::EXACT);
  EXPECT_FALSE(cset1.equals(cset2));

  cset2.insert(SourceLocation(), &Int32Type::instance, Constraint::EXACT);
  EXPECT_TRUE(cset1.equals(cset2));

  cset1.insert(SourceLocation(), &Int64Type::instance, Constraint::EXACT);
  EXPECT_FALSE(cset1.equals(cset2));

  cset2.insert(SourceLocation(), &FloatType::instance, Constraint::EXACT);
  EXPECT_FALSE(cset1.equals(cset2));
}

TEST_F(ConstraintTest, ProvisionSetInsert) {
  ProvisionSet ps;
  MockProvision p0;

  // Insert a mock provision into the set
  ps.insert(&p0);

  // Check if it's true
  EXPECT_CALL(p0, check()).WillOnce(Return(true));
  EXPECT_TRUE(ps.check());

  ps.insert(&p0);

  // Should only be called once
  EXPECT_CALL(p0, check()).WillOnce(Return(true));
  EXPECT_TRUE(ps.check());
}

TEST_F(ConstraintTest, ProvisionSetMultipleProvisions) {
  ProvisionSet ps;
  MockProvision p0;
  MockProvision p1;

  // Insert two provisions
  ps.insert(&p0);
  ps.insert(&p1);

  // Both true
  EXPECT_CALL(p0, check()).WillOnce(Return(true));
  EXPECT_CALL(p1, check()).WillOnce(Return(true));
  EXPECT_TRUE(ps.check());

  // One is false
  EXPECT_CALL(p0, check()).WillOnce(Return(true));
  EXPECT_CALL(p1, check()).WillOnce(Return(false));
  EXPECT_FALSE(ps.check());
}

TEST_F(ConstraintTest, ProvisionSetEqual) {
  ProvisionSet ps0;
  ProvisionSet ps1;
  MockProvision p0;
  MockProvision p1;

  ps0.insert(&p0);
  EXPECT_FALSE(ps0.equals(ps1));

  ps1.insert(&p0);
  EXPECT_TRUE(ps0.equals(ps1));

  ps0.insert(&p1);
  EXPECT_FALSE(ps0.equals(ps1));

  ps1.insert(&p1);
  EXPECT_TRUE(ps0.equals(ps1));
}

TEST_F(ConstraintTest, ProvisionSetImplies) {
  ProvisionSet ps0;
  ProvisionSet ps1;
  MockProvision p0;
  MockProvision p1;

  ps0.insert(&p0);
  ps1.insert(&p0);

  EXPECT_TRUE(ps0.implies(ps1));
  EXPECT_TRUE(ps1.implies(ps0));

  ps0.insert(&p1);
  EXPECT_TRUE(ps0.implies(ps1));
  EXPECT_FALSE(ps1.implies(ps0));
}

TEST_F(ConstraintTest, ProvisionSetIsConsistent) {
  ProvisionSet ps;
  MockProvision p0;
  MockProvision p1;
  MockProvision p2;

  ps.insert(&p0);
  ps.insert(&p1);
  ps.insert(&p2);

  EXPECT_CALL(p0, contradicts(&p1)).WillOnce(Return(false));
  EXPECT_CALL(p0, contradicts(&p2)).WillOnce(Return(false));
  EXPECT_CALL(p1, contradicts(&p2)).WillOnce(Return(false));
  EXPECT_CALL(p2, contradicts(_)).Times(0);

  EXPECT_TRUE(ps.isConsistent());

  EXPECT_CALL(p0, contradicts(&p1)).WillOnce(Return(true));
  EXPECT_FALSE(ps.isConsistent());
}

TEST_F(ConstraintTest, IntersectSameTypeConstraints) {
  MockProvision p0;
  MockProvision p1;

  ProvisionSet lenientProvisions;
  lenientProvisions.insert(&p0);

  ProvisionSet strictProvisions;
  strictProvisions.insert(&p0);
  strictProvisions.insert(&p1);

  ProvisionSet disjointProvision;
  disjointProvision.insert(&p1);

  // Two completely equal constraints
  Constraint * c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT),
      makeConstraint(&Int8Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  // One constraint is inexact
  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT),
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT),
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  // The other constraint is inexact
  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  // Both constraints are inexact
  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  // One constraint has stronger provisions
  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT, strictProvisions),
      makeConstraint(&Int8Type::instance, Constraint::EXACT, lenientProvisions));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().equals(lenientProvisions));

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT, lenientProvisions),
      makeConstraint(&Int8Type::instance, Constraint::EXACT, strictProvisions));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().equals(lenientProvisions));

  // Constraints have disjoint provisions
  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT, lenientProvisions),
      makeConstraint(&Int8Type::instance, Constraint::EXACT, disjointProvision));
  EXPECT_TRUE(c0 == NULL);

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT, lenientProvisions),
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND, disjointProvision));
  EXPECT_TRUE(c0 == NULL);

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT, lenientProvisions),
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND, disjointProvision));
  EXPECT_TRUE(c0 == NULL);

  // The other constraint is inexact
  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  // Both constraints are inexact
  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());
}

TEST_F(ConstraintTest, IntersectDifferentTypeConstraints) {
  MockProvision p0;
  MockProvision p1;

  ProvisionSet lenientProvisions;
  lenientProvisions.insert(&p0);

  ProvisionSet strictProvisions;
  strictProvisions.insert(&p0);
  strictProvisions.insert(&p1);

  ProvisionSet disjointProvision;
  disjointProvision.insert(&p1);

  Constraint * c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT),
      makeConstraint(&Int16Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0 == NULL);

  c0 = Constraint::intersect(
      makeConstraint(&Int16Type::instance, Constraint::EXACT),
      makeConstraint(&Int8Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0 == NULL);

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int16Type::instance, Constraint::UPPER_BOUND));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::UPPER_BOUND, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int16Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::UPPER_BOUND, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT),
      makeConstraint(&Int16Type::instance, Constraint::UPPER_BOUND));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int16Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0->value() == &Int8Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int16Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0 == NULL);

  c0 = Constraint::intersect(
      makeConstraint(&Int16Type::instance, Constraint::EXACT),
      makeConstraint(&Int8Type::instance, Constraint::UPPER_BOUND));
  EXPECT_TRUE(c0 == NULL);

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int16Type::instance, Constraint::LOWER_BOUND));
  EXPECT_TRUE(c0->value() == &Int16Type::instance);
  EXPECT_EQ(Constraint::LOWER_BOUND, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int16Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND));
  EXPECT_TRUE(c0->value() == &Int16Type::instance);
  EXPECT_EQ(Constraint::LOWER_BOUND, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int16Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0->value() == &Int16Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int16Type::instance, Constraint::EXACT),
      makeConstraint(&Int8Type::instance, Constraint::LOWER_BOUND));
  EXPECT_TRUE(c0->value() == &Int16Type::instance);
  EXPECT_EQ(Constraint::EXACT, c0->kind());
  EXPECT_TRUE(c0->provisions().empty());

  c0 = Constraint::intersect(
      makeConstraint(&Int8Type::instance, Constraint::EXACT),
      makeConstraint(&Int16Type::instance, Constraint::LOWER_BOUND));
  EXPECT_TRUE(c0 == NULL);

  c0 = Constraint::intersect(
      makeConstraint(&Int16Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int8Type::instance, Constraint::EXACT));
  EXPECT_TRUE(c0 == NULL);

  // TODO: More tests with provisions
}

TEST_F(ConstraintTest, GetSingularSolutiuon) {
  TypeAssignment ta(NULL, NULL);
  ConstraintSet & cs = ta.constraints();

  const Type * t0 = ta.findSingularSolution();
  EXPECT_TRUE(t0 == NULL);

  cs.clear();
  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::EXACT);
  t0 = ta.findSingularSolution();
  EXPECT_EQ(&Int32Type::instance, t0);

  cs.clear();
  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::EXACT);
  cs.insert(SourceLocation(), &Int16Type::instance, Constraint::EXACT);
  t0 = ta.findSingularSolution();
  EXPECT_TRUE(t0 == NULL);

  cs.clear();
  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::LOWER_BOUND);
  cs.insert(SourceLocation(), &Int16Type::instance, Constraint::LOWER_BOUND);
  t0 = ta.findSingularSolution();
  EXPECT_EQ(&Int32Type::instance, t0);

  cs.clear();
  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::UPPER_BOUND);
  cs.insert(SourceLocation(), &Int16Type::instance, Constraint::UPPER_BOUND);
  t0 = ta.findSingularSolution();
  EXPECT_EQ(&Int16Type::instance, t0);

  cs.clear();
  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::EXACT);
  cs.insert(SourceLocation(), &Int16Type::instance, Constraint::LOWER_BOUND);
  t0 = ta.findSingularSolution();
  EXPECT_EQ(&Int32Type::instance, t0);

  cs.clear();
  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::UPPER_BOUND);
  cs.insert(SourceLocation(), &Int16Type::instance, Constraint::EXACT);
  t0 = ta.findSingularSolution();
  EXPECT_EQ(&Int16Type::instance, t0);

  cs.clear();
  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::LOWER_BOUND);
  cs.insert(SourceLocation(), &Int16Type::instance, Constraint::EXACT);
  t0 = ta.findSingularSolution();
  EXPECT_TRUE(t0 == NULL);

  cs.clear();
  cs.insert(SourceLocation(), &Int32Type::instance, Constraint::EXACT);
  cs.insert(SourceLocation(), &Int16Type::instance, Constraint::UPPER_BOUND);
  t0 = ta.findSingularSolution();
  EXPECT_TRUE(t0 == NULL);
}

TEST_F(ConstraintTest, Contradicts) {

  // Two identical constraints shouldn't contradict

  EXPECT_FALSE(Constraint::contradicts(
      makeConstraint(&Int32Type::instance, Constraint::EXACT),
      makeConstraint(&Int32Type::instance, Constraint::EXACT)));

  // Two different types, both EXACT

  EXPECT_TRUE(Constraint::contradicts(
      makeConstraint(&Int32Type::instance, Constraint::EXACT),
      makeConstraint(&Int16Type::instance, Constraint::EXACT)));

  // Lower bound + exact

  EXPECT_TRUE(Constraint::contradicts(
      makeConstraint(&Int32Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int16Type::instance, Constraint::EXACT)));

  EXPECT_FALSE(Constraint::contradicts(
      makeConstraint(&Int16Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int32Type::instance, Constraint::EXACT)));

  EXPECT_FALSE(Constraint::contradicts(
      makeConstraint(&Int32Type::instance, Constraint::EXACT),
      makeConstraint(&Int16Type::instance, Constraint::LOWER_BOUND)));

  EXPECT_TRUE(Constraint::contradicts(
      makeConstraint(&Int16Type::instance, Constraint::EXACT),
      makeConstraint(&Int32Type::instance, Constraint::LOWER_BOUND)));

  // Upper bound + exact

  EXPECT_FALSE(Constraint::contradicts(
      makeConstraint(&Int32Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int16Type::instance, Constraint::EXACT)));

  EXPECT_TRUE(Constraint::contradicts(
      makeConstraint(&Int16Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int32Type::instance, Constraint::EXACT)));

  EXPECT_TRUE(Constraint::contradicts(
      makeConstraint(&Int32Type::instance, Constraint::EXACT),
      makeConstraint(&Int16Type::instance, Constraint::UPPER_BOUND)));

  EXPECT_FALSE(Constraint::contradicts(
      makeConstraint(&Int16Type::instance, Constraint::EXACT),
      makeConstraint(&Int32Type::instance, Constraint::UPPER_BOUND)));

  // Lower bound + upper bound

  EXPECT_TRUE(Constraint::contradicts(
      makeConstraint(&Int32Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int16Type::instance, Constraint::UPPER_BOUND)));

  EXPECT_FALSE(Constraint::contradicts(
      makeConstraint(&Int16Type::instance, Constraint::LOWER_BOUND),
      makeConstraint(&Int32Type::instance, Constraint::UPPER_BOUND)));

  EXPECT_FALSE(Constraint::contradicts(
      makeConstraint(&Int32Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int16Type::instance, Constraint::LOWER_BOUND)));

  EXPECT_TRUE(Constraint::contradicts(
      makeConstraint(&Int16Type::instance, Constraint::UPPER_BOUND),
      makeConstraint(&Int32Type::instance, Constraint::LOWER_BOUND)));

}

}  // namespace
