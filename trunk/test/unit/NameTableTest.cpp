/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/Meta/NameTable.h"

using namespace tart;

TEST(NameTableTest, SimpleNameEqual) {
  NameTable   table;
  NameTable::Name * n1 = table.addName("foo");
  NameTable::Name * n2 = table.addName("foo");
  ASSERT_EQ(n1, n2);
  ASSERT_EQ(0, n1->useCount());
}

TEST(NameTableTest, SimpleNameNotEqual) {
  NameTable   table;
  NameTable::Name * n1 = table.addName("foo");
  NameTable::Name * n2 = table.addName("bar");
  ASSERT_NE(n1, n2);
  ASSERT_EQ(0, n1->useCount());
  ASSERT_EQ(0, n2->useCount());
}

TEST(NameTableTest, CompoundNameEqual) {
  NameTable   table;
  NameTable::Name * n1 = table.addName("foo");
  NameTable::Name * n2 = table.addName("bar");
  NameTable::Name * c1 = table.addName(n1, n2);
  NameTable::Name * c2 = table.addName(n1, n2);
  ASSERT_EQ(c1, c2);
  ASSERT_EQ(0, c1->useCount());
  ASSERT_EQ(1, n1->useCount());
  ASSERT_EQ(1, n2->useCount());
}

TEST(NameTableTest, CompoundNameNotEqual) {
  NameTable   table;
  NameTable::Name * n1 = table.addName("foo");
  NameTable::Name * n2 = table.addName("bar");
  NameTable::Name * c1 = table.addName(n1, n1);
  NameTable::Name * c2 = table.addName(n1, n2);
  ASSERT_NE(c1, c2);
  ASSERT_EQ(0, c1->useCount());
  ASSERT_EQ(0, c2->useCount());
  ASSERT_EQ(3, n1->useCount());
  ASSERT_EQ(1, n2->useCount());
}

TEST(NameTableTest, SimpleNameSorting) {
  NameTable   table;
  NameTable::Name * na = table.addName("a")->use()->use();
  NameTable::Name * nc = table.addName("c")->use()->use();
  NameTable::Name * nd = table.addName("d")->use()->use();
  NameTable::Name * nb = table.addName("b")->use()->use();
  table.assignIndices();

  ASSERT_EQ(0, na->index());
  ASSERT_EQ(1, nb->index());
  ASSERT_EQ(2, nc->index());
  ASSERT_EQ(3, nd->index());
}

TEST(NameTableTest, AssignIds) {
  NameTable   table;
  NameTable::Name * n1 = table.addName("aaa")->use()->use();
  NameTable::Name * n2 = table.addName("bbb")->use()->use();
  NameTable::Name * c1 = table.addName(n1, n1)->use()->use();
  NameTable::Name * c2 = table.addName(n1, n2)->use()->use();
  NameTable::Name * c3 = table.addName(n2, n2)->use()->use();
  table.assignIndices();

  ASSERT_EQ(0, n1->index());
  ASSERT_EQ(1, n2->index());
  ASSERT_EQ(0, c1->index());
  ASSERT_EQ(1, c2->index());
  ASSERT_EQ(2, c3->index());
}

TEST(NameTableTest, QualifiedNames) {
  NameTable table;
  NameTable::CompoundName * c0 = static_cast<NameTable::CompoundName *>(
      table.addQualifiedName("a.b.c"));
  ASSERT_TRUE(c0->isCompound());
  ASSERT_TRUE(c0->first()->isCompound());
  ASSERT_FALSE(c0->second()->isCompound());
}
