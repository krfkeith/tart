/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include <gtest/gtest.h>
#include "tart/Common/SmallEnumSet.h"

enum TestEnum {
  Const00,
  Const01,
  Const02,
  Const03,
  Const04,
  Const05,
  Const06,
  Const07,
  Const08,
  Const09,

  TestEnumCount
};

enum LargeTestEnum {
  LConst00,
  LConst01,
  LConst02,
  LConst03,
  LConst04,
  LConst05,
  LConst06,
  LConst07,
  LConst08,
  LConst09,
  LConst10 = 10,
  LConst11 = 11,
  LConst15 = 15,
  LConst20 = 20,
  LConst30 = 30,
  LConst33 = 33,
  LConst40 = 40,
  LConst49 = 49,
  LConst50 = 50,
  LConst60 = 60,
  LConst70 = 70,
  LConst80 = 80,
  LConst90 = 90,
  LConst99 = 99,

  LargeTestEnumCount
};

inline void printRange(std::ostream & out, int first, int last) {
  if (first + 1 == last) {
    out << first;
  } else {
    out << first << "-" << last - 1;
  }
}

// Convert SmallEnumSet into a string representation.
template <class EnumType, EnumType numValues>
inline std::ostream & operator<<(std::ostream & out,
    const SmallEnumSet<EnumType, numValues> & set) {

  out << '<';
  
  int rgStart = -1;
  bool comma = false;
  for (int i = 0; i < numValues; i++) {
    if (set.contains((EnumType)i)) {
      if (rgStart < 0) rgStart = i;
    } else {
      if (rgStart >= 0) {
        if (comma) {
          out << ",";
        }
        printRange(out, rgStart, i);
        rgStart = -1;
        comma = true;
      }
    }
  }
  
  if (rgStart >= 0) {
    if (comma) {
      out << ",";
    }
    printRange(out, rgStart, numValues);
  }
      
  out << '>';
  return out;
}

TEST(SmallEnumSetTest, SmallSetTests) {
  typedef SmallEnumSet<TestEnum, TestEnumCount> TestSet;

  TestSet theSet;
  
  EXPECT_FALSE(theSet.contains(Const00));
  EXPECT_FALSE(theSet.contains(Const01));
  EXPECT_FALSE(theSet.contains(Const09));
  EXPECT_TRUE(theSet.empty());
  
  theSet.add(Const00);
  EXPECT_TRUE(theSet.contains(Const00));
  EXPECT_FALSE(theSet.contains(Const01));
  EXPECT_FALSE(theSet.contains(Const09));
  EXPECT_FALSE(theSet.empty());

  theSet.add(Const01);
  EXPECT_TRUE(theSet.contains(Const00));
  EXPECT_TRUE(theSet.contains(Const01));
  EXPECT_FALSE(theSet.contains(Const09));
  EXPECT_FALSE(theSet.empty());

  theSet.remove(Const00);
  EXPECT_FALSE(theSet.contains(Const00));
  EXPECT_TRUE(theSet.contains(Const01));
  EXPECT_FALSE(theSet.contains(Const09));
  EXPECT_FALSE(theSet.empty());

  theSet.remove(Const01);
  EXPECT_FALSE(theSet.contains(Const00));
  EXPECT_FALSE(theSet.contains(Const01));
  EXPECT_FALSE(theSet.contains(Const09));
  EXPECT_TRUE(theSet.empty());
  
  theSet = TestSet::of(Const01, Const02);
  EXPECT_FALSE(theSet.contains(Const00));
  EXPECT_TRUE(theSet.contains(Const01));
  EXPECT_TRUE(theSet.contains(Const02));
  
  EXPECT_TRUE(theSet.containsAny(TestSet::of(Const01)));
  EXPECT_TRUE(theSet.containsAny(TestSet::of(Const01, Const02)));
  EXPECT_TRUE(theSet.containsAny(TestSet::of(Const01, Const02, Const01)));
  EXPECT_TRUE(theSet.containsAll(TestSet::of(Const01, Const02)));

  EXPECT_TRUE(TestSet::of(Const01, Const02, Const03).contains(Const01));
  EXPECT_TRUE(TestSet::of(Const01, Const02, Const03).contains(Const02));
  EXPECT_TRUE(TestSet::of(Const01, Const02, Const03).contains(Const03));

  EXPECT_TRUE(TestSet::of(Const01, Const02, Const03)
    .containsAll(TestSet::of(Const01, Const02, Const03)));
  EXPECT_TRUE(TestSet::of(Const01, Const02, Const03)
    .containsAny(TestSet::of(Const01, Const02, Const03, Const04)));
  EXPECT_FALSE(TestSet::of(Const01, Const02, Const03)
    .containsAll(TestSet::of(Const01, Const02, Const03, Const04)));

  theSet.clear();
  EXPECT_TRUE(theSet.empty());

  theSet.addAll(TestSet::of(Const06, Const07, Const08));
  EXPECT_FALSE(theSet.contains(Const09));
  theSet.removeAll(TestSet::of(Const06, Const07, Const08));
  EXPECT_TRUE(theSet.empty());
  
  theSet.clear();
  theSet.add(Const01);
  EXPECT_EQ(theSet, TestSet::of(Const01));
  EXPECT_NE(theSet, TestSet::of(Const02));

  theSet.add(Const02);
  EXPECT_EQ(theSet, TestSet::of(Const01, Const02));

  theSet.add(Const03);
  EXPECT_EQ(theSet, TestSet::of(Const01, Const02, Const03));

  theSet.add(Const04);
  EXPECT_EQ(theSet, TestSet::of(Const01, Const02, Const03, Const04));

  theSet.add(Const05);
  EXPECT_EQ(theSet, TestSet::of(Const01, Const02, Const03, Const04, Const05));

  theSet.add(Const06);
  EXPECT_EQ(theSet, TestSet::of(Const01, Const02, Const03, Const04,
      Const05, Const06));

  theSet.add(Const07);
  EXPECT_EQ(theSet, TestSet::of(Const01, Const02, Const03, Const04,
      Const05, Const06, Const07));

  theSet.add(Const08);
  EXPECT_EQ(theSet, TestSet::of(Const01, Const02, Const03, Const04,
      Const05, Const06, Const07, Const08));

  theSet.add(Const09);
  EXPECT_EQ(theSet, TestSet::of(Const01, Const02, Const03, Const04,
      Const05, Const06, Const07, Const08, Const09));
      
  theSet.clear();
  theSet.addRange(Const02, Const06);
  EXPECT_EQ(TestSet::of(Const02, Const03, Const04, Const05), theSet);
  theSet.removeRange(Const03, Const05);
  EXPECT_EQ(TestSet::of(Const02, Const05), theSet);

  //static SmallEnumSet ofRange(EnumType first, EnumType last) {
  //static SmallEnumSet unionOf(const SmallEnumSet & a, const SmallEnumSet & b) {
}

TEST(SmallEnumSetTest, LargeSetTests) {
  typedef SmallEnumSet<LargeTestEnum, LargeTestEnumCount> TestSet;

  TestSet theSet;
  
  EXPECT_FALSE(theSet.contains(LConst00));
  EXPECT_FALSE(theSet.contains(LConst01));
  EXPECT_FALSE(theSet.contains(LConst09));
  EXPECT_TRUE(theSet.empty());
  
  theSet.add(LConst00);
  EXPECT_TRUE(theSet.contains(LConst00));
  EXPECT_FALSE(theSet.contains(LConst01));
  EXPECT_FALSE(theSet.contains(LConst09));
  EXPECT_FALSE(theSet.empty());

  theSet.add(LConst50);
  EXPECT_TRUE(theSet.contains(LConst00));
  EXPECT_TRUE(theSet.contains(LConst50));
  EXPECT_FALSE(theSet.contains(LConst09));
  EXPECT_FALSE(theSet.empty());

  theSet.remove(LConst00);
  EXPECT_FALSE(theSet.contains(LConst00));
  EXPECT_TRUE(theSet.contains(LConst50));
  EXPECT_FALSE(theSet.contains(LConst09));
  EXPECT_FALSE(theSet.empty());

  theSet.remove(LConst50);
  EXPECT_FALSE(theSet.contains(LConst00));
  EXPECT_FALSE(theSet.contains(LConst50));
  EXPECT_FALSE(theSet.contains(LConst09));
  EXPECT_TRUE(theSet.empty());
  
  theSet = TestSet::of(LConst01, LConst40);
  EXPECT_FALSE(theSet.contains(LConst00));
  EXPECT_TRUE(theSet.contains(LConst01));
  EXPECT_TRUE(theSet.contains(LConst40));
  
  EXPECT_TRUE(theSet.containsAny(TestSet::of(LConst01)));
  EXPECT_TRUE(theSet.containsAny(TestSet::of(LConst01, LConst02)));
  EXPECT_TRUE(theSet.containsAny(TestSet::of(LConst01, LConst02, LConst01)));
  EXPECT_TRUE(theSet.containsAll(TestSet::of(LConst01, LConst40)));

  EXPECT_TRUE(TestSet::of(LConst01, LConst02, LConst70).contains(LConst01));
  EXPECT_TRUE(TestSet::of(LConst01, LConst02, LConst70).contains(LConst02));
  EXPECT_TRUE(TestSet::of(LConst01, LConst02, LConst70).contains(LConst70));

  EXPECT_TRUE(TestSet::of(LConst01, LConst02, LConst60)
    .containsAll(TestSet::of(LConst01, LConst02, LConst60)));
  EXPECT_TRUE(TestSet::of(LConst01, LConst02, LConst60)
    .containsAny(TestSet::of(LConst01, LConst02, LConst60, LConst04)));
  EXPECT_FALSE(TestSet::of(LConst01, LConst02, LConst60)
    .containsAll(TestSet::of(LConst01, LConst02, LConst60, LConst04)));

  theSet.clear();
  EXPECT_TRUE(theSet.empty());

  theSet.addAll(TestSet::of(LConst06, LConst07, LConst80));
  EXPECT_FALSE(theSet.contains(LConst90));
  theSet.removeAll(TestSet::of(LConst06, LConst07, LConst80));
  EXPECT_TRUE(theSet.empty());
  
  theSet.clear();
  theSet.add(LConst10);
  EXPECT_EQ(theSet, TestSet::of(LConst10));
  EXPECT_NE(theSet, TestSet::of(LConst20));

  theSet.add(LConst20);
  EXPECT_EQ(theSet, TestSet::of(LConst10, LConst20));

  theSet.add(LConst30);
  EXPECT_EQ(theSet, TestSet::of(LConst10, LConst20, LConst30));

  theSet.add(LConst40);
  EXPECT_EQ(theSet, TestSet::of(LConst10, LConst20, LConst30, LConst40));

  theSet.add(LConst50);
  EXPECT_EQ(theSet, TestSet::of(LConst10, LConst20, LConst30, LConst40, LConst50));

  theSet.add(LConst60);
  EXPECT_EQ(theSet, TestSet::of(LConst10, LConst20, LConst30, LConst40,
      LConst50, LConst60));

  theSet.add(LConst70);
  EXPECT_EQ(theSet, TestSet::of(LConst10, LConst20, LConst30, LConst40,
      LConst50, LConst60, LConst70));

  theSet.add(LConst80);
  EXPECT_EQ(theSet, TestSet::of(LConst10, LConst20, LConst30, LConst40,
      LConst50, LConst60, LConst70, LConst80));

  theSet.add(LConst90);
  EXPECT_EQ(theSet, TestSet::of(LConst10, LConst20, LConst30, LConst40,
      LConst50, LConst60, LConst70, LConst80, LConst90));
      
  theSet.clear();
  theSet.addRange(LConst10, LConst50);
  EXPECT_TRUE(theSet.containsAll(
      TestSet::of(LConst10, LConst11, LConst15, LConst20, LConst33, LConst49)));
  EXPECT_FALSE(theSet.contains(LConst50));
  theSet.removeRange(LConst11, LConst49);
  EXPECT_EQ(TestSet::of(LConst10, LConst49), theSet);

  //static SmallEnumSet ofRange(EnumType first, EnumType last) {
  //static SmallEnumSet unionOf(const SmallEnumSet & a, const SmallEnumSet & b) {
}
