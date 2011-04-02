/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/AST/ASTDecl.h"
#include "tart/Defn/Module.h"
#include "tart/Objects/Builtins.h"
#include "tart/Sema/DefnAnalyzer.h"
#include "tart/Common/Diagnostics.h"
#include "FakeSourceFile.h"

using namespace tart;

class DefnAnalyzerTest : public testing::Test {
protected:
  Module * testModule;
  DefnAnalyzer declAnalyzer;

public:
  DefnAnalyzerTest()
    : testModule(new Module("test", &Builtins::module))
    , declAnalyzer(testModule, testModule, testModule, NULL)
  {
    testModule->setModuleSource(new FakeSourceFile(""));
  }

  virtual void SetUp() {}
  virtual void TearDown() {}

  ASTTypeDecl * createTestClassDecl(const char * name) {
    return new ASTTypeDecl(ASTNode::Class, SourceLocation(), name, ASTNodeList(),
        DeclModifiers());
  }

  //bool hasMember(Defn * de, const char * name) {
  //  DefnList defs;
  //  return de->lookupMember(name, defs, false);
  //}
};

#if 0
TEST_F(DefnAnalyzerTest, TestAddMember) {
  ASTTypeDecl * cl1 = createTestClassDecl("cl1");
  TypeDefn * cl1Defn = declAnalyzer.createCompositeType(Defn::Class, cl1);

  ASSERT_EQ(Defn::Class, cl1Defn->defnType());
  ASSERT_EQ("cl1", cl1Defn->name());
  ASSERT_EQ(&testModule, cl1Defn->module());
  ASSERT_EQ(&testModule, cl1Defn->getParentScope());
  ASSERT_EQ(&testModule, cl1Defn->getParentDefn());
  ASSERT_EQ("test.cl1", cl1Defn->qualifiedName());
  ASSERT_EQ(cl1Defn, testModule.lookupMember("cl1", false));

  DefnList defs;
  bool success = testModule.lookupMember("cl1", defs, false);
  ASSERT_TRUE(success);
  ASSERT_EQ(1u, defs.size());
  ASSERT_EQ(cl1Defn, defs[0]);
}

TEST_F(DefnAnalyzerTest, TestNameConflict) {

  ASTTypeDecl * cl1 = createTestClassDecl("cl1");
  ASTTypeDecl * cl2 = createTestClassDecl("cl2");
  ASTTypeDecl * cl3 = createTestClassDecl("cl2");

  // Adding two classes with dissimilar names should not produce an error
  TypeDefn * cl1Defn = declAnalyzer.createCompositeType(Defn::Class, cl1);
  TypeDefn * cl2Defn = declAnalyzer.createCompositeType(Defn::Class, cl2);
  EXPECT_EQ(0, diag.getErrorCount());

  // But adding a class with the same name should produce an error
  diag.setMinSeverity(Diagnostics::Off);
  TypeDefn * cl3Defn = declAnalyzer.createCompositeType(Defn::Class, cl3);
  EXPECT_EQ(1, diag.getErrorCount());
  diag.reset();
  diag.setMinSeverity(Diagnostics::Debug);

  // TODO: Add test with overloaded function names
}

TEST_F(DefnAnalyzerTest, CreateMembers) {
  ASTTypeDecl * cldecl = createTestClassDecl("cl");
  ASTTypeDecl * innerdecl = createTestClassDecl("inner");

  TypeDefn * cl = declAnalyzer.createCompositeType(Defn::Class, cldecl);
  DefnAnalyzer da(cl);

  cldecl->addMember(innerdecl);

  ASSERT_FALSE(cl->isPhaseComplete(CreateMembers));
  ASSERT_FALSE(cl->isPhaseInProgress(CreateMembers));
  ASSERT_FALSE(hasMember(cl, "inner"));
  da.createMembers();
  ASSERT_TRUE(cl->isPhaseComplete(CreateMembers));
  ASSERT_FALSE(cl->isPhaseInProgress(CreateMembers));
  ASSERT_TRUE(hasMember(cl, "inner"));
}
#endif
