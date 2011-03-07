/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/AST/ASTDecl.h"
#include "tart/Defn/VariableDefn.h"
#include "llvm/Support/Format.h"

using namespace tart;

TEST(ScopeTest, EmptyScope) {
  SymbolTable   testScope;

  ASSERT_EQ(0u, testScope.count());
}

TEST(ScopeTest, IterableScope) {
  SymbolTable   testScope;

  VariableDefn var0(VariableDefn::Var, NULL, "var0");
  testScope.add(&var0);
  ASSERT_EQ(&var0, testScope.findSymbol(var0.name())->front());
  ASSERT_EQ(1u, testScope.count());
}

TEST(ScopeTest, LargeScope) {
  SymbolTable   testScope;
  char * names[32];
  Defn * decls[32];

  for (int i = 0; i < 32; ++i) {
    char namebuf[32];
    size_t len = snprintf(namebuf, 32, "name_%d", i);
    names[i] = new char[len+1];
    memcpy(names[i], namebuf, len+1);
    VariableDefn * v =
        new VariableDefn(VariableDefn::Var, NULL, names[i]);
    decls[i] = v;
    testScope.add(decls[i]);
    ASSERT_EQ(decls[i], testScope.findSymbol(decls[i]->name())->front());
  }

  for (int i = 0; i < 32; ++i) {
    ASSERT_EQ(decls[i], testScope.findSymbol(decls[i]->name())->front());
  }

  ASSERT_EQ(32u, testScope.count());
}

#if 0
TEST(ScopeTest, BlockScope) {
  LocalScope   parentScope(NULL);
  LocalScope   childScope(&parentScope);

  VariableDefn pvar(SourceLocation(), "pvar", NULL, NULL, Storage_Global);
  parentScope.addMember(&pvar);

  VariableDefn cvar(SourceLocation(), "cvar", NULL, NULL, Storage_Global);
  childScope.addMember(&cvar);

  // TODO: Redo this test.
  //ASSERT_EQ(&pvar, childScope.findMember(pvar.name())->front());
  //ASSERT_EQ(&cvar, childScope.findMember(cvar.name())->front());
}
#endif
