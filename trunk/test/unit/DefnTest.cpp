/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>
#include "tart/AST/ASTDecl.h"
#include "tart/CFG/NamespaceDefn.h"
#include "tart/CFG/VariableDefn.h"

using namespace tart;
using llvm::isa;

TEST(DefnTest, Casting) {
  EXPECT_FALSE(isa<ValueDefn>(new NamespaceDefn(NULL, "test")));
  EXPECT_TRUE(isa<ValueDefn>(new VariableDefn(Defn::Var, NULL, "test")));
  EXPECT_TRUE(isa<ValueDefn>(new VariableDefn(Defn::Let, NULL, "test")));
}

