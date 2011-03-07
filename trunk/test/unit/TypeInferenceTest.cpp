/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gtest/gtest.h>

#include "tart/Sema/BindingEnv.h"
#include "tart/Sema/CallCandidate.h"

#include "tart/Expr/Exprs.h"
#include "tart/Type/StaticType.h"

#include "TestHelpers.h"

namespace {

using namespace tart;

class TypeInferenceTest : public testing::Test {
protected:

  CallExpr * createCall() {
    return new CallExpr(Expr::Call, SourceLocation(), NULL);
  }

};

TEST_F(TypeInferenceTest, CallCandidateSpecificity) {
  CallExpr * call = createCall();
  //call->candidates().push_back(new CallCandidate(call, NULL, method, pa));


//CallCandidate::CallCandidate(CallExpr * call, Expr * baseExpr, FunctionDefn * m,
//    const ParameterAssignments & params)

}

}
