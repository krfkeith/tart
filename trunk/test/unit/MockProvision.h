/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include <gmock/gmock.h>

#include "tart/Sema/Infer/Provision.h"

namespace tart {

class MockProvision : public Provision {
public:
  MOCK_CONST_METHOD0(check, bool());
  MOCK_CONST_METHOD1(implies, bool(const Provision *));
  MOCK_CONST_METHOD1(contradicts, bool(const Provision *));
  MOCK_CONST_METHOD1(isType, bool(uint32_t));

  void trace() const {}
  void format(FormatStream & out) const {};
};

}  // namespace
