/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_ENUMANALYZER_H
#define TART_SEMA_ENUMANALYZER_H

#ifndef TART_SEMA_DEFNANALYZER_H
#include "tart/Sema/DefnAnalyzer.h"
#endif

#ifndef TART_CFG_ENUMTYPE_H
#include "tart/CFG/EnumType.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Analyzer for enumerations.
class EnumAnalyzer : public DefnAnalyzer {
private:
  TypeDefn * target_;
  ConstantInteger * prevValue_;
  ConstantInteger * minValue_;
  ConstantInteger * maxValue_;
  Type * intValueType_;

public:
  /** Constructor. */
  EnumAnalyzer(TypeDefn * target);

  /** Fully analyze the input defn and all of its descendants. */
  bool analyze();
  bool runPasses(EnumType::PassSet passesToRun);
  bool createMembers();
  bool createEnumConstant(const ASTVarDecl * ast);
  void defineOperators();
};

}

#endif
