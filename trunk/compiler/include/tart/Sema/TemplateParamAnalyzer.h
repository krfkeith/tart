/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_SEMA_TEMPLATEPARAMNALYZER_H
#define TART_SEMA_TEMPLATEPARAMNALYZER_H

#ifndef TART_SEMA_TYPEANALYZER_H
#include "tart/Sema/TypeAnalyzer.h"
#endif

//#ifndef TART_DEFN_DEFN_H
//#include "tart/Defn/Defn.h"
//#endif
//
//#ifndef TART_TYPE_COMPOSITETYPE_H
//#include "tart/Type/CompositeType.h"
//#endif

namespace tart {

// -------------------------------------------------------------------
// TemplateParamAnalyzer

class TemplateParamAnalyzer : public TypeAnalyzer {
public:
  TemplateParamAnalyzer(Defn * de)
    : TypeAnalyzer(de->module(), de->definingScope())
    , tsig_(de->templateSignature())
  {}

  Type * reduceTypeVariable(const ASTTypeVariable * ast);

private:
  TemplateSignature * tsig_;
};

} // namespace tart

#endif // TART_SEMA_TEMPLATEPARAMNALYZER_H
