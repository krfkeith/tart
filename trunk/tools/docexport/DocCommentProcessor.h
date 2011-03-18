/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DOCCOMMENTPROCESSOR_H
#define TART_DOCCOMMENTPROCESSOR_H

#ifndef TART_AST_DOCCOMMENT_H
#include "tart/AST/DocComment.h"
#endif

#ifndef TART_DOCNODE_H
#include "DocNode.h"
#endif

#include "llvm/ADT/SmallVector.h"

namespace tart {

/// -------------------------------------------------------------------
/// Writes out the contents of a module as XML.
class DocCommentProcessor {
public:
  Doc::Node * processDocComment(const DocComment & dc);

protected:
  void generate(Module * mod);

private:
};

} // namespace tart

#endif // DOCEXPORTER
