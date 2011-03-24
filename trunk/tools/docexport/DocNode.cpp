/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "DocNode.h"

namespace tart {
namespace Doc {

using llvm::dyn_cast;

Doc::Node::~Node() {
  for (NodeList::const_iterator it = children_.begin(); it != children_.end(); ++it) {
    delete *it;
  }
}

}}
