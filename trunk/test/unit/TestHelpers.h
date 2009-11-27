/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

#ifndef TART_CFG_TYPE_H
#include "tart/CFG/Type.h"
#endif

#include <ostream>
#include <sstream>

namespace tart {
  // Make nodes streamable so they can be printed in test results.
  inline std::ostream & operator<<(std::ostream & out, const ASTNode * node) {
    FormatStream stream(out);
    node->format(stream);
    return out;
  }

  // Make node types streamable
  inline std::ostream & operator<<(std::ostream & out, const ASTNode::NodeType nt) {
    out << nodeTypeName(nt);
    return out;
  }

  // Compare an AST node with its string representation
  inline bool ASTCmp(const char * expected, const ASTNode * actual) {
    if (actual != NULL) {
      std::stringstream strm;
      FormatStream stream(strm);
      actual->format(stream);
      stream.flush();
      return strm.str() == expected;
    }

    return false;
  }

  #define ASSERT_AST_EQ(expected, actual) \
    ASSERT_PRED2(ASTCmp, expected, actual);

  #define EXPECT_AST_EQ(expected, actual) \
    EXPECT_PRED2(ASTCmp, expected, actual);

  // Make types streamable so they can be printed in test results.
  inline std::ostream & operator<<(std::ostream & out, const Type * node) {
    FormatStream stream(out);
    node->format(stream);
    return out;
  }

  // Make conversion ranks streamable
  inline std::ostream & operator<<(std::ostream & out, const ConversionRank rank) {
    switch (rank) {
      case Incompatible: {
        out << "Incompatible";
        break;
      }

      case Truncation: {
        out << "Truncation";
        break;
      }

      case IntegerToBool: {
        out << "IntegerToBool";
        break;
      }

      case SignedUnsigned: {
        out << "SignedUnsigned";
        break;
      }

      case PrecisionLoss: {
        out << "PrecisionLoss";
        break;
      }

      case NonPreferred: {
        out << "NonPreferred";
        break;
      }

      case ExactConversion: {
        out << "ExactConversion";
        break;
      }

      case IdenticalTypes: {
        out << "IdenticalTypes";
        break;
      }
    }

    return out;
  }
}
