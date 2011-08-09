/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_AST_ASTNODE_H
#include "tart/AST/ASTNode.h"
#endif

#ifndef TART_TYPE_TYPE_H
#include "tart/Type/Type.h"
#endif

#ifndef LLVM_ADT_STRINGREF_H
#include "llvm/ADT/StringRef.h"
#endif

#include <ostream>
#include <sstream>

namespace tart {
  // Make nodes streamable so they can be printed in test results.
  inline std::ostream & operator<<(std::ostream & out, const ASTNode * node) {
    OsFormatStream stream(out);
    node->format(stream);
    return out;
  }

  // Make node types streamable
  inline std::ostream & operator<<(std::ostream & out, const ASTNode::NodeType nt) {
    out << nodeTypeName(nt);
    return out;
  }

  // Make node types streamable
  inline std::ostream & operator<<(std::ostream & out, StringRef str) {
    out << str.str();
    return out;
  }

  // Compare an AST node with its string representation
  inline bool ASTCmp(llvm::StringRef expected, const ASTNode * actual) {
    if (actual != NULL) {
      StrFormatStream stream;
      actual->format(stream);
      stream.flush();
      return stream.str() == expected;
    }

    return false;
  }

  #define ASSERT_AST_EQ(expected, actual) \
    ASSERT_PRED2(ASTCmp, expected, actual);

  #define EXPECT_AST_EQ(expected, actual) \
    EXPECT_PRED2(ASTCmp, expected, actual);

  // Make types streamable so they can be printed in test results.
  inline std::ostream & operator<<(std::ostream & out, const Type * node) {
    OsFormatStream stream(out);
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
