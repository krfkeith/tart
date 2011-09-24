/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_META_TAGS_H
#define TART_META_TAGS_H

#ifndef TART_COMMON_FORMATTABLE_H
#include "tart/Common/Formattable.h"
#endif

#define TART_OBJECT_FORMAT_VERSION 0

#ifdef DEFTAG_AST
#undef DEFTAG_AST
#endif

namespace tart {
namespace meta {
namespace Defn {

  /// -------------------------------------------------------------------
  /// Tag definitions for serialized definitions.
  enum Tag {
    INVALID = 0,      // Invalid tag
    CLASS,            // A class definition
    STRUCT,           // A struct definition
    INTERFACE,        // An interface definition
    PROTOCOL,         // A protocol definition
    ENUM,             // An enum definition
    ECONST,           // An enum constant
    TYPEALIAS,        // A protocol definition
    NAMESPACE,        // A namespace definition
    FUNCTION,         // A function definition
    UNDEF,            // A method un-definition
    OVERRIDE,         // A method override
    CONSTRUCTOR,      // A constructor definition
    MACRO,            // A macro definition
    INDEXER,          // An indexer definition
    PROPERTY,         // A property definition
    VARIABLE,         // A variable definition
    LET,              // An immutable constant
    IMPORT,           // An import definition
    PARAM,            // A parameter definition within a method
    TYPE_PARAM,       // A template parameter definition
  };
}

namespace Detail {

  /// -------------------------------------------------------------------
  /// Tag definitions for additional details of a declaration.
  enum Tag {
    INVALID = 0,        // Invalid tag
    BASE_TYPE,          // Base class info
    ATTRIBUTE,          // Information about an atttribute
  };
}

namespace DefnFlag {

  /// -------------------------------------------------------------------
  /// Tag definitions for serialized definition flags.
  enum Tag {
    STATIC       = (1<<0),
    ABSTRACT     = (1<<1),
    FINAL        = (1<<2),
    UNSAFE       = (1<<3),   // @Unsafe attribute
    PROTECTED    = (1<<4),
    PRIVATE      = (1<<5),
    INTERNAL     = (1<<6),
    CONSTANT     = (1<<7),   // Unused
    EXTERN       = (1<<8),   // @Extern attribute
    READONLY     = (1<<9),
    VARIADIC     = (1<<10),
    KEYWORDONLY  = (1<<11),
    TEMPLATE     = (1<<12),
    ATTRIBUTE    = (1<<13),  // @Attribute attribute
    FLAGS_ENUM   = (1<<14),  // @Flags attribute
    REFLECTED    = (1<<15),  // @Reflect attribute
    INTRINSIC    = (1<<16),  // @Intrinsic attribute
    ENTRY_POINT  = (1<<17),  // @EntryPoint attribute
    TRACE_METHOD = (1<<18),  // @TraceMethod attribute
  };
}

namespace ExprID {

  /// -------------------------------------------------------------------
  /// Tag definitions for serialized expressions.
  enum Tag {
    NONE = 0,
    CONST_INT,
    CONST_STR,
    CONST_OBJ,
    CONST_LVAL,
    UPCAST,
  };
}

namespace AST {

  /// -------------------------------------------------------------------
  /// Tag definitions for serialized ASTs.
  enum Tag {
    #define DEFTAG_AST(x) x,
    #include "ASTTags.def"
    #undef DEFTAG_AST
    #define DEFTAG_AST(x)
    COUNT,
  };

  /// String name of a tag
  const char * str(Tag tag);
}

} // namespace meta

/** Stream operator for AST tag names. */
inline FormatStream & operator<<(FormatStream & out, meta::AST::Tag tag) {
  return out << meta::AST::str(tag);
}

/** Stream operator for AST tag names. */
inline llvm::raw_ostream & operator<<(llvm::raw_ostream & out, meta::AST::Tag tag) {
  return out << meta::AST::str(tag);
}

} // namespace tart

#endif // TART_META_TAGS_H
