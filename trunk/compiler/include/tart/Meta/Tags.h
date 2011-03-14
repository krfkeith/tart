/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_META_TAGS_H
#define TART_META_TAGS_H

namespace tart {

/// -------------------------------------------------------------------
/// Tag definitions for module symbols.

enum NameTag {
  // The high bit of the first byte indicates whether the symbol is
  // a simple or compound symbol. A simple name tag contains an
  // index into the string table for the module. A compound name tag
  // contains a pair of indices into the name table, representing
  // a dotted pair of names.
  COMPOUND_NAME_FLAG = 0x01,
};

/// -------------------------------------------------------------------
/// Tag definitions for reflected definitions.

enum MemberTag {

  // Tags representing member declarations. Declarations are always followed
  // by the index of their name (variable-length encoded), any members
  // or modifiers, and ending with the end-of-scope tag.

  TAG_DEF_INVALID = 0,      // Invalid tag
  TAG_DEF_CLASS,            // A class definition
  TAG_DEF_STRUCT,           // A struct definition
  TAG_DEF_INTERFACE,        // An interface definition
  TAG_DEF_PROTOCOL,         // A protocol definition
  TAG_DEF_ENUM,             // An enum definition
  TAG_DEF_TYPEALIAS,        // A protocol definition
  TAG_DEF_NAMESPACE,        // A namespace definition
  TAG_DEF_FUNCTION,         // A function definition
  TAG_DEF_UNDEF,            // A method un-definition
  TAG_DEF_OVERRIDE,         // A method override
  TAG_DEF_CONSTRUCTOR,      // A constructor definition
  TAG_DEF_MACRO,            // A macro definition
  TAG_DEF_INDEXER,          // An indexer definition
  TAG_DEF_PROPERTY,         // A property definition
  TAG_DEF_VARIABLE,         // A variable definition
  TAG_DEF_LET,              // An immutable constant
  TAG_DEF_IMPORT,           // An import definition
  TAG_DEF_PARAM,            // A parameter definition within a method
  TAG_DEF_TYPE_PARAM,       // A template parameter definition
  TAG_DEF_ATTRIBUTE,        // An attribute (applies to current scope).
  TAG_DEF_TEMPLATE_INST,    // A template instantiation


  // Modifier tags which affect the containing scope.

  TAG_DEFMOD_ATTRIBUTE = 32,// Modifies a declaration to add an attribute.
//  TAG_DEFMOD_STATIC,        // Modifies a declaration to have static storage class
//  TAG_DEFMOD_FINAL,         // Modifies a class or method to be final
//  TAG_DEFMOD_ABSTRACT,      // Modifies a class or method to be abstract
  TAG_DEFMOD_VARIADIC,      // Modifies a parameter to be variadic
  TAG_DEFMOD_KEYWORD_ONLY,  // Modifies a parameter to be keyword-only

  // Upper bits representing visibility.

  TAG_DEFFLAG_PRIVATE = 0x80,   // A private declaration
  TAG_DEFFLAG_PROTECTED = 0x40, // A protected declaration
};

/// -------------------------------------------------------------------
/// Tag definitions for defn flags.

enum DefnFlag {
  DEFNFLAG_STATIC       = (1<<0),
  DEFNFLAG_ABSTRACT     = (1<<1),
  DEFNFLAG_FINAL        = (1<<2),
  DEFNFLAG_UNSAFE       = (1<<3),
  DEFNFLAG_PROTECTED    = (1<<4),
  DEFNFLAG_PRIVATE      = (1<<5),
  DEFNFLAG_INTERNAL     = (1<<6),
  DEFNFLAG_CONSTANT     = (1<<7),   // Unused
  DEFNFLAG_EXTERN       = (1<<8),
  DEFNFLAG_READONLY     = (1<<9),
  DEFNFLAG_VARIADIC     = (1<<10),
  DEFNFLAG_KEYWORDONLY  = (1<<11),
};

/// -------------------------------------------------------------------
/// Tag definitions for module types.

enum TypeTag {

  // Primitive types

  TAG_TYPE_VOID = 0,
  TAG_TYPE_BOOL,
  TAG_TYPE_CHAR,
  TAG_TYPE_INT8,
  TAG_TYPE_INT16,
  TAG_TYPE_INT32,
  TAG_TYPE_INT64,
  TAG_TYPE_UINT8,
  TAG_TYPE_UINT16,
  TAG_TYPE_UINT32,
  TAG_TYPE_UINT64,
  TAG_TYPE_FLOAT,
  TAG_TYPE_DOUBLE,
  TAG_TYPE_NULL,
  TAG_TYPE_UNSIZED_INT,

  // Derived types

  TAG_TYPE_FUNCTION = 16,   // Function type (flags, return-type, params...)
  TAG_TYPE_FUNCTION_STATIC, // Function with no 'self' param.
  TAG_TYPE_BOUND_METHOD,    // Bound method (flags, return-type, params...)
  TAG_TYPE_TUPLE,           // Tuple table index follows
  TAG_TYPE_UNION,           // Tuple table index follows
  TAG_TYPE_UNIT,            // ??
  TAG_TYPE_NADDRESS,        // Type table index follows
  TAG_TYPE_NARRAY,          // Type table index and length follows
  TAG_TYPE_TYPEVAR,         // variable name index follows (? should be index ?)
  TAG_TYPE_TYPELITERAL,     // Literal reference to a type
  TAG_TYPE_VARIADIC,        // Type follows

  // Indirect type tags - tag byte is followed by varint table index.

  TAG_TYPE_COMPOSITE = 32,   // Followed by index into composite type table.
  TAG_TYPE_DERIVED = 33,     // Followed by index into derived type table.
  TAG_TYPE_ENUM = 34,        // Followed by index into enum type table.

  // Immediate-mode tags, tag data is in low bits of tag byte.

  TAG_TYPE_ENUM_IMM = 48,       // Low 4 bits are index into enum type table.
  TAG_TYPE_COMPOSITE_IMM = 64,  // Low 6 bits are index into composite type table.
  TAG_TYPE_DERIVED_IMM = 128,   // Low 7 bits are index into derived type table.
};

} // namespace tart

#endif // TART_META_TAGS_H
