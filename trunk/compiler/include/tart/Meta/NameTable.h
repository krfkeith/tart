/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_META_NAMETABLE_H
#define TART_META_NAMETABLE_H

#include "tart/CFG/Type.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>

namespace tart {

class Defn;
class Module;
class ASTDecl;

/// -------------------------------------------------------------------
/// Usage data for a tag.

struct TagInfo {
  long useCount;
  unsigned index;

  TagInfo() : useCount(0), index(0) {}
  TagInfo(int initialUseCount) : useCount(initialUseCount), index(0) {}
};

/// -------------------------------------------------------------------
/// Class used to build the module constants object.

class NameTable {
public:

  /// -------------------------------------------------------------------
  /// Represents a symbol name in the module metadata.

  class Name : public Formattable {
  public:
    /** The unique ID of this name reference. */
    long encodedIndex() const;

    /** The unique ID of this name reference. */
    long index() const { return info_.index; }
    void setIndex(long index) { info_.index = index; }

    /** The number of uses of this name. */
    long useCount() const { return info_.useCount; }
    virtual Name * use() { info_.useCount++; return this; }

    virtual bool isCompound() const { return false; }

  private:
    TagInfo info_;
  };

  /// -------------------------------------------------------------------
  /// A simple name.

  class SimpleName : public Name {
  public:
    SimpleName(const llvm::StringRef value) : value_(value) {}

    /** The text of this name. */
    llvm::StringRef value() const { return value_; }

    // Overrides

    void format(FormatStream & out) const;

  private:
    const llvm::StringRef value_;
  };

  /// -------------------------------------------------------------------
  /// A name that consists of a dotted pair.

  class CompoundName : public Name {
  public:
    typedef std::pair<Name *, Name *> value_type;

    CompoundName(const value_type & value) : value_(value) {}

    Name * first() const { return value_.first; }
    Name * second() const { return value_.second; }
    const value_type & value() const { return value_; }

    // Overrides

    bool isCompound() const { return true; }
    void format(FormatStream & out) const;

  private:
    value_type value_;
  };

  ~NameTable();

  Name * getName(const llvm::StringRef name) const;
  Name * getName(Name * scope, Name * member) const;
  Name * getQualifiedName(llvm::StringRef name) const;

  Name * addName(const llvm::StringRef name);
  Name * addName(Name * scope, Name * member);
  Name * addQualifiedName(const llvm::StringRef name);

  bool empty() const { return simpleNames_.empty(); }

  // Sort all of the names by popularity and assign IDs.
  void assignIndices();

  void writeStringTable(llvm::raw_ostream & out);
  void writeCompoundNameTable(llvm::raw_ostream & out);

private:
  typedef llvm::StringMap<SimpleName *> SimpleNameMap;
  typedef llvm::DenseMap<CompoundName::value_type, CompoundName *> CompoundNameMap;

  typedef std::vector<SimpleName *> SimpleNameArray;
  typedef std::vector<CompoundName *> CompoundNameArray;

  SimpleNameMap simpleNames_;
  CompoundNameMap compoundNames_;

  SimpleNameArray sortedSimpleNames_;
  CompoundNameArray sortedCompoundNames_;
};

} // namespace tart

#endif // TART_META_METADATAWRITER_H
