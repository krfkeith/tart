/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_XMLWRITER_H
#define TART_XMLWRITER_H

#include "llvm/Support/raw_ostream.h"

namespace tart {

/// -------------------------------------------------------------------
/// A class which makes it easy to emit correct XML.
class XmlWriter {
public:
  XmlWriter(llvm::raw_ostream & strm, bool formatPretty = true)
    : strm_(strm)
    , formatPretty_(formatPretty)
    , indentSpaces_(2)
    , state_(CHARACTERS)
    , simpleElement_(false)
    , indentLevel_(0)
  {}

  /** Return the underlying stream object. */
  llvm::raw_ostream & strm() { return strm_; }

  /** Set the number of indent spaces for pretty format. The default is 2. */
  XmlWriter & setIndentSpaces(int32_t spaces) {
    indentSpaces_ = spaces;
    return *this;
  }

  /** Write the XML prologue. */
  XmlWriter & writeXmlPrologue();

  /** Begin a new element. */
  XmlWriter & beginElement(llvm::StringRef elName, bool newline = true);

  /** End the current element. */
  XmlWriter & endElement(llvm::StringRef elName, bool newline = true);

  /** Begin a new processing instruction. */
  XmlWriter & beginProcessingInstruction(llvm::StringRef psName);

  /** End the current processing instruction. */
  XmlWriter & endProcessingInstruction();

  /** Append an attribute to the current element or processing instruction, assuming
      that it has not already been closed. This should be done before any character
      data or child elements are written. XML special characters in the attribute
      value will be escaped as entities. */
  XmlWriter & appendAttribute(llvm::StringRef name, llvm::StringRef value);

  /** Write a string of character data. XML special characters will be escaped as entities. */
  XmlWriter & writeCharacterData(llvm::StringRef data);

  /** Overload the stream operator to write character data as well. */
  XmlWriter & operator<<(llvm::StringRef data) {
    writeCharacterData(data);
    return * this;
  }

  /** Write a string of unencoded character data. Consecutive calls will be merged into
      a single CDATA block. */
  XmlWriter & writeCDATA(llvm::StringRef data);

  /** Write a comment. For compatibility with the XML standard, a run of dashes will be
      converted into a single dash. No other transformation is performed. */
  XmlWriter & writeComment(llvm::StringRef data);

private:
  enum State {
    CHARACTERS,
    ELEMENT,
    PI,
    CDATA
  };

  llvm::raw_ostream & strm_;
  bool formatPretty_;
  int32_t indentSpaces_;
  State state_;
  bool simpleElement_;
  int32_t indentLevel_;

  void closeCurrentElement(bool newline = true);
  void writeIndent();
  void writeEscapedString(llvm::StringRef text);
};

}

#endif // TART_XMLWRITER_H
