/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "DocCommentProcessor.h"
#include "tart/Common/SourceFile.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

using namespace Doc;
using namespace llvm;

namespace {

  // Inline markup characters.
  StringRef INLINE_MARKUP("_*'");

  // Characters that are allowed to immediately precede the start of an inline markup span.
  StringRef INLINE_LEADING(" '\"([{<");

  // Characters that are allowed to immediately succeed the end of an inline markup span.
  StringRef INLINE_FOLLOWING(" '\")]}>.,:;!?-/\\");

  // If an inline markup start char is followed by one of these, the end markup must be
  // preceded by the corresponding matching character.
  StringRef MATCHING_START = "'\"([{<";
  StringRef MATCHING_END   = "'\")]}>";

  // Characters that are allowed to immediately succeed the end of an inline markup span.
  StringRef WORD_CHARS("_.");

  inline bool isInlineMarkupChar(char ch) {
    return INLINE_MARKUP.find_first_of(ch) != StringRef::npos;
  }

  inline bool isInlineLeadingChar(char ch) {
    return INLINE_LEADING.find_first_of(ch) != StringRef::npos;
  }

  inline bool isInlineFollowingChar(char ch) {
    return INLINE_FOLLOWING.find_first_of(ch) != StringRef::npos;
  }

  inline bool isWordChar(char ch) {
    return isalnum(ch) or WORD_CHARS.find_first_of(ch) != StringRef::npos;
  }
}

// Represents a recognized document tag or section name.
struct DocTag {
  const char * name;
  void (DocCommentProcessor::*method)(Node * parent);
};

// Table of recognized tags.
DocTag tags[] = {
  { "Author", &DocCommentProcessor::authors },
  { "Authors", &DocCommentProcessor::authors },
  { "Example", &DocCommentProcessor::example },
  { "Exceptions", &DocCommentProcessor::throws },
  { "Note", &DocCommentProcessor::note },
  { "Parameters", &DocCommentProcessor::parameters },
  { "Returns", &DocCommentProcessor::returns },
  { "Section", &DocCommentProcessor::genericSection },
  { "See", &DocCommentProcessor::seeAlso },
  { "See Also", &DocCommentProcessor::seeAlso },
  { "Since", &DocCommentProcessor::since },
  { "Summary", &DocCommentProcessor::summary },
  { "Throws", &DocCommentProcessor::throws },
  { "TODO", &DocCommentProcessor::todo },
  { "Warning", &DocCommentProcessor::warning },
};

Node * DocCommentProcessor::process() {
  splitLines();
  removeBanners();
  trimEmptyLines();
  if (lines_.empty()) {
    return NULL;
  }

  Node * root = new Node(ROOT);
  currentLine_ = lines_.begin();
  currentSection_ = NULL;
  currentPos_= 0;
  parseLines(root, 0);
  return root;
}

void DocCommentProcessor::splitLines() {
  // Break up into lines.
  for (DocComment::const_iterator it = docComment_.begin(); it != docComment_.end(); ++it) {
    const DocComment::Entry * entry = *it;
    StringRef text = entry->text();
    size_t pos = 0;
    int column = tokenPosition(entry->location()).beginCol;
    while (pos < text.size()) {
      Line line;
      pos = breakLine(line, text, pos, column);
      lines_.push_back(line);
      column = 0;
    }
  }
}

size_t DocCommentProcessor::breakLine(Line & line, StringRef text, size_t pos, int column) {
  // Skip leading whitespace
  size_t end = text.size();
  while (pos < end) {
    char ch = text[pos];
    if (ch == ' ') {
      pos++;
      column++;
    } else {
      break;
    }
  }

  // Now find the next newline
  size_t nextLine = text.find('\n', pos);
  size_t lineEnd = nextLine;
  if (nextLine == StringRef::npos) {
    nextLine = lineEnd = end;
  } else {
    nextLine += 1;
  }

  // Trim trailing whitespace
  while (lineEnd > pos) {
    char ch = text[lineEnd - 1];
    if (ch == ' ') {
      --lineEnd;
    } else {
      break;
    }
  }

  line.text = text.slice(pos, lineEnd);
  line.indent = column;
  return nextLine;
}

// Clean up lines consisting of '====' and other decorative junk.
void DocCommentProcessor::removeBanners() {
  // First establish where the left margin is. Find the line that extends leftmost,
  // not counting decorative characters.
  size_t leftMargin = (size_t) -1;
  for (LineList::iterator it = lines_.begin(); it != lines_.end(); ++it) {
    Line & line = *it;
    size_t pos = line.text.find_first_not_of("*=-+ ");
    if (pos != StringRef::npos) {
      leftMargin = std::min(leftMargin, pos + line.indent);
    }
  }

  // Now, remove any characters to the left of the left margin.
  // While we're at it, normalize the indentation of all lines.
  for (LineList::iterator it = lines_.begin(); it != lines_.end(); ++it) {
    Line & line = *it;
    if (size_t(line.indent) < leftMargin) {
      line.text = line.text.substr(leftMargin - line.indent);
      line.indent = 0;
    } else {
      line.indent -= leftMargin;
    }
  }

  // Next, clear any long lines that consist entirely of decorative characters.
  // Short lines are presumed to be intentional.
  for (LineList::iterator it = lines_.begin(); it != lines_.end(); ++it) {
    Line & line = *it;
    if (line.text.size() > 32) {
      size_t pos = line.text.find_first_not_of("*=-+ ");
      if (pos == StringRef::npos) {
        line.text = StringRef();
        line.indent = 0;
      }
    }
  }
}

// Trim empty lines at the beginning and end
void DocCommentProcessor::trimEmptyLines() {
  while (!lines_.empty() && lines_.front().text.size() == 0) {
    lines_.erase(lines_.begin());
  }

  while (!lines_.empty() && lines_.back().text.size() == 0) {
    lines_.erase(lines_.end() - 1);
  }
}

// Parse lines until:
// 1) We get to the end of the the line list
// 2) We reach a point where the indentation is less than that of our parent.
void DocCommentProcessor::parseLines(Node * parent, size_t parentIndent) {
  bool parentIsRoot = parent->type() == ROOT;
  size_t startingIndent = currentLine_->indent;
  while (currentLine_ != lines_.end() && currentLine_->indent >= parentIndent) {
    if (currentLine_->text.empty()) {
      finishParagraph(parent);
      nextLine();
      continue;
    } else if (currentLine_->indent < startingIndent) {
      finishParagraph(parent);
    }

    // See if we can find a section header (only at the root level).
    DocTag * tag = parentIsRoot ? matchDocTag() : NULL;
    if (tag != NULL) {
      finishParagraph(parent);
      (this->*(tag->method))(parent);
    } else if (currentLine_->text.startswith("* ")) {
      finishParagraph(parent);
      unorderedList(parent);
    } else if (currentLine_->text.startswith("# ")) {
      finishParagraph(parent);
      orderedList(parent);
    } else if (currentLine_->text.startswith("{{{")) {
      finishParagraph(parent);
      codeBlock(parent);
    } else if (currentLine_->indent > startingIndent) {
      finishParagraph(parent);
      blockQuote(parent);
    } else {
      // Append the text of this line onto the current paragraph, separated by space.
      if (!paragraphText_.empty()) {
        paragraphText_.push_back(' ');
      }
      paragraphText_.append(currentLine_->text.begin(), currentLine_->text.end());
      nextLine();
    }
  }
  finishParagraph(parent);
}

bool DocCommentProcessor::definition(Node * parent, NodeType nt) {
  // Looking for a line that starts with "word: " or "word - ".
  size_t wordSize = parseWord();
  if (wordSize == 0) {
    nextLine();
    return false;
  }

  StringRef wordText = currentLine_->text.substr(currentPos_, wordSize);
  DefinitionNode * defnNode = new DefinitionNode(nt, wordText);
  parent->append(defnNode);
  currentPos_ += wordSize;
  size_t end = currentLine_->text.size();
  if (currentPos_ + 1 < end &&
      currentLine_->text[currentPos_] == ':' &&
      currentLine_->text[currentPos_+1] == ' ') {
    currentPos_ += 1;
    skipWS();
  } else if (skipWS() &&
      currentPos_ < end &&
      currentLine_->text[currentPos_] == '-') {
    currentPos_ += 1;
    skipWS();
  } else {
    nextLine();
    return false;
  }

  skipWS();

  // Append any characters remaining on the starting line
  paragraphText_.append(currentLine_->text.begin() + currentPos_, currentLine_->text.end());
  size_t indent = currentLine_->indent + 1;
  nextLine();

  // Gather any additional lines from the hanging indent.
  parseLines(defnNode, indent);
  return true;
}

DocTag * DocCommentProcessor::matchDocTag() {
  size_t end = currentLine_->text.size();
  for (size_t i = 0; i < sizeof(tags) / sizeof(tags[0]); ++i) {
    DocTag * tag = &tags[i];
    if (currentLine_->text.startswith(tag->name)) {
      currentPos_ = strlen(tag->name);
      // Doc tag must end with a colon and then either whitespace or end of line.
      if (currentPos_ < end && currentLine_->text[currentPos_] == ':') {
        currentPos_++;
        if (currentPos_ == end || currentLine_->text[currentPos_] == ' ') {
          // Skip any whitespace after the doc tag.
          skipWS();
          return tag;
        }
      }
    }
  }

  return NULL;
}

void DocCommentProcessor::finishParagraph(Node * parent) {
  if (!paragraphText_.empty()) {
    if (parent->type() == ROOT) {
      if (currentSection_ == NULL) {
        currentSection_ = new SectionNode(DESCRIPTION);
        parent->append(currentSection_);
      }
      createParagraph(currentSection_);
    } else {
      createParagraph(parent);
    }
  }
}

void DocCommentProcessor::createParagraph(Doc::Node * parent) {
  if (!paragraphText_.empty()) {
    Node * paragraph = new Node(PARAGRAPH);
    parent->append(paragraph);

    StringRef::const_iterator pos = paragraphText_.begin();
    StringRef::const_iterator end = paragraphText_.end();
    StringRef::const_iterator prevPos = pos;
    char lastCh = ' ';

    // Check if this paragraph ends with "word::".
    bool isIntroducingCodeBlock = false;
    if (paragraphText_.size() >= 3 &&
        end[-1] == ':' &&
        end[-2] == ':' &&
        end[-3] != ':' && end[-3] != ' ') {
      paragraphText_.pop_back();
      isIntroducingCodeBlock = true;
    }

    while (pos < end) {
      char ch = *pos++;
      if (isInlineMarkupChar(ch)) {
        // Inline markup. The markup char must be preceded by an allowable leading char,
        // and must be followed by non-whitespace.
        if (isInlineLeadingChar(lastCh) && pos < end && *pos != ' ' && *pos != ch) {
          char markupCh = ch;

          // If the following char is one in MATCHING_START, it must be matched by the
          // corresponding one in MATCHING_END
          size_t matchDelim = MATCHING_START.find_first_of(*pos);
          bool isDelim = matchDelim != StringRef::npos;
          char matchDelimCh = isDelim ? MATCHING_END[matchDelim] : 0;

          // Lookahead to find the end of the span.
          StringRef::const_iterator la = pos;
          bool foundEnd = false;
          lastCh = ch;
          while (la < end) {
            // It must be the same markup char, not preceded by a space, and followed
            // by an allowable trailing char.
            char ch = *la++;
            if (ch == markupCh &&
               (la == end || isInlineFollowingChar(*la)) &&
                (isDelim ? lastCh == matchDelimCh : lastCh != ' ')) {
              foundEnd = true;
              break;
            }
            lastCh = ch;
          }

          if (foundEnd) {
            size_t prevSize = pos - 1 - prevPos;
            if (prevSize > 0) {
              // First take the characters before the run.
              paragraph->append(new TextNode(StringRef(prevPos, prevSize)));
            }

            Style st = STYLE_SYMBOL;
            if (markupCh == '_') {
              st = STYLE_EMPHATIC;
            } else if (markupCh == '*') {
              st = STYLE_STRONG;
            }

            // Now take the characters inside the span.
            TextNode * text = new TextNode(StringRef(pos, la - pos - 1));
            StyleNode * sn = new StyleNode(st);
            sn->append(text);
            paragraph->append(sn);
            prevPos = pos = la;
            continue;
          }

          // Otherwise, ignore the markup charaxcter, since we didn't find the end.
        }
      } else if (ch == '{') { // Look for inline code block.
      }

      lastCh = ch;
    }

    // Handle any remaining text at the end.
    if (pos > prevPos) {
      paragraph->append(new TextNode(StringRef(prevPos, pos - prevPos)));
    }

    // Ready for a new paragraph.
    paragraphText_.clear();
  }
}

void DocCommentProcessor::authors(Node * parent) {
  size_t end = currentLine_->text.size();
  Node * result = NULL;
  if (currentPos_ < end) {
    StringRef authorName = currentLine_->text.substr(currentPos_);
    parent->append(new PropertyNode("author", authorName));
    nextLine();
  } else {
    size_t startingIndent = currentLine_->indent;
    nextLine();
    // Author names, one per line
    while (currentLine_ != lines_.end() && currentLine_->indent > startingIndent) {
      parent->append(new PropertyNode("author", currentLine_->text));
      nextLine();
    }
  }
}

void DocCommentProcessor::example(Node * parent) {
  diag.debug(location()) << currentLine_->text;
  DFAIL("Implement");
  nextLine();
}

void DocCommentProcessor::note(Node * parent) {
  diag.debug(location()) << currentLine_->text;
  DFAIL("Implement");
  nextLine();
}

void DocCommentProcessor::parameters(Node * parent) {
  size_t end = currentLine_->text.size();
  if (currentPos_ < end) {
    diag.warn(location()) << "Extra characters after Parameters tag";
  }
  size_t indent = currentLine_->indent;
  nextLine();
  while (currentLine_ != lines_.end() && currentLine_->indent > indent) {
    if (!definition(parent, PARAMETER)) {
      diag.warn(location()) << "Parameter definition not found, line skipped";
    }
  }
}

void DocCommentProcessor::returns(Node * parent) {
  Node * retNode = new Node(RETURNS);
  parent->append(retNode);

  // Append any characters remaining on the starting line
  paragraphText_.append(currentLine_->text.begin() + currentPos_, currentLine_->text.end());
  size_t indent = currentLine_->indent + 1;
  nextLine();

  // Gather any additional lines from the hanging indent.
  parseLines(retNode, indent);
}

void DocCommentProcessor::genericSection(Node * parent) {
  diag.debug(location()) << currentLine_->text;
  DFAIL("Implement");
  nextLine();
}

void DocCommentProcessor::seeAlso(Node * parent) {
  diag.debug(location()) << currentLine_->text;
  DFAIL("Implement");
  nextLine();
}

void DocCommentProcessor::since(Node * parent) {
  size_t end = currentLine_->text.size();
  Node * result = NULL;
  if (currentPos_ < end) {
    StringRef authorName = currentLine_->text.substr(currentPos_);
    parent->append(new PropertyNode("since", authorName));
    nextLine();
  } else {
    size_t startingIndent = currentLine_->indent;
    nextLine();
    // Since dates, one per line
    // Not really legal to have more than one, but we're not validating.
    while (currentLine_ != lines_.end() && currentLine_->indent > startingIndent) {
      parent->append(new PropertyNode("since", currentLine_->text));
      nextLine();
    }
  }
}

void DocCommentProcessor::summary(Node * parent) {
  diag.debug(location()) << currentLine_->text;
  DFAIL("Implement");
  nextLine();
}

void DocCommentProcessor::throws(Node * parent) {
  size_t end = currentLine_->text.size();
  if (currentPos_ < end) {
    diag.warn(location()) << "Extra characters after section tag";
  }
  size_t indent = currentLine_->indent;
  nextLine();
  while (currentLine_ != lines_.end() && currentLine_->indent > indent) {
    if (!definition(parent, EXCEPTION)) {
      diag.warn(location()) << "Throws definition not found, line skipped";
    }
  }
}

void DocCommentProcessor::todo(Node * parent) {
  diag.debug(location()) << currentLine_->text;
  DFAIL("Implement");
  nextLine();
}

void DocCommentProcessor::warning(Node * parent) {
  diag.debug(location()) << currentLine_->text;
  DFAIL("Implement");
  nextLine();
}

void DocCommentProcessor::unorderedList(Doc::Node * parent) {
  diag.warn() << "UL Unimplemented";
  nextLine();
}

void DocCommentProcessor::orderedList(Doc::Node * parent) {
  diag.warn() << "OL Unimplemented";
  nextLine();
}

void DocCommentProcessor::codeBlock(Doc::Node * parent) {
  diag.debug(location()) << currentLine_->text;
  DFAIL("Implement");
  nextLine();
}

void DocCommentProcessor::blockQuote(Doc::Node * parent) {
  Node * node = new Node(BLOCKQUOTE);
  parent->append(node);
  size_t indent = currentLine_->indent;
  parseLines(node, indent);
}

void DocCommentProcessor::nextLine() {
  ++currentLine_;
  currentPos_= 0;
}

// Find a word and return it's length, or return 0 if none found.
size_t DocCommentProcessor::parseWord() {
  size_t end = currentLine_->text.size();
  size_t pos = currentPos_;
  while (pos < end && isWordChar(charAt(pos))) {
    pos = nextChar(pos);
  }
  return pos - currentPos_;
}

int DocCommentProcessor::charAt(size_t pos) {
  // TODO: UTF-8 encoding
  return currentLine_->text[pos];
}

size_t DocCommentProcessor::nextChar(size_t pos) {
  // TODO: UTF-8 encoding
  return pos + 1;
}

size_t DocCommentProcessor::skipWS() {
  size_t end = currentLine_->text.size();
  size_t pos = currentPos_;
  while (currentPos_ < end && currentLine_->text[currentPos_] == ' ') {
    currentPos_++;
  }
  return currentPos_ - pos;
}

SourceLocation DocCommentProcessor::location() {
  return (*docComment_.begin())->location();
}

}
