/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/SourceFile.h"
#include "tart/Common/Diagnostics.h"

#include "llvm/Support/raw_ostream.h"

#include <ostream>
#include <iostream>
#include <fstream>
#include <algorithm>

namespace tart {

namespace {
  // A 4095 character line, 100k lines per file ought to be long enough
  const uint32_t COL_BITS = 12;
  const uint32_t MAX_COL_NUM = 0x0fff;
  const uint32_t MAX_LINE_NUM = 0x000fffff;
}

// -------------------------------------------------------------------
// SourceLocation

void SourceLocation::trace() const {
  if (file != NULL) {
    file->mark();
  }
}

void SourceLocation::dump() const {
  if (file != NULL && !file->filePath().empty()) {
    TokenPosition pos = file->tokenPosition(*this);
    llvm::errs() << file->filePath() << ":" << pos.beginLine << "\n";
  }
}

// -------------------------------------------------------------------
// ProgramSource

TokenPosition ProgramSource::tokenPosition(const SourceLocation & loc) {
  DASSERT(loc.end >= loc.begin);
  std::vector<uint32_t>::const_iterator itBegin =
    std::upper_bound(lineOffsets_.begin(), lineOffsets_.end(), loc.begin) - 1;
  std::vector<uint32_t>::const_iterator itEnd =
    std::upper_bound(lineOffsets_.begin(), lineOffsets_.end(), loc.end) - 1;

  TokenPosition result;
  result.beginLine = itBegin - lineOffsets_.begin() + 1;
  result.beginCol = loc.begin - *itBegin;
  result.endLine = itEnd - lineOffsets_.begin() + 1;
  result.endCol = loc.end - *itEnd;

  DASSERT(int32_t(result.beginLine) >= 0);
  DASSERT(int32_t(result.beginCol) >= 0);
  return result;
}

// -------------------------------------------------------------------
// SourceFile

std::istream & SourceFile::open() {
  stream.open(filePath_.c_str());
  return stream;
}

void SourceFile::close() {
  stream.close();
}

void SourceFile::dump() const {
  llvm::errs() << filePath_ << "\n";
}

bool SourceFile::readLineAt(uint32_t lineIndex, std::string & result) {
  if (stream.good()) {
    std::wifstream::pos_type savePos = stream.tellg();
    stream.seekg(lineOffsets_[lineIndex], std::ios_base::beg);
    result.clear();
    wchar_t ch;
    while ((ch = stream.get()) > 0 && ch != '\n' && ch != '\r') {
      result.push_back(ch);
    }
    stream.seekg(savePos, std::ios_base::beg);
    return true;
  }
  return false;
}

// -------------------------------------------------------------------
// SourceString

bool SourceString::readLineAt(uint32_t lineIndex, std::string & result) {
  const std::string &str = stream.rdbuf()->str();
  std::string::const_iterator it = str.begin() + lineOffsets_[lineIndex];
  result.clear();
  while (it < str.end() && *it != '\n' && *it != '\r') {
    result.push_back(*it++);
  }
  return true;
}

// -------------------------------------------------------------------
// ArchiveFile

std::istream & ArchiveFile::open() {
  diag.__fail("Invalid operation: ArchiveFile::open", __FILE__, __LINE__);
}

void ArchiveFile::close() {
  DFAIL("Invalid operation: ArchiveFile::close");
}

bool ArchiveFile::readLineAt(uint32_t start, std::string & result) {
  DFAIL("Invalid operation: ArchiveFile::readLineAt");
}

void ArchiveFile::dump() const {
  llvm::errs() << filePath_ << "\n";
}

// -------------------------------------------------------------------
// ArchiveEntry

std::istream & ArchiveEntry::open() {
  DFAIL("Invalid operation: ArchiveEntry::open");
}

void ArchiveEntry::close() {
  DFAIL("Invalid operation: ArchiveEntry::close");
}

bool ArchiveEntry::readLineAt(uint32_t start, std::string & result) {
  DFAIL("Invalid operation: ArchiveEntry::readLineAt");
}

void ArchiveEntry::dump() const {
  llvm::errs() << container_->filePath() << "#" << filePath_ << "\n";
}

void ArchiveEntry::trace() const {
  container_->mark();
}

TokenPosition ArchiveEntry::tokenPosition(const SourceLocation & loc) {
  TokenPosition result;
  result.beginLine = loc.begin >> COL_BITS;
  result.beginCol = loc.begin & MAX_COL_NUM;
  result.endLine = loc.end >> COL_BITS;
  result.endCol = loc.end & MAX_COL_NUM;
  return result;
}

void ArchiveEntry::encodeLocation(const TokenPosition & pos, SourceLocation & loc) {
  uint32_t beginLine = std::min(pos.beginLine, MAX_LINE_NUM);
  uint32_t beginCol = std::min(pos.beginCol, MAX_COL_NUM);
  uint32_t endLine = std::min(pos.endLine, MAX_LINE_NUM);
  uint32_t endCol = std::min(pos.endCol, MAX_COL_NUM);
  loc.begin = (beginLine << COL_BITS) + beginCol;
  loc.end = (endLine << COL_BITS) + endCol;
}

}
