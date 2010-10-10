/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/SourceFile.h"
#include "tart/Common/Diagnostics.h"
#include <ostream>
#include <iostream>
#include <fstream>
#include <algorithm>

namespace tart {

void SourceLocation::trace() const {
  if (region != NULL) {
    region->mark();
  }
}

void SourceLocation::dump() const {
  if (region != NULL && !region->getFilePath().empty()) {
    TokenPosition pos = region->tokenPosition(*this);
    fprintf(stderr, "%s:%d\n", region->getFilePath().c_str(), pos.beginLine);
  }
}

TokenPosition ProgramSource::tokenPosition(const SourceLocation & loc) {
  DASSERT(loc.end >= loc.begin);
  std::vector<uint32_t>::const_iterator itBegin =
    std::upper_bound(lineOffsets.begin(), lineOffsets.end(), loc.begin) - 1;
  std::vector<uint32_t>::const_iterator itEnd =
    std::upper_bound(lineOffsets.begin(), lineOffsets.end(), loc.end) - 1;

  TokenPosition result;
  result.beginLine = itBegin - lineOffsets.begin() + 1;
  result.beginCol = loc.begin - *itBegin;
  result.endLine = itEnd - lineOffsets.begin() + 1;
  result.endCol = loc.end - *itEnd;

  DASSERT(int32_t(result.beginLine) >= 0);
  DASSERT(int32_t(result.beginCol) >= 0);
  return result;
}

std::istream & SourceFile::open() {
  stream.open(filePath.c_str());
  return stream;
}

void SourceFile::close() {
  stream.close();
}

bool SourceFile::readLineAt(uint32_t lineIndex, std::string & result) {
  if (stream.good()) {
    std::wifstream::pos_type savePos = stream.tellg();
    stream.seekg(lineOffsets[lineIndex], std::ios_base::beg);
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

bool SourceString::readLineAt(uint32_t lineIndex, std::string & result) {
  const std::string &str = stream.rdbuf()->str();
  std::string::const_iterator it = str.begin() + lineOffsets[lineIndex];
  result.clear();
  while (it < str.end() && *it != '\n' && *it != '\r') {
    result.push_back(*it++);
  }
  return true;
}

}
