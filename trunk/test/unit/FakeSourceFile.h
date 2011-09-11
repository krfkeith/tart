/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_COMMON_SOURCEFILE_H
#include "tart/Common/SourceFile.h"
#endif

#include <sstream>

/// -------------------------------------------------------------------
/// Fake source file class
class FakeSourceFile : public tart::SourceString {
public:
  FakeSourceFile(const char * src) : tart::SourceString(src) {}
  ProgramSource * get() { return NULL; }
};
