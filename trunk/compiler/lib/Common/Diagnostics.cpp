/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"
#include <llvm/Support/CommandLine.h>
#include <config.h>
#include <stdio.h>

#if HAVE_EXECINFO_H
#include <execinfo.h>         // For backtrace().
#endif

#if HAVE_CXXABI_H
#include <cxxabi.h>
#endif

static llvm::cl::opt<bool>
DebugErrors("debug-errors",
    llvm::cl::desc("Print compiler stack trace on fatal error"));

namespace tart {
    
Diagnostics::StdErrWriter Diagnostics::StdErrWriter::instance;

namespace {
  char * severityNames[Diagnostics::Severity_Levels] = {
    "",
    "info: ",
    "warning: ",
    "error: ",
    "error: ",
  };

  static char * INDENTATION = "                                ";
  static int MAX_INDENT = 16;

  void writeIndent(int level) {
    fprintf(stderr, "%.*s", std::min(level, MAX_INDENT) * 2, INDENTATION);
  }
}

Diagnostics diag;

Diagnostics::Diagnostics()
    : writer_(&StdErrWriter::instance)
    , recovery(Open)
    , indentLevel(0)
    , minSeverity(Debug) {
  reset();
}

void Diagnostics::reset() {
  // Clear message counts
  for (int i = 0; i < Severity_Levels; i++) {
    messageCount[i] = 0;
  }
  recovery = Open;
}

// -------------------------------------------------------------------
// Error reporting functions
// -------------------------------------------------------------------

void Diagnostics::write(const SourceLocation & loc, Severity sev,
    const std::string & msg) {
      
  switch (sev) {
    case Fatal:
    case Error:
      // If we are recovering from another error, don't show this one.
      if (recovery == Closed || recovery == Gated) {
        recovery = Closed;
        return;
      }
      
      recovery = Gated;
      break;
      
    case Warning:
    case Info:
      // Allow info messages to follow-up a fatal, but not
      // if the fatal was supressed from an earlier fatal.
      if (recovery == Closed) {
        return;
      }
      break;
      
    default:
      break;
  }

  if (writer_ && sev >= minSeverity) {
    writer_->write(loc, sev, msg);

    if (sev == Fatal && DebugErrors) {
      printStackTrace(5);
    }
  }

  messageCount[(int)sev] += 1;
}

void Diagnostics::debug(char * msg, ...) {
  va_list ap;
  va_start(ap, msg);
  vfprintf(stderr, msg, ap);
  va_end(ap);
}

void Diagnostics::printContextStack(SourceContext * source) {
  while (source) {
    if (source->expression()) {
      diag.info(source) << FormatOptions(source->formatOptions()) << source->expression();
      source = source->parent();
    }
  }
}
  
void Diagnostics::indent() {
  ++indentLevel;
}

void Diagnostics::unindent() {
  --indentLevel;
}

int Diagnostics::getIndentLevel() {
  return indentLevel;
}

int Diagnostics::setIndentLevel(int level) {
  int result = indentLevel;
  indentLevel = level;
  return result;
}

void Diagnostics::writeLnIndent(const std::string & str) {
  writeIndent(indentLevel);
  fprintf(stderr, "%s\n", str.c_str());
}

void Diagnostics::writeLnIndent(char * msg, ...) {
  va_list ap;
  va_start(ap, msg);
  writeIndent(indentLevel);
  vfprintf(stderr, msg, ap);
  fprintf(stderr, "\n");
  va_end(ap);
}

void Diagnostics::assertionFailed(const char * expression, const char * filename, unsigned lineno) {
  fprintf(stderr, "%s:%d: Assertion failed (%s)\n", filename, lineno, expression);
  debugBreak();
  printStackTrace(2);
  abort();
}

void Diagnostics::fail(const char * msg, const char * filename, unsigned lineno) {
  fprintf(stderr, "%s:%d: Fatal error (%s)\n", filename, lineno, msg);
  debugBreak();
  printStackTrace(2);
  abort();
}

void Diagnostics::debugBreak() {
  fflush(stderr);
}

void Diagnostics::printStackTrace(int skipFrames) {
#if HAVE_BACKTRACE
  static void * stackTrace[256];

  // Use backtrace() to output a backtrace on Linux systems with glibc.
  int depth = backtrace(stackTrace,
      static_cast<int>(sizeof(stackTrace) / sizeof(stackTrace[0])));

#if HAVE_CXXABI_H
  if (char ** symbols = backtrace_symbols(stackTrace, depth)) {
    
    // Name buffer used to contain demangling result.
    size_t sz = 256;
    char * buffer = (char *)malloc(sz);

    for (int i = 0; i < depth; ++i) {
      if (i >= skipFrames) {
        char * symbol = symbols[i];
        // TODO: This is a very cheesy way to extract the symbol name,
        // need to come up with something that will work on various platforms.
        // fprintf(outstream, "%s\n", symbol);
        char * begin = strchr(symbol, '_');
        char * demangled_name = NULL;
        if (begin) {
          char * end = strchr(begin, ' ');
          if (end) {
            *end = 0;
            int status;
            demangled_name = abi::__cxa_demangle(begin, buffer, &sz, &status);
          }
        }
      
        if (demangled_name != NULL) {
          fprintf(stderr, "    %s\n", demangled_name);

          // Result may be a realloc of input buffer.
          buffer = demangled_name;
        }
      }
    }

    free(symbols);
    free(buffer);
  }
#else
  backtrace_symbols_fd(stackTrace, depth, STDERR_FILENO);
#endif
#endif
}

template<>
void Diagnostics::DiagnosticAction<Diagnostics::Fatal>::write(
    const SourceLocation & loc, const std::string & msg) {
  diag.write(loc, Fatal, msg);
  debugBreak();
}

template<>
void Diagnostics::DiagnosticAction<Diagnostics::Error>::write(
    const SourceLocation & loc, const std::string & msg) {
  diag.write(loc, Error, msg);
  debugBreak();
}

template<>
void Diagnostics::DiagnosticAction<Diagnostics::Warning>::write(
    const SourceLocation & loc, const std::string & msg) {
  diag.write(loc, Warning, msg);
  debugBreak();
}

template<>
void Diagnostics::DiagnosticAction<Diagnostics::Info>::write(
    const SourceLocation & loc, const std::string & msg) {
  diag.write(loc, Info, msg);
}

template<>
void Diagnostics::DiagnosticAction<Diagnostics::Debug>::write(
    const SourceLocation & loc, const std::string & msg) {
  diag.write(loc, Debug, msg);
}

void Diagnostics::FailAction::write(const SourceLocation & loc, const std::string & msg) {
  diag.write(loc, Debug, msg);
  debugBreak();
  diag.printStackTrace(2);
  abort();
}

void Diagnostics::AssertAction::write(const SourceLocation & loc, const std::string & msg) {
  diag.write(loc, Debug, msg);
  debugBreak();
}

void Diagnostics::StdErrWriter::write(const SourceLocation & loc, Severity sev,
    const std::string & msg) {
  if (loc.file != NULL && !loc.file->getFilePath().empty()) {
    // The TextMate error parser is fairly strict
    TokenPosition tokLoc = loc.file->getTokenPosition(loc);
    fprintf(stderr, "%s:%d: %s%.*s%s\n",
        loc.file->getFilePath().c_str(),
        tokLoc.beginLine + 1,
        severityNames[(int)sev],
        std::min(diag.indentLevel, MAX_INDENT) * 2,
        INDENTATION,
        msg.c_str());
  } else {
    fprintf(stderr, "%s%.*s%s\n",
        severityNames[(int)sev],
        std::min(diag.indentLevel, MAX_INDENT) * 2,
        INDENTATION,
        msg.c_str());
  }
}

void Diagnostics::StringWriter::write(const SourceLocation & loc, Severity sev,
    const std::string & msg) {
  char buffer[256];
  size_t len;
  if (loc.file != NULL && !loc.file->getFilePath().empty()) {
    // The TextMate error parser is fairly strict
    TokenPosition tokLoc = loc.file->getTokenPosition(loc);
    len = snprintf(buffer, sizeof(buffer), "%s:%d: %s%.*s%s\n",
        loc.file->getFilePath().c_str(),
        tokLoc.beginLine + 1,
        severityNames[(int)sev],
        std::min(diag.indentLevel, MAX_INDENT) * 2,
        INDENTATION,
        msg.c_str());
  } else {
    len = snprintf(buffer, sizeof(buffer), "%s%.*s%s\n",
        severityNames[(int)sev],
        std::min(diag.indentLevel, MAX_INDENT) * 2,
        INDENTATION,
        msg.c_str());
  }

  str_.append(buffer, 0, len);
}

}
