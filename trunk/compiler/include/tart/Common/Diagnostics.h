/* ================================================================ *
  TART - A Sweet Programming Language.
* ================================================================ */

#ifndef TART_COMMON_DIAGNOSTICS_H
#define TART_COMMON_DIAGNOSTICS_H

#ifndef TART_COMMON_FORMATTABLE_H
#include "Tart/Common/Formattable.h"
#endif

#ifndef TART_COMMON_SOURCELOCATION_H
#include "Tart/Common/SourceLocation.h"
#endif

#include <stdarg.h>
#include <stdio.h>
#include <string>
#include <sstream>

#include "llvm/Support/Compiler.h"

namespace tart {

/// ---------------------------------------------------------------
/// Assertion macros
#define DASSERT(expression) \
  if (!(expression)) { \
    diag.assertionFailed(#expression, __FILE__, __LINE__); \
  } else (void)0

#define DASSERT_MSG(expression, msg) \
  if (!(expression)) { \
    diag.assertionFailedMsg(msg, __FILE__, __LINE__); \
  } else (void)0

#define DASSERT_OBJ(expression, ctx) \
  if (!(expression)) { \
    diag.assertionFailed(#expression, __FILE__, __LINE__, ctx); \
  } else (void)0

#define DFAIL(msg) \
  diag.fail(msg, __FILE__, __LINE__)

/// ---------------------------------------------------------------
/// Various diagnostic functions.
///
/// Typical usage:
///
///   diag.fatal(location) << "Undefined symbol: " << name;
///
class Diagnostics {
public:
  enum Severity {
    Debug = 0,
    Info,
    Warning,
    Error,
    Fatal,
    Off,

    Severity_Levels,
  };

  enum RecoveryState {
    Open,         // All messages are allowed
    Gated,        // Follow-up messages allowed, but not new ones.
    Closed,       // All error messages supressed
  };

  /** Base stream class for diagnostic messages. */
  template <class T>
  class MessageStream : public FormatStream {
  private:
    const SourceLocation loc;
    std::stringstream sstream;

  public:

    MessageStream()
      : FormatStream(sstream)
      , loc(SourceLocation())
    {}

    MessageStream(const SourceLocation & l)
      : FormatStream(sstream)
      , loc(l)
    {}

    MessageStream(const Locatable * l)
      : FormatStream(sstream)
      , loc(l ? l->location() : SourceLocation())
    {}

    MessageStream(const MessageStream & src)
      : FormatStream(sstream)
      , loc(src.loc)
    {}

    // The destructor is where all the real action happens.
    // When the entry is destructed, the accumulated messages are
    // written to the diagnostic output.
    ~MessageStream() {
      if (!sstream.str().empty()) {
        T::write(loc, sstream.str());
      }
    }
  };

  /** Diagnostic action which prints a message with the specified severity level. */
  template <Severity severity>
  class DiagnosticAction {
  public:
    static void write(const SourceLocation & loc, const std::string & msg);
  };

  /** Diagnostic action which prints a message, then prints a stack trace and exits. */
  class FailAction {
  public:
    static void write(const SourceLocation & loc, const std::string & msg);
  };

  class AssertAction {
  public:
    static void write(const SourceLocation & loc, const std::string & msg);
  };

  typedef MessageStream<DiagnosticAction<Fatal> > FatalErrorStream;
  typedef MessageStream<DiagnosticAction<Error> > ErrorStream;
  typedef MessageStream<DiagnosticAction<Warning> > WarningStream;
  typedef MessageStream<DiagnosticAction<Info> > InfoStream;
  typedef MessageStream<DiagnosticAction<Debug> > DebugStream;

  typedef MessageStream<FailAction> FailStream;
  typedef MessageStream<AssertAction> AssertStream;

  /** A stream which does nothing. */
  class NullStream {
  public:
    NullStream() {}

    template<class T>
    inline NullStream & operator<<(const T &) {
      return *this;
    }
  };

  class Writer {
  public:
    virtual void write(const SourceLocation & loc, Severity sev, const std::string & msg) = 0;
  };

  class StdErrWriter : public Writer {
  public:
    void write(const SourceLocation & loc, Severity sev, const std::string & msg);

    static StdErrWriter instance;
  };

  class StringWriter : public Writer {
  public:
    void write(const SourceLocation & loc, Severity sev, const std::string & msg);
    const std::string & str() const { return str_; }
    std::string & str() { return str_; }

  private:
    std::string str_;
  };

protected:
  int messageCount[Severity_Levels];
  Writer * writer_;
  RecoveryState recovery;       // True if in recovery mode.
  int indentLevel;    // Used for dumping hierarchical stuff
  Severity minSeverity;

  void write(const SourceLocation & loc, Severity sev, const std::string & msg);

public:
  Diagnostics();

  /** Set the writer. */
  void setWriter(Writer * writer) { writer_ = writer; }

  /** reset counters for testing */
  void reset();

  /** Set the minimum severity level to be reported. */
  void setMinSeverity(Severity s) { minSeverity = s; }

  /** Fatal error. */
  FatalErrorStream fatal(const SourceLocation & loc = SourceLocation()) {
    return FatalErrorStream(loc);
  }

  /** Fatal error. */
  FatalErrorStream fatal(const Locatable * loc) {
    return FatalErrorStream(loc);
  }

  /** Error. */
  ErrorStream error(const SourceLocation & loc = SourceLocation()) {
    return ErrorStream(loc);
  }

  /** Error. */
  ErrorStream error(const Locatable * loc) {
    return ErrorStream(loc);
  }

  /** Warning message. */
  WarningStream warn(const SourceLocation & loc = SourceLocation()) {
    return WarningStream(loc);
  }

  /** Warning message. */
  WarningStream warn(const Locatable * loc) {
    return WarningStream(loc);
  }

  /** Info message. */
  InfoStream info(const SourceLocation & loc = SourceLocation()) {
    return InfoStream(loc);
  }

  /** Info message. */
  InfoStream info(const Locatable * loc) {
    return InfoStream(loc);
  }

  /** Debugging message. */
  DebugStream debug(const SourceLocation & loc = SourceLocation()) {
    return DebugStream(loc);
  }

  /** Debugging message. */
  DebugStream debug(const Locatable * loc) {
    return DebugStream(loc);
  }

  /** Debugging message. */
  void debug(char * msg, ...);

  /** Return true if we're recovering from another error. */
  bool inRecovery() const { return recovery != Open; }

  /** Let it know that we've recovered, and can start reporting
      errors again. */
  void recovered() { recovery = Open; }

  /** Get message count by severity. */
  int getMessageCount(Severity sev) const { return messageCount[(int)sev]; }

  /** Get the count of errors. */
  int getErrorCount() const {
    return getMessageCount(Error) + getMessageCount(Fatal);
  }

  /** Get the count of warnings. */
  int getWarningCount() const { return getMessageCount(Warning); }

  /** Print the list of source contexts. */
  void printContextStack(SourceContext * source);

  /** Increase the indentation level. */
  void indent();

  /** Decrease the indentation level. */
  void unindent();

  /** Get the current indent level. */
  int getIndentLevel();

  /** Set the current indentation level. */
  int setIndentLevel(int level);

  /** write an indented line. */
  void writeLnIndent(const std::string & str);

  /** write an indented line, formatted. */
  void writeLnIndent(char * msg, ...);

  /** Assertion failure. */
  void NORETURN(assertionFailed(const char * expr, const char * fname, unsigned lineno));

  /** Assertion failure. */
  template<class T>
  void NORETURN(assertionFailed(
      const char * expr, const char * fname, unsigned lineno, const T & obj)) {
    std::stringstream ss;
    FormatStream stream(ss);
    stream.setFormatOptions(Format_Verbose);
    stream << expr;
    stream << ", context = ";
    stream << obj;
    assertionFailed(ss.str().c_str(), fname, lineno);
  }

  /** Fatal compiler error. */
  void NORETURN(fail(const char * msg, const char * fname, unsigned lineno));

  /** Break execution. */
  static void debugBreak();

  /** Print the current stack trace. */
  void printStackTrace(int skipFrames);
};

extern Diagnostics diag;

/// ---------------------------------------------------------------
/// Convenience class that increases indentation level within a scope.
class AutoIndent {
public:
  AutoIndent(bool enabled = true) : enabled_(enabled) { if (enabled_) diag.indent(); }
  ~AutoIndent() { if (enabled_) diag.unindent(); }

private:
  bool enabled_;
};

}

#endif
