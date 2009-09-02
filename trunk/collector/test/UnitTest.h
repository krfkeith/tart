#include <llvm/Support/DataTypes.h>
#include <llvm/ADT/StringExtras.h>
#include <stdio.h>
#include <string>

inline std::string ToString(bool v) {
  return v ? "true" : "false";
}

inline std::string ToString(int32_t v) {
  return llvm::itostr(v);
}

inline std::string ToString(uint32_t v) {
  return llvm::itostr(v);
}

inline std::string ToString(int64_t v) {
  return llvm::itostr(v);
}

inline std::string ToString(uint64_t v) {
  return llvm::itostr(v);
}

inline std::string ToString(size_t v) {
  return llvm::itostr(v);
}

inline std::string ToString(double v) {
  return llvm::ftostr(v);
}

template<class T>
inline std::string ToString(T * v) {
  return llvm::utohexstr((uint32_t)v);
}

inline std::string ToString(const std::wstring & v) {
  std::string result;
  result.push_back('"');
  char numbuf[16];
  for (std::wstring::const_iterator i = v.begin(); i != v.end(); ++i) {
    wchar_t ch = *i;
    if (ch == '\n') {
      result.append("\\n");
    } else if (ch == '\r') {
      result.append("\\r");
    } else if (ch == '\t') {
      result.append("\\t");
    } else if (ch == '\b') {
      result.append("\\b");
    } else if (ch == '\0') {
      result.append("\\0");
    } else if (ch > 0xffff) {
      sprintf(numbuf, "\\U%8.8x", ch);
      result.append(numbuf);
    } else if (ch > 0xff) {
      sprintf(numbuf, "\\u%4.4x", ch);
      result.append(numbuf);
    } else if (ch < ' ' || ch > 0x7f) {
      sprintf(numbuf, "\\x%2.2x", ch);
      result.append(numbuf);
    } else {
      result.push_back(ch);
    }
  }
  result.push_back('"');
  return result;
}

class TestRunner {
public:
  int32_t failureCount;

  static int32_t GetFailureCount() {
    return runner.failureCount;
  }

  static void Fail(const char *file, int32_t line, const char *msg) {
    fprintf(stderr, "%s:%d: error: %s\n", file, line, msg);
    runner.failureCount++;
  }

  template<class T1, class T2>
  static void VerifyEqual(
    const char *file, int32_t line, const T1 & expected, const T2 & actual) {
    if (expected != actual) {
      std::string sExpected = ToString(expected);
      std::string sActual = ToString(actual);
      fprintf(stderr, "%s:%d: error: expected %s, got %s\n", file, line, sExpected.c_str(), sActual.c_str());
      runner.failureCount++;
    }
  }

  template<class T1, class T2>
  static void VerifyNotEqual(
    const char *file, int32_t line, const T1 & expected, const T2 & actual) {
    if (expected == actual) {
      std::string sExpected = ToString(expected);
      std::string sActual = ToString(actual);
      fprintf(stderr, "%s:%d: error: expected %s != %s\n", file, line, sExpected.c_str(), sActual.c_str());
      runner.failureCount++;
    }
  }

  static TestRunner runner;
};

#define TEST_FAIL(msg) \
    TestRunner::Fail(__FILE__, __LINE__, msg)

#define TEST_ASSERT(x) \
    if (!(x)) TestRunner::Fail(__FILE__, __LINE__, "Assertion failed: " #x)

#define TEST_ASSERT_EQUAL(expected, actual) \
    TestRunner::VerifyEqual(__FILE__, __LINE__, expected, actual)

#define TEST_ASSERT_NOT_EQUAL(expected, actual) \
    TestRunner::VerifyNotEqual(__FILE__, __LINE__, expected, actual)
