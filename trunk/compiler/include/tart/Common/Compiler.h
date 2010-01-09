/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

// Compiler class

#include "tart/Parse/Parser.h"
#include "tart/Common/Diagnostics.h"

namespace tart {

// The compiler class
class Compiler {
public:
  Compiler()
    : generateBitcode_(true)
    , generateDependencies_(false)
  {}

  void processInputFile(const std::string & infile);
  void setGenerateBitcode(bool generate) {
    generateBitcode_ = generate;
  }

  void setGenerateDependencies(bool generate) {
    generateDependencies_ = generate;
  }

private:
  bool generateBitcode_;
  bool generateDependencies_;
};

}
