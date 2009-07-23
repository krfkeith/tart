# CMake build file for Tart/compiler

file(GLOB sources lib/*/*.cpp)
file(GLOB headers include/*/*.h)

source_group(AST REGULAR_EXPRESSION lib/AST/*)
source_group(AST REGULAR_EXPRESSION include/tart/AST/*)
source_group(CFG REGULAR_EXPRESSION include/tart/CFG/*)
source_group(Common REGULAR_EXPRESSION include/tart/Common/*)
source_group(Gen REGULAR_EXPRESSION include/tart/Gen/*)
source_group(Lex REGULAR_EXPRESSION include/tart/Lex/*)

add_library(compiler STATIC ${sources} ${headers})
