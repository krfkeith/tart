# CMake build file for Tart/compiler

# Extra flags for GCC (C++ only)
if (CMAKE_COMPILER_IS_GNUCXX)
  add_definitions(
      -Woverloaded-virtual
      -fno-operator-names -ffor-scope
      )
   if ($PROFILE_TARTC)
     add_definitions(-pg)
   endif ($PROFILE_TARTC)
endif (CMAKE_COMPILER_IS_GNUCXX)

file(GLOB sources lib/*/*.cpp)
file(GLOB headers include/*/*.h)

source_group(AST REGULAR_EXPRESSION lib/AST/*)
source_group(AST REGULAR_EXPRESSION include/tart/AST/*)
source_group(CFG REGULAR_EXPRESSION include/tart/CFG/*)
source_group(Common REGULAR_EXPRESSION include/tart/Common/*)
source_group(Defn REGULAR_EXPRESSION include/tart/Defn/*)
source_group(Expr REGULAR_EXPRESSION include/tart/Expr/*)
source_group(Gen REGULAR_EXPRESSION include/tart/Gen/*)
source_group(Lex REGULAR_EXPRESSION include/tart/Lex/*)
source_group(Meta REGULAR_EXPRESSION include/tart/Meta/*)
source_group(Sema REGULAR_EXPRESSION include/tart/Sema/*)
source_group(Type REGULAR_EXPRESSION include/tart/Type/*)

add_library(compiler STATIC ${sources} ${headers})
