# CMake build file for Tart/third-party

# Google test include dirs
include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.6.0/include)
include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.6.0/gtest/include)
include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.6.0)
include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.6.0/gtest)

# Unit test framework
add_library(gtest STATIC
    ${TART_SOURCE_DIR}/third-party/gmock-1.6.0/gtest/src/gtest-all.cc)
if (CMAKE_COMPILER_IS_GNUCXX)
  set_target_properties(gtest PROPERTIES COMPILE_FLAGS "-Wno-extra")
endif (CMAKE_COMPILER_IS_GNUCXX)

# Mock framework
add_library(gmock STATIC
    ${TART_SOURCE_DIR}/third-party/gmock-1.6.0/src/gmock-all.cc)
