# CMake build file for Tart/third-party

# Google test include dirs
include_directories(${TART_SOURCE_DIR}/third-party/gtest-1.3.0/include)
include_directories(${TART_SOURCE_DIR}/third-party/gtest-1.3.0)

# Unit test framework
add_library(gtest STATIC
    ${TART_SOURCE_DIR}/third-party/gtest-1.3.0/src/gtest-all.cc)
