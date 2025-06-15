cmake_minimum_required(VERSION 3.16)
project(zpiler LANGUAGES CXX)

# --- Compiler settings ---
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Reproduce your flags exactly:
set(COMMON_FLAGS
    -g
    -O2
    -Wreorder
    -D_FORTIFY_SOURCE=2
    -D_GLIBCXX_ASSERTIONS
    -fstack-protector-strong
    -fstack-clash-protection
    -Wpedantic
    -Wno-unused-variable
    -Wno-unused-parameter
    -pedantic
)

if (MSVC)
    add_compile_options(/W4 /WX)
else()
    add_compile_options(-Wall -Wextra)
endif()

# Append to both debug and release by default:
string(REPLACE ";" " " FLAGS_STR "${COMMON_FLAGS}")
add_compile_options(${COMMON_FLAGS})

# --- Include directories ---
include_directories(${PROJECT_SOURCE_DIR}/include)

# --- Sources ---
file(GLOB_RECURSE ZPILER_SRCS
    ${PROJECT_SOURCE_DIR}/src/*.cpp
)
set(ZPILER_SRCS ${ZPILER_SRCS} ${PROJECT_SOURCE_DIR}/main.cpp)

# --- Output directories ---
# Place all object files under build/ (relative to your build dir)
# CMake does this by default when you configure in a separate build tree.

# --- Executable ---
add_executable(zpiler ${ZPILER_SRCS})

# Ensure the executable is named exactly "zpiler"
set_target_properties(zpiler PROPERTIES OUTPUT_NAME "zpiler")