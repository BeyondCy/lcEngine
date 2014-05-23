cmake_minimum_required(VERSION 2.6)

include(cmake/lcEngine.cmake)

project(lcEngine)

find_package(Boost REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_subdirectory(src)

