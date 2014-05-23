cmake_minimum_required(VERSION 2.6)

include(cmake/lcEngine.cmake)

project(lcEngine)

find_package(Boost REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})

add_subdirectory(src)

