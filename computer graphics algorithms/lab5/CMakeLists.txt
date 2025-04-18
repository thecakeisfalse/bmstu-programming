cmake_minimum_required(VERSION 3.29)

project(
  lab5
  VERSION 0.0.1
  LANGUAGES CXX
)

file(GLOB_RECURSE SOURCES *.cc)

add_executable(${CMAKE_PROJECT_NAME} ${SOURCES})

set_target_properties(${CMAKE_PROJECT_NAME} PROPERTIES
  CXX_STANDART 17
  CXX_STANDART_REQUIRED YES
)

target_link_libraries(${CMAKE_PROJECT_NAME} glfw GL)
