cmake_minimum_required(VERSION 3.10)
project(CategoryTheory)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_subdirectory(Identity_Composition)
add_subdirectory(Types_Functions)
add_subdirectory(Categories)
add_subdirectory(Kleisli_Embellishments)
add_subdirectory(Products_Coproducts)
add_subdirectory(Algebraic_Types)
add_subdirectory(DP_Graph)
add_subdirectory(Functors)