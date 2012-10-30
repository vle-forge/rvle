CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(test_port CXX C)
ENABLE_TESTING()

#SET(MODEL_MAJOR 0)
#SET(MODEL_MINOR 1)
#SET(MODEL_PATCH 0)
#SET(MODEL_EXTRA "-dev")

#IF ("${MODEL_EXTRA}" STREQUAL "")
#  SET(MODEL_NAME "${PROJECT_NAME}-${MODEL_MAJOR}.${MODEL_MINOR}.${MODEL_PATCH}")
#ELSE ("${MODEL_EXTRA}" STREQUAL "")
#  SET(MODEL_NAME "${PROJECT_NAME}-${MODEL_MAJOR}.${MODEL_MINOR}.${MODEL_PATCH}${MODEL_EXTRA}")
#ENDIF ("${MODEL_EXTRA}" STREQUAL "")


SET(MODEL_NAME "${PROJECT_NAME}")

##
## Modules
##

INCLUDE(CheckIncludeFileCXX)
INCLUDE(CheckIncludeFile)
INCLUDE(CheckLibraryExists)
INCLUDE(CMakeDetermineCCompiler)

##
## Debug mode
##

IF(UNIX AND NOT WIN32)
  SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")
  SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra")
ENDIF(UNIX AND NOT WIN32)

##
## Check libraries with pkgconfig
##

FIND_PACKAGE(PkgConfig REQUIRED)
PKG_CHECK_MODULES(VLE vle-1.1)

##
## Find boost
##

SET(Boost_USE_STATIC_LIBS OFF)
SET(Boost_USE_MULTITHREAD ON)
FIND_PACKAGE(Boost COMPONENTS unit_test_framework date_time)

IF (Boost_UNIT_TEST_FRAMEWORK_FOUND)
  SET(HAVE_UNITTESTFRAMEWORK 1 CACHE INTERNAL "" FORCE)
ENDIF (Boost_UNIT_TEST_FRAMEWORK_FOUND)

##
## Subdirectory
##

ADD_SUBDIRECTORY(exp)
ADD_SUBDIRECTORY(src)

MESSAGE(STATUS "- - - -")
MESSAGE(STATUS "Package configured successfully.")
MESSAGE(STATUS "Using ${CMAKE_INSTALL_PREFIX} for installation.")
MESSAGE(STATUS "Using Building with ${CMAKE_C_FLAGS} for C compiler flags")
MESSAGE(STATUS "Using Building with ${CMAKE_CXX_FLAGS} for C++ compiler flags")
MESSAGE(STATUS "- - - -")

##
## CPack configuration
##

INSTALL(FILES Authors.txt ChangeLog.txt License.txt News.txt Readme.txt
  DESTINATION .)

INCLUDE(CMakeCPack.cmake)
