# CMake project file for KVFParser

cmake_minimum_required (VERSION 2.8)
project(KVFParser)
enable_language (Fortran)
set (CMAKE_Fortran_FORMAT FREE)
set (KVFParser_VERSION_MAJOR 1)
set (KVFParser_VERSION_MINOR 0)
set (KVFParser_VERSION_PATCH 0)
set (KVFParser_VERSION
   "${KVFParser_VERSION_MAJOR}.${KVFParser_VERSION_MINOR}.${KVFParser_VERSION_PATCH}" )
include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_SURRENT_SOURCE_DIR})
link_directories(${CMAKE_CURRENT_BINARY_DIR})

# add a target to generate API documentation with Doxygen.
find_package(Doxygen 1.8)

add_library(KVFParser parser.F90)
install(TARGETS KVFParser ARCHIVE DESTINATION lib)

if(BUILD_TESTS)
   add_subdirectory(tests)
endif()

if(BUILD_DOCS)
   add_subdirectory(docs)
endif(BUILD_DOCS)

if(DOXYGEN_FOUND)
   configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in
      ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
   add_custom_target(APIDOX
      ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT "Generating API documentation with Doxygen" VERBATIM
      )
endif(DOXYGEN_FOUND)

INCLUDE(InstallRequiredSystemLibraries)
set(CPACK_GENERATOR "TGZ")
set(CPACK_SOURCE_GENERATOR "TGZ")
set(CPACK_SOURCE_IGNORE_FILES "/BUILD/;/bin/;test*;/.git*;/.kate*;run-test*")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${DRS_SOURCE_DIR}/README")
set(CPACK_PACKAGE_VENDOR "The University of Glasgow")
set(CPACK_PACKAGE_VERSION_MAJOR ${DRS_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${DRS_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${DRS_VERSION_PATCH})

INCLUDE(CPack)
