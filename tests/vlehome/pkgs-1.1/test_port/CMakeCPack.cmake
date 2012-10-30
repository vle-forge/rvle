IF (WIN32)
  SET(CPACK_GENERATOR "ZIP")
  SET(CPACK_SOURCE_GENERATOR "ZIP")
ENDIF (WIN32)

IF (UNIX)
  SET(CPACK_GENERATOR "TGZ")
  SET(CPACK_SOURCE_GENERATOR "TGZ")
ENDIF (UNIX)

SET(CPACK_PACKAGE_NAME "${PROJECT_NAME}")
SET(CPACK_PACKAGE_VENDOR "VLE Development Team")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Package Virtual Laboratory Environment")
SET(CPACK_PACKAGE_CONTACT "Gauthier Quesnel <quesnel@users.sourceforge.net>")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${PROJECT_SOURCE_DIR}/Readme.txt")
SET(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/License.txt")
SET(CPACK_PACKAGE_VERSION_MAJOR "${MODEL_MAJOR}")
SET(CPACK_PACKAGE_VERSION_MINOR "${MODEL_MINOR}")

# CPack source configuration
SET(CPACK_SOURCE_PACKAGE_FILE_NAME "${MODEL_NAME}")
SET(CPACK_PACKAGE_FILE_NAME "${MODEL_NAME}-${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
SET(CPACK_SOURCE_IGNORE_FILES "\\\\.swp$;/\\\\.gitignore;/build/;/lib/;/\\\\.git/")

INCLUDE(CPack)

# vim:tw=0:ts=8:tw=0:sw=2:sts=2
