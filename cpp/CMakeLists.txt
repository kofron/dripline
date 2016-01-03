# Minimum cmake verison 3.0 required by the way CMake handles rpath information on Mac OSX.
cmake_minimum_required (VERSION 3.0)

#########
# setup #
#########

cmake_policy( SET CMP0048 NEW ) # version in project()
project( dripline VERSION 0.1.0 )

list( APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/drosophila/cmake )

include( PackageBuilder )

pbuilder_prepare_project()

set( CMAKE_MACOSX_RPATH TRUE )

set (BUILD_SHARED_LIBS ON)


####################
# dripline options #
####################

#set( Drosophila_BUILD_CODEC_JSON CACHE BOOL "Flag to enable building the JSON codec" )

#set( Drosophila_BUILD_CODEC_MSGPACK CACHE BOOL "Flag to enable building the msgpack codec" )



#########################
# dripline dependencies #
#########################

add_subdirectory( drosophila )
pbuilder_add_submodule_libraries( ${Drosophila_LIBRARIES} )
include_directories( ${Drosophila_DEP_INCLUDE_DIRS} )



##################
# Build Dripline #
##################

include_directories( BEFORE
    ${PROJECT_SOURCE_DIR}/source
)

add_subdirectory( source )

################################
# Variables for parent project #
################################

pbuilder_variables_for_parent()

