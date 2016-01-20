# Minimum cmake verison 3.0 required by the way CMake handles rpath information on Mac OSX.
cmake_minimum_required (VERSION 3.0)

#########
# setup #
#########

cmake_policy( SET CMP0048 NEW ) # version in project()
project( dripline VERSION 1.0.0 )

list( APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/scarab/cmake )

include( PackageBuilder )

pbuilder_prepare_project()


####################
# dripline options #
####################

# require C++11
set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11" )


#########################
# dripline dependencies #
#########################

########
# Scarab
########

# set authentication build to TRUE
# this will require Boost 1.46 or better
set( Scarab_BUILD_AUTHENTICATION TRUE CACHE BOOL "" FORCE )

pbuilder_add_submodule( Scarab scarab/library )


##################
# SimpleAmqpClient
##################

pbuilder_add_submodule( SimpleAmqpClient SimpleAmqpClient )
set( Scarab_BUILD_CODEC_JSON TRUE CACHE BOOL "" FORCE )
set( Scarab_BUILD_CODEC_MSGPACK TRUE CACHE BOOL "" FORCE )


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

