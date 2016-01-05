# Minimum cmake verison 3.0 required by the way CMake handles rpath information on Mac OSX.
cmake_minimum_required (VERSION 3.0)

#########
# setup #
#########

cmake_policy( SET CMP0048 NEW ) # version in project()
project( dripline VERSION 0.1.0 )

list( APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/scarab/cmake )

include( PackageBuilder )

pbuilder_prepare_project()


####################
# dripline options #
####################




#########################
# dripline dependencies #
#########################

########
# Scarab
########

pbuilder_add_submodule( Scarab scarab/library )


########
# Boost
########

# Boost (1.46 required for filesystem version 3)
find_package( Boost 1.46.0 REQUIRED )
# make sure dynamic linking is assumed for all boost libraries
add_definitions( -DBOOST_ALL_DYN_LINK )
include_directories( ${Boost_INCLUDE_DIRS} )
# No compiled libraries are currently required
#pbuilder_add_ext_libraries( ${Boost_LIBRARIES} )


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

