# CMakeLists.txt for dripline/cpp/source
# Author: N. Oblath
# Created: Jan 5, 2016

##########

set( dir ${CMAKE_CURRENT_SOURCE_DIR} )

set( dripline_HEADERS
    ${dir}/message.hh
)

set( dripline_SOURCES
    ${dir}/message.cc
)

##########

#pbuilder_library( _dripline dripline_SOURCES "" )

pbuilder_install_headers( ${dripline_HEADERS} )

