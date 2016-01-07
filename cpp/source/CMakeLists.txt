# CMakeLists.txt for dripline/cpp/source
# Author: N. Oblath
# Created: Jan 5, 2016

##########

set( dir ${CMAKE_CURRENT_SOURCE_DIR} )

set( dripline_HEADERS
    ${dir}/amqp.hh
    ${dir}/dripline_constants.hh
    ${dir}/dripline_error.hh
    ${dir}/message.hh
    ${dir}/service.hh
    ${dir}/uuid.hh
)

set( dripline_SOURCES
    ${dir}/dripline_constants.cc
    ${dir}/dripline_error.cc
    ${dir}/message.cc
    ${dir}/service.cc
    ${dir}/uuid.cc
)

##########

pbuilder_library( _dripline dripline_SOURCES "" )

pbuilder_install_headers( ${dripline_HEADERS} )

