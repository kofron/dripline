# CMakeLists.txt for dripline/cpp/source
# Author: N. Oblath
# Created: Jan 5, 2016

##########

set( dir ${CMAKE_CURRENT_SOURCE_DIR} )

set( dripline_HEADERS
    ${dir}/amqp.hh
    ${dir}/dripline_constants.hh
    ${dir}/dripline_error.hh
    ${dir}/dripline_version.hh
    ${dir}/hub.hh
    ${dir}/message.hh
    ${dir}/relayer.hh
    ${dir}/service.hh
    ${dir}/uuid.hh
)

set( dripline_SOURCES
    ${dir}/dripline_constants.cc
    ${dir}/dripline_error.cc
    ${dir}/dripline_version.cc
    ${dir}/hub.cc
    ${dir}/message.cc
    ${dir}/relayer.cc
    ${dir}/service.cc
    ${dir}/uuid.cc
)

configure_file( ${dir}/dripline_version.cc.in ${CMAKE_CURRENT_BINARY_DIR}/dripline_version.cc )
set( dripline_SOURCES
    ${dripline_SOURCES}
    ${CMAKE_CURRENT_BINARY_DIR}/dripline_version.cc
)


##########

pbuilder_library( _dripline dripline_SOURCES "" )

pbuilder_install_headers( ${dripline_HEADERS} )

