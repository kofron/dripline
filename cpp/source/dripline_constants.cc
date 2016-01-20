/*
 * constants.cc
 *
 *  Created on: Jan 5, 2016
 *      Author: nsoblath
 */

#include "dripline_constants.hh"

namespace dripline
{

    // Conversion functions for use when a numeric value is needed
    uint32_t to_uint( op_t an_op )
    {
        return static_cast< uint32_t >( an_op );
    }
    op_t to_op_t( uint32_t an_op_uint )
    {
        return static_cast< op_t >( an_op_uint );
    }
    std::ostream& operator<<( std::ostream& a_os, op_t an_op )
    {
        return a_os << to_uint( an_op );
    }

    // Conversion functions for use when a numeric value is needed
    uint32_t to_uint( msg_t a_msg )
    {
        return static_cast< uint32_t >( a_msg );
    }
    msg_t to_msg_t( uint32_t a_msg_uint )
    {
        return static_cast< msg_t >( a_msg_uint );
    }
    std::ostream& operator<<( std::ostream& a_os, msg_t a_msg )
    {
        return a_os << to_uint( a_msg );
    }

    // Conversion functions for use when a numeric value is needed
    uint32_t to_uint( retcode_t a_ret )
    {
        return static_cast< uint32_t >( a_ret );
    }
    retcode_t to_retcode_t( uint32_t a_ret_uint )
    {
        return static_cast< retcode_t >( a_ret_uint );
    }
    std::ostream& operator<<( std::ostream& a_os, retcode_t a_ret )
    {
        return a_os << to_uint( a_ret );
    }

} /* namespace dripline */
