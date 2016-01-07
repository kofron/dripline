/*
 * constants.hh
 *
 *  Created on: Jan 5, 2016
 *      Author: nsoblath
 */

#ifndef DRIPLINE_CONSTANTS_HH_
#define DRIPLINE_CONSTANTS_HH_

#include <limits>

namespace dripline
{

// Dripline message constants
// Conforming to the dripline wire protocol: https://github.com/project8/hardware/wiki/Wire-Protocol
// Please be sure that these constants are kept in sync with the dripline constants.

// Operation constants
    enum class op_t:uint32_t {
            set = 0,
            get = 1,
            config = 6,
            send = 7,
            run = 8,
            cmd = 9,
            unknown = std::numeric_limits::max< uint32_t >()
    };

    // Message type constants
    enum class msg_t:uint32_t
    {
        reply = 2,
        request = 3,
        alert = 4,
        info = 5
    };

    // Return codes
    enum class retcode_t:uint32_t
    {
        success = 0,

        warning_no_action_taken = 1,

        amqp_error = 100,
        amqp_error_broker_connection = 101,
        amqp_error_routingkey_notfound = 102,

        device_error = 200,
        device_error_connection = 201,
        device_error_no_resp = 202,

        message_error = 300,
        message_error_no_encoding = 301,
        message_error_decoding_fail = 302,
        message_error_bad_payload = 303,
        message_error_invalid_value = 304,
        message_error_timeout = 305,
        message_error_invalid_method = 306,
        message_error_access_denied = 307,
        message_error_invalid_key = 308
    };

} /* namespace dripline */

#endif /* DRIPLINE_CONSTANTS_HH_ */
