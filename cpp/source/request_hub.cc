/*
 * request_hub.cc
 *
 *  Created on: Jan 7, 2016
 *      Author: nsoblath
 */

#include "request_hub.hh"

#include "logger.hh"

namespace dripline
{
    LOGGER( dlog, "request_hub" );

    bool reply_package::send_reply( retcode_t a_return_code, const std::string& a_return_msg )
    {
        reply_ptr_t t_reply = msg_reply::create( a_return_code, a_return_msg, new param_node( f_payload ), f_reply_to, message::encoding::json );
        t_reply->correlation_id() = f_correlation_id;

        DEBUG( dlog, "Sending reply message:\n" <<
                 "Return code: " << t_reply->get_return_code() << '\n' <<
                 "Return message: " << t_reply->return_msg() <<
                 f_payload );

        shared_ptr< service > t_service = f_service_ptr.lock();
        if( ! t_service )
        {
            WARN( dlog, "Unable to send reply; it appears that the AMQP service no longer exists" );
            return false;
        }

        if( ! t_service->send( t_reply ) )
        {
            WARN( dlog, "Something went wrong while sending the reply" );
            return false;
        }

        return true;
    }


    request_hub::request_hub( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_queue_name, bool a_authenticate ) :
            service( a_address, a_port, a_exchange, a_queue_name, a_authenticate)
    {
    }

    request_hub::~request_hub()
    {
    }

    bool request_hub::on_request_message( request_ptr_t a_request )
    {
        reply_package t_reply_pkg( this, a_request, f_channel );

        // the lockout key must be valid
        if( ! a_request->get_lockout_key_valid() )
        {
            t_reply_pkg.send_reply( retcode_t::message_error_invalid_key, "Lockout key could not be parsed" );
            WARN( dlog, "Message had an invalid lockout key" );
        }
        else
        {
            switch( a_request->get_message_op() )
            {
                case op_t::run:
                {
                    do_run_request( t_request, t_reply_pkg );
                    break;
                }
                case op_t::get:
                {
                    do_get_request( t_request, t_reply_pkg );
                    break;
                } // end "get" operation
                case op_t::set:
                {
                    do_set_request( t_request, t_reply_pkg );
                    break;
                } // end "set" operation
                case op_t::cmd:
                {
                    do_cmd_request( t_request, t_reply_pkg );
                    break;
                }
                default:
                    std::stringstream t_error_stream;
                    t_error_stream << "Unrecognized message operation: <" << a_request->get_message_type() << ">";
                    string t_error_msg( t_error_stream.str() );
                    ERROR( dlog, t_error_msg );
                    t_reply_pkg.send_reply( retcode_t::message_error_invalid_method, t_error_msg );
                    break;
            } // end switch on message type
        }
    }

} /* namespace dripline */
