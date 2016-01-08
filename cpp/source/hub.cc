/*
 * hub.cc
 *
 *  Created on: Jan 7, 2016
 *      Author: nsoblath
 */

#include "hub.hh"

#include "logger.hh"
#include "parsable.hh"

namespace dripline
{
    using scarab::parsable;

    LOGGER( dlog, "hub" );


    hub::hub( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_queue_name, const string& a_auth_file ) :
            service( a_address, a_port, a_exchange, a_queue_name, a_auth_file)
    {
        f_keys.insert( a_queue_name + string( ".#" ) );
    }

    hub::~hub()
    {
    }

    bool hub::on_request_message( const request_ptr_t a_request )
    {
        reply_package t_reply_pkg( shared_ptr< service >( this ), a_request );

        // the lockout key must be valid
        if( ! a_request->get_lockout_key_valid() )
        {
            t_reply_pkg.send_reply( retcode_t::message_error_invalid_key, "Lockout key could not be parsed" );
            WARN( dlog, "Message had an invalid lockout key" );
            return false;
        }
        else
        {
            switch( a_request->get_message_op() )
            {
                case op_t::run:
                {
                    return __do_run_request( a_request, t_reply_pkg );
                    break;
                }
                case op_t::get:
                {
                    return __do_get_request( a_request, t_reply_pkg );
                    break;
                } // end "get" operation
                case op_t::set:
                {
                    return __do_set_request( a_request, t_reply_pkg );
                    break;
                } // end "set" operation
                case op_t::cmd:
                {
                    return __do_cmd_request( a_request, t_reply_pkg );
                    break;
                }
                default:
                    std::stringstream t_error_stream;
                    t_error_stream << "Unrecognized message operation: <" << a_request->get_message_type() << ">";
                    string t_error_msg( t_error_stream.str() );
                    WARN( dlog, t_error_msg );
                    t_reply_pkg.send_reply( retcode_t::message_error_invalid_method, t_error_msg );
                    return false;
                    break;
            } // end switch on message type
        }
        return false;
    }

    bool hub::do_run_request( const request_ptr_t, reply_package& )
    {
        WARN( dlog, "This hub does not accept run requests" );
        return false;
    }

    bool hub::do_get_request( const request_ptr_t, reply_package& )
    {
        WARN( dlog, "This hub does not accept get requests other than the basic dripline instructions" );
        return false;
    }

    bool hub::do_set_request( const request_ptr_t, reply_package& )
    {
        WARN( dlog, "This hub does not accept set requests" );
        return false;
    }

    bool hub::do_cmd_request( const request_ptr_t, reply_package& )
    {
        WARN( dlog, "This hub does not accept cmd requests other than the basic dripline instructions" );
        return false;
    }

    bool hub::__do_run_request( const request_ptr_t a_request, hub::reply_package& a_reply_pkg )
    {
        DEBUG( dlog, "Run operation request received" );

        if( ! authenticate( a_request->lockout_key() ) )
        {
            std::stringstream t_conv;
            t_conv << a_request->lockout_key();
            string t_message( "Request denied due to lockout (key used: " + t_conv.str() + ")" );
            INFO( dlog, t_message );
            a_reply_pkg.send_reply( retcode_t::message_error_access_denied, t_message );
            return false;
        }

        return do_run_request( a_request, a_reply_pkg );
    }

    bool hub::__do_get_request( request_ptr_t a_request, hub::reply_package& a_reply_pkg )
    {
        DEBUG( dlog, "Get operation request received" );

        string t_query_type;
        if( ! a_request->get_parsed_rks()->empty() )
        {
            t_query_type = a_request->get_parsed_rks()->begin()->first;
        }

        if( t_query_type == "is-locked" )
        {
            return handle_is_locked_request( a_request, a_reply_pkg );
        }

        return do_get_request( a_request, a_reply_pkg );
    }

    bool hub::__do_set_request( const request_ptr_t a_request, hub::reply_package& a_reply_pkg )
    {
        DEBUG( dlog, "Set request received" );

        if( ! authenticate( a_request->lockout_key() ) )
        {
            std::stringstream t_conv;
            t_conv << a_request->lockout_key();
            string t_message( "Request denied due to lockout (key used: " + t_conv.str() + ")" );
            INFO( dlog, t_message );
            a_reply_pkg.send_reply( retcode_t::message_error_access_denied, t_message );
            return false;
        }

        return do_set_request( a_request, a_reply_pkg );
    }

    bool hub::__do_cmd_request( const request_ptr_t a_request, hub::reply_package& a_reply_pkg )
    {
        DEBUG( dlog, "Cmd request received" );

        string t_instruction;
        if( ! a_request->get_parsed_rks()->empty() )
        {
            t_instruction = a_request->get_parsed_rks()->begin()->first;
        }

        //WARN( mtlog, "uuid string: " << a_request->get_payload().get_value( "key", "") << ", uuid: " << uuid_from_string( a_request->get_payload().get_value( "key", "") ) );
        // this condition includes the exception for the unlock instruction that allows us to force the unlock regardless of the key.
        // disable_key() checks the lockout key if it's not forced, so it's okay that we bypass this call to authenticate() for the unlock instruction.
        if( ! authenticate( a_request->lockout_key() ) && t_instruction != "unlock" )
        {
            std::stringstream t_conv;
            t_conv << a_request->lockout_key();
            string t_message( "Request denied due to lockout (key used: " + t_conv.str() + ")" );
            INFO( dlog, t_message );
            a_reply_pkg.send_reply( retcode_t::message_error_access_denied, t_message );
            return false;
        }

        if( t_instruction == "lock" )
        {
            return handle_lock_request( a_request, a_reply_pkg );
        }
        else if( t_instruction == "unlock" )
        {
            return handle_unlock_request( a_request, a_reply_pkg );
        }
        else if( t_instruction == "ping" )
        {
            return handle_ping_request( a_request, a_reply_pkg );
        }

        return do_cmd_request( a_request, a_reply_pkg );
    }

    bool hub::handle_lock_request( const request_ptr_t a_request, hub::reply_package& a_reply_pkg )
    {
        uuid_t t_new_key = enable_lockout( a_request->get_sender_info(), a_request->lockout_key() );
        if( t_new_key.is_nil() )
        {
            a_reply_pkg.send_reply( retcode_t::device_error, "Unable to lock server" );
            return false;
        }

        a_reply_pkg.f_payload.add( "key", new param_value( string_from_uuid( t_new_key ) ) );
        return a_reply_pkg.send_reply( retcode_t::success, "Server is now locked" );
    }

    bool hub::handle_unlock_request( const request_ptr_t a_request, hub::reply_package& a_reply_pkg )
    {
        if( ! is_locked() )
        {
            return a_reply_pkg.send_reply( retcode_t::warning_no_action_taken, "Already unlocked" );
        }

        bool t_force = a_request->get_payload().get_value( "force", false );

        if( disable_lockout( a_request->lockout_key(), t_force ) )
        {
            return a_reply_pkg.send_reply( retcode_t::success, "Server unlocked" );
        }
        a_reply_pkg.send_reply( retcode_t::device_error, "Failed to unlock server" );
        return false;
    }

    bool hub::handle_is_locked_request( const request_ptr_t, hub::reply_package& a_reply_pkg )
    {
        bool t_is_locked = is_locked();
        a_reply_pkg.f_payload.add( "is_locked", param_value( t_is_locked ) );
        if( t_is_locked ) a_reply_pkg.f_payload.add( "tag", f_lockout_tag );
        return a_reply_pkg.send_reply( retcode_t::success, "Checked lock status" );
    }

    bool hub::handle_ping_request( const request_ptr_t a_request, hub::reply_package& a_reply_pkg )
    {
        string t_sender = a_request->sender_package();
        return a_reply_pkg.send_reply( retcode_t::success, "Hello, " + t_sender );
    }


    bool hub::reply_package::send_reply( retcode_t a_return_code, const std::string& a_return_msg ) const
    {
        reply_ptr_t t_reply = msg_reply::create( a_return_code, a_return_msg, new param_node( f_payload ), f_reply_to, message::encoding::json );
        t_reply->correlation_id() = f_correlation_id;

        DEBUG( dlog, "Sending reply message:\n" <<
                 "Return code: " << t_reply->get_return_code() << '\n' <<
                 "Return message: " << t_reply->return_msg() <<
                 f_payload );

        const shared_ptr< service > t_service = f_service_ptr.lock();
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

    uuid_t hub::enable_lockout( const param_node& a_tag, uuid_t a_key )
    {
        if( is_locked() ) return generate_nil_uuid();
        if( a_key.is_nil() ) f_lockout_key = generate_random_uuid();
        else f_lockout_key = a_key;
        f_lockout_tag = a_tag;
        return f_lockout_key;
    }

    bool hub::disable_lockout( const uuid_t& a_key, bool a_force )
    {
        if( ! is_locked() ) return true;
        if( ! a_force && a_key != f_lockout_key ) return false;
        f_lockout_key = generate_nil_uuid();
        f_lockout_tag.clear();
        return true;
    }

    bool hub::authenticate( const uuid_t& a_key ) const
    {
        DEBUG( dlog, "Authenticating with key <" << a_key << ">" );
        if( is_locked() ) return check_key( a_key );
        return true;
    }


} /* namespace dripline */
