/*
 * message.cc
 *
 *  Created on: Jul 9, 2015
 *      Author: nsoblath
 */

#define DRIPLINE_API_EXPORTS

#include "message.hh"

#include "dripline_constants.hh"
#include "dripline_error.hh"

#include "logger.hh"
#include "param_json.hh"
#include "time.hh"
//#include "mt_version.hh"

#include <map>

namespace dripline
{
    using scarab::param_input_json;
    using scarab::param_output_json;

    using std::string;

    LOGGER( dlog, "message" );

    //***********
    // Message
    //***********

    message::message() :
            f_routing_key(),
            f_routing_key_specifier(),
            f_parsed_rks( new parsable() ),
            f_correlation_id(),
            f_reply_to(),
            f_encoding( encoding::json ),
            f_timestamp(),
            f_sender_package( "N/A" ),
            f_sender_exe( "N/A" ),
            f_sender_version( "N/A" ),
            f_sender_commit( "N/A" ),
            f_sender_hostname( "N/A" ),
            f_sender_username( "N/A" ),
            f_sender_info( new param_node() ),
            f_payload( new param_node() )
    {
        // make sure the sender_info node is filled out correctly
        f_sender_info->add( "package", new param_value( "N/A" ) );
        f_sender_info->add( "exe", new param_value( "N/A" ) );
        f_sender_info->add( "version", new param_value( "N/A" ) );
        f_sender_info->add( "commit", new param_value( "N/A" ) );
        f_sender_info->add( "hostname", new param_value( "N/A" ) );
        f_sender_info->add( "username", new param_value( "N/A" ) );

        // set the sender_info correctly for the server software
        /*
        version_global* t_version = version_global::get_instance();
        set_sender_commit( t_version->commit() );
        set_sender_version( t_version->version_str() );
        set_sender_package( t_version->package() );
        set_sender_exe( t_version->exe_name() );
        set_sender_hostname( t_version->hostname() );
        set_sender_username( t_version->username() );
        */
    }

    message::~message()
    {
        delete f_payload;
    }

    message_ptr_t message::process_envelope( amqp_envelope_ptr a_envelope, const std::string& a_queue_name )
    {
        param_node* t_msg_node = NULL;
        encoding t_encoding;
        if( a_envelope->Message()->ContentEncoding() == "application/json" )
        {
            t_encoding = encoding::json;
            t_msg_node = param_input_json::read_string( a_envelope->Message()->Body() );
        }
        else
        {
            throw dripline_error() << retcode_t::message_error_decoding_fail << "Unable to parse message with content type <" << a_envelope->Message()->ContentEncoding() << ">";
        }

        if( t_msg_node == NULL )
        {
            throw dripline_error() << retcode_t::message_error_decoding_fail << "Message body could not be parsed; skipping request";
        }

        string t_routing_key = a_envelope->RoutingKey();

        DEBUG( dlog, "Processing message:\n" <<
                 "Routing key: " << t_routing_key <<
                 *t_msg_node );

        message_ptr_t t_message;
        switch( to_msg_t( t_msg_node->get_value< uint32_t >( "msgtype" ) ) )
        {
            case msg_t::request:
            {
                request_ptr_t t_request = msg_request::create(
                        t_msg_node->node_at( "payload" ),
                        to_op_t( t_msg_node->get_value< uint32_t >( "msgop", to_uint( op_t::unknown ) ) ),
                        t_routing_key,
                        a_envelope->Message()->ReplyTo(),
                        t_encoding);
                t_request->set_routing_key_specifier( t_routing_key, a_queue_name );

                bool t_lockout_key_valid = true;
                t_request->lockout_key() = uuid_from_string( t_msg_node->get_value( "lockout_key", "" ), t_lockout_key_valid );
                t_request->set_lockout_key_valid( t_lockout_key_valid );

                t_message = t_request;
                break;
            }
            case msg_t::reply:
            {
                reply_ptr_t t_reply = msg_reply::create(
                        to_retcode_t( t_msg_node->get_value< uint32_t >( "retcode" ) ),
                        t_msg_node->get_value( "return_msg", "" ),
                        t_msg_node->node_at( "payload" ),
                        t_routing_key,
                        t_encoding);

                t_message = t_reply;
                break;
            }
            case msg_t::alert:
            {
                alert_ptr_t t_alert = msg_alert::create(
                        t_msg_node->node_at( "payload" ),
                        t_routing_key,
                        t_encoding);

                t_message = t_alert;
                break;
            }
            // TODO: handle info
            case msg_t::info:
            {
                throw dripline_error() << retcode_t::message_error_invalid_method << "message::process_envelope does not handle info messages";
                break;
            }
            default:
            {
                throw dripline_error() << retcode_t::message_error_invalid_method << "Message received with unhandled type: " << t_msg_node->get_value< uint32_t >( "msgtype" );
                break;
            }
        }

        // set message fields
        t_message->correlation_id() = a_envelope->Message()->CorrelationId();
        t_message->timestamp() = t_msg_node->get_value( "timestamp", "" );

        t_message->set_sender_info( new param_node( *(t_msg_node->node_at( "sender_info" ) ) ) );

        if( t_msg_node->has( "payload" ) )
        {
            if( (*t_msg_node)[ "payload" ].is_node() )
            {
                t_message->set_payload( new param_node( *(t_msg_node->node_at( "payload" ) ) ) );
            }
            else
            {
                WARN( dlog, "Non-node payload is present; it will be ignored" );
                t_message->set_payload( new param_node() );
            }
        }
        else
        {
            t_message->set_payload( new param_node() );
        }

        return t_message;
    }

    amqp_message_ptr message::create_amqp_message() const
    {
        string t_body;
        if( ! encode_message_body( t_body ) )
        {
            ERROR( dlog, "Unable to encode message body" );
            return amqp_message_ptr();
        }

        amqp_message_ptr t_message = AmqpClient::BasicMessage::Create( t_body );
        t_message->ContentEncoding( interpret_encoding() );
        t_message->CorrelationId( f_correlation_id );
        this->derived_modify_amqp_message( t_message );

        return t_message;
    }

    bool message::encode_message_body( std::string& a_body ) const
    {
        param_node t_body_node;
        t_body_node.add( "msgtype", param_value( to_uint( message_type() ) ) );
        t_body_node.add( "timestamp", param_value( scarab::get_absolute_time_string() ) );
        t_body_node.add( "sender_info", new param_node( *f_sender_info ) );
        t_body_node.add( "payload", f_payload->clone() ); // use a clone of f_payload

        if( ! this->derived_modify_message_body( t_body_node ) )
        {
            ERROR( dlog, "Something went wrong in the derived-class modify_body_message function" );
            return false;
        }

        switch( f_encoding )
        {
            case encoding::json:
                if( ! param_output_json::write_string( t_body_node, a_body, param_output_json::k_compact ) )
                {
                    ERROR( dlog, "Could not convert message body to string" );
                    return false;
                }
                return true;
                break;
            default:
                ERROR( dlog, "Cannot encode using <" << interpret_encoding() << "> (" << f_encoding << ")" );
                return false;
                break;
        }
        // should not get here
        return false;
    }

    string message::interpret_encoding() const
    {
        switch( f_encoding )
        {
            case encoding::json:
                return std::string( "application/json" );
                break;
            default:
                return std::string( "Unknown" );
        }
    }



    //***********
    // Request
    //***********

    msg_request::msg_request() :
            message(),
            f_lockout_key( generate_nil_uuid() ),
            f_lockout_key_valid( true ),
            f_message_op( op_t::unknown )
    {
        f_correlation_id = string_from_uuid( generate_random_uuid() );
    }

    msg_request::~msg_request()
    {

    }

    request_ptr_t msg_request::create( param_node* a_payload, op_t a_msg_op, const std::string& a_routing_key, const std::string& a_reply_to, message::encoding a_encoding )
    {
        request_ptr_t t_request = make_shared< msg_request >();
        t_request->set_payload( a_payload );
        t_request->set_message_op( a_msg_op );
        t_request->routing_key() = a_routing_key;
        t_request->reply_to() = a_reply_to;
        t_request->set_encoding( a_encoding );
        return t_request;
    }

    msg_t msg_request::s_message_type = msg_t::request;

    msg_t msg_request::message_type() const
    {
        return msg_request::s_message_type;
    }


    //*********
    // Reply
    //*********

    msg_reply::msg_reply() :
            message(),
            f_return_code( retcode_t::success ),
            f_return_msg(),
            f_return_buffer()
    {
    }

    msg_reply::~msg_reply()
    {

    }

    reply_ptr_t msg_reply::create( retcode_t a_retcode, const std::string& a_ret_msg, param_node* a_payload, const std::string& a_routing_key, message::encoding a_encoding )
    {
        reply_ptr_t t_reply = make_shared< msg_reply >();
        t_reply->set_return_code( a_retcode );
        t_reply->return_msg() = a_ret_msg;
        t_reply->set_payload( a_payload );
        t_reply->routing_key() = a_routing_key;
        t_reply->set_encoding( a_encoding );
        return t_reply;
    }

    reply_ptr_t msg_reply::create( const dripline_error& a_error, const std::string& a_routing_key, message::encoding a_encoding )
    {
        reply_ptr_t t_reply = make_shared< msg_reply >();
        t_reply->set_return_code( a_error.retcode() );
        t_reply->return_msg() = a_error.what();
        t_reply->set_payload( new param_node() );
        t_reply->routing_key() = a_routing_key;
        t_reply->set_encoding( a_encoding );
        return t_reply;
    }

    msg_t msg_reply::s_message_type = msg_t::reply;

    msg_t msg_reply::message_type() const
    {
        return msg_reply::s_message_type;
    }


    //*********
    // Alert
    //*********

    alert_ptr_t msg_alert::create( param_node* a_payload, const std::string& a_routing_key, message::encoding a_encoding )
    {
        alert_ptr_t t_alert = make_shared< msg_alert >();
        t_alert->set_payload( a_payload );
        t_alert->routing_key() = a_routing_key;
        t_alert->set_encoding( a_encoding );
        return t_alert;
    }

    msg_alert::msg_alert() :
            message()
    {
        f_correlation_id = string_from_uuid( generate_random_uuid() );
    }

    msg_alert::~msg_alert()
    {

    }

    msg_t msg_alert::s_message_type = msg_t::alert;

    msg_t msg_alert::message_type() const
    {
        return msg_alert::s_message_type;
    }


    //********
    // Info
    //********

    msg_info::msg_info() :
            message()
    {
        f_correlation_id = string_from_uuid( generate_random_uuid() );
    }

    msg_info::~msg_info()
    {

    }

    msg_t msg_info::s_message_type = msg_t::info;

    msg_t msg_info::message_type() const
    {
        return msg_info::s_message_type;
    }



    std::ostream& operator<<( std::ostream& a_os, message::encoding a_enc )
    {
        static std::map< message::encoding, string > s_enc_strings = { { message::encoding::json, "json" } };
        return a_os << s_enc_strings[ a_enc ];
    }


} /* namespace mantis */
