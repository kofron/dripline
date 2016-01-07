/*
 * service.cc
 *
 *  Created on: Jan 5, 2016
 *      Author: nsoblath
 */

#include "service.hh"

#include "dripline_error.hh"

#include "authentication.hh"
#include "logger.hh"

using scarab::authentication;
using scarab::param_node;
using scarab::param_value;

using std::static_pointer_cast;

namespace dripline
{
    LOGGER( dlog, "service" );

    service::service( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_queue_name ) :
            f_address( a_address ),
            f_port( a_port ),
            f_exchange( a_exchange ),
            f_queue_name( a_queue_name ),
            f_channel(),
            f_consumer_tag()
    {
    }

    service::~service()
    {
    }

    bool service::start( bool authenticate )
    {
        INFO( dlog, "Connecting to <" << f_address << ":" << f_port << ">" );

        if( ! open_channel( authenticate ) ) return false;

        if( ! setup_exchange() ) return false;

        if( f_queue_name.empty() ) return true;

        INFO( dlog, "Starting service on <" << f_queue_name << ">" );

        if( ! setup_queue() ) return false;

        if( ! bind_keys( f_keys ) ) return false;

        if( ! start_consuming() ) return false;

        return true;
    }

    bool service::listen()
    {
        INFO( dlog, "Listening for incoming messages on <" << f_queue_name << ">" );

        while( true )
        {
            amqp_envelope_ptr t_envelope = f_channel->BasicConsumeMessage( f_consumer_tag );

            try
            {
                message_ptr_t t_message = message::process_envelope( t_envelope, f_queue_name );

                bool t_msg_handled = true;
                if( t_message->is_request() )
                {
                    t_msg_handled = on_request_message( static_pointer_cast< msg_request >( t_message ) );
                }
                else if( t_message->is_alert() )
                {
                    t_msg_handled = on_alert_message( static_pointer_cast< msg_alert >( t_message ) );
                }
                else if( t_message->is_info() )
                {
                    t_msg_handled = on_info_message( static_pointer_cast< msg_info >( t_message ) );
                }
                else if( t_message->is_reply() )
                {
                    t_msg_handled = on_reply_message( static_pointer_cast< msg_reply >( t_message ) );
                }
                if( ! t_msg_handled )
                {
                    throw dripline_error() << retcode_t::message_error << "Unknown error while handling message";
                }
            }
            catch( dripline_error& e )
            {
                reply_ptr_t t_reply = msg_reply::create( e, t_envelope->Message()->ReplyTo(), f_queue_name, message::encoding::json );
                try
                {
                    t_reply->do_publish( f_channel, f_exchange, f_consumer_tag );
                }
                catch( amqp_exception& e )
                {
                    ERROR( dlog, "AMQP exception caught while sending reply: (" << e.reply_code() << ") " << e.reply_text() );
                }
                catch( amqp_lib_exception& e )
                {
                    ERROR( dlog, "AMQP Library Exception caught while sending reply: (" << e.ErrorCode() << ") " << e.what() );
                }
                catch( std::exception& e )
                {
                    ERROR( dlog, "Standard exception caught while sending reply: " << e.what() );
                }
            }
        }
    }

    bool service::on_request_message( request_ptr_t )
    {
        throw dripline_error() << retcode_t::message_error_invalid_method << "Base service does not handle request messages";
        return false;
    }

    bool service::on_reply_message( reply_ptr_t )
    {
        throw dripline_error() << retcode_t::message_error_invalid_method << "Base service does not handle reply messages";
        return false;
    }

    bool service::on_alert_message( alert_ptr_t )
    {
        throw dripline_error() << retcode_t::message_error_invalid_method << "Base service does not handle alert messages";
        return false;
    }

    bool service::on_info_message( info_ptr_t )
    {
        throw dripline_error() << retcode_t::message_error_invalid_method << "Base service does not handle info messages";
        return false;
    }


    bool service::stop()
    {
        INFO( dlog, "Stopping service on <" << f_queue_name << ">" );

        if( ! stop_consuming() ) return false;

        return true;
    }

    bool service::open_channel( bool authenticate )
    {
        try
        {
            string t_username = "guest";
            string t_password = "guest";
            if( authenticate )
            {
                authentication* t_auth = authentication::get_instance();
                if( ! t_auth->is_loaded() )
                {
                    ERROR( dlog, "Authentications were not loaded; create AMQP connection" );
                    f_channel = AmqpClient::Channel::ptr_t();
                    return false;
                }
                const param_node* t_amqp_auth = t_auth->node_at( "amqp" );
                if( t_amqp_auth == NULL || ! t_amqp_auth->has( "username" ) || ! t_amqp_auth->has( "password" ) )
                {
                    ERROR( dlog, "AMQP authentication is not available or is not complete" );
                    f_channel = AmqpClient::Channel::ptr_t();
                    return false;
                }
                t_username = t_amqp_auth->get_value( "username" );
                t_password = t_amqp_auth->get_value( "password" );
            }
            DEBUG( dlog, "Opening AMQP connection and creating channel to " << f_address << ":" << f_port );
            DEBUG( dlog, "Using broker authentication: " << t_username << ":" << t_password );
            f_channel = AmqpClient::Channel::Create( f_address, f_port, t_username, t_password );
            return true;
        }
        catch( amqp_exception& e )
        {
            ERROR( dlog, "AMQP exception caught while declaring exchange: (" << e.reply_code() << ") " << e.reply_text() );
            return false;
        }
        catch( amqp_lib_exception& e )
        {
            ERROR( dlog, "AMQP Library Exception caught while creating channel: (" << e.ErrorCode() << ") " << e.what() );
            return false;
        }
        catch( std::exception& e )
        {
            ERROR( dlog, "Standard exception caught while creating channel: " << e.what() );
            return false;
        }

    }

    bool service::setup_exchange()
    {
        try
        {
            DEBUG( dlog, "Declaring exchange <" << f_exchange << ">" );
            f_channel->DeclareExchange( f_exchange, AmqpClient::Channel::EXCHANGE_TYPE_TOPIC, false, false, false );
            return true;
        }
        catch( amqp_exception& e )
        {
            ERROR( dlog, "AMQP exception caught while declaring exchange: (" << e.reply_code() << ") " << e.reply_text() );
            return false;
        }
        catch( amqp_lib_exception& e )
        {
            ERROR( dlog, "AMQP library exception caught while declaring exchange: (" << e.ErrorCode() << ") " << e.what() );
            return false;
        }
    }

    bool service::setup_queue()
    {
        try
        {
            DEBUG( dlog, "Declaring queue <" << f_queue_name << ">" );
            f_channel->DeclareQueue( f_queue_name, false, false, true, true );
            return true;
        }
        catch( amqp_exception& e )
        {
            ERROR( dlog, "AMQP exception caught while declaring queue: (" << e.reply_code() << ") " << e.reply_text() );
            return false;
        }
        catch( amqp_lib_exception& e )
        {
            ERROR( dlog, "AMQP library exception caught while declaring queue: (" << e.ErrorCode() << ") " << e.what() );
            return false;
        }

    }

    bool service::bind_keys( const vector< string >& a_keys )
    {
        try
        {
            for( vector< string >::const_iterator t_key_it = a_keys.begin(); t_key_it != a_keys.end(); ++t_key_it )
            {
                f_channel->BindQueue( f_queue_name, f_exchange, *t_key_it );
            }
            return true;
        }
        catch( amqp_exception& e )
        {
            ERROR( dlog, "AMQP exception caught while declaring binding keys: (" << e.reply_code() << ") " << e.reply_text() );
            return false;
        }
        catch( amqp_lib_exception& e )
        {
            ERROR( dlog, "AMQP library exception caught while binding keys: (" << e.ErrorCode() << ") " << e.what() );
            return false;
        }
    }

    bool service::start_consuming()
    {
        try
        {
            DEBUG( dlog, "Starting to consume messages" );
            // second bool is setting no_ack to false
            f_consumer_tag = f_channel->BasicConsume( f_queue_name, "", true, false );
            return true;
        }
        catch( amqp_exception& e )
        {
            ERROR( dlog, "AMQP exception caught while starting consuming messages: (" << e.reply_code() << ") " << e.reply_text() );
            return false;
        }
        catch( amqp_lib_exception& e )
        {
            ERROR( dlog, "AMQP library exception caught while starting consuming messages: (" << e.ErrorCode() << ") " << e.what() );
            return false;
        }
    }

    bool service::stop_consuming()
    {
        try
        {
            DEBUG( dlog, "Stopping consuming messages" );
            f_channel->BasicCancel( f_consumer_tag );
            return true;
        }
        catch( amqp_exception& e )
        {
            ERROR( dlog, "AMQP exception caught while stopping consuming messages: (" << e.reply_code() << ") " << e.reply_text() );
            return false;
        }
        catch( amqp_lib_exception& e )
        {
            ERROR( dlog, "AMQP library exception caught while stopping consuming messages: (" << e.ErrorCode() << ") " << e.what() );
            return false;
        }
    }


} /* namespace dripline */
