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

    service::service( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_queue_name, bool a_authenticate ) :
            f_address( a_address ),
            f_port( a_port ),
            f_username( "guest" ),
            f_password( "guest" ),
            f_exchange( a_exchange ),
            f_queue_name( a_queue_name ),
            f_channel(),
            f_consumer_tag()
    {
        if( a_authenticate )
        {
            authentication* t_auth = authentication::get_instance();
            if( ! t_auth->is_loaded() )
            {
                throw scarab::error() << "Authentication file was not loaded";
            }

            const param_node* t_amqp_auth = t_auth->node_at( "amqp" );
            if( t_amqp_auth == NULL || ! t_amqp_auth->has( "username" ) || ! t_amqp_auth->has( "password" ) )
            {
                throw scarab::error() << "AMQP authentication is not available or is not complete";
            }
            f_username = t_amqp_auth->get_value( "username" );
            f_password = t_amqp_auth->get_value( "password" );
        }
    }

    service::~service()
    {
    }

    bool service::start()
    {
        if( f_queue_name.empty() )
        {
            WARN( dlog, "Service requires a queue name to be started" );
            return false;
        }

        INFO( dlog, "Connecting to <" << f_address << ":" << f_port << ">" );

        f_channel = open_channel();
        if( ! f_channel ) return false;

        if( ! setup_exchange( f_channel, f_exchange ) ) return false;

        if( ! setup_queue( f_channel, f_queue_name ) ) return false;

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
                    send( t_reply );
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


    amqp_channel_ptr service::send( request_ptr_t a_request, string& a_reply_consumer_tag ) const
    {
        DEBUG( dlog, "Sending request with routing key <" << a_request->routing_key() << ">" );
        return send_withreply( static_pointer_cast< message >( a_request ), a_reply_consumer_tag );
    }

    bool service::send( reply_ptr_t a_reply ) const
    {
        DEBUG( dlog, "Sending reply with routing key <" << a_reply->routing_key() << ">" );
        return send_noreply( static_pointer_cast< message >( a_reply ) );
    }

    bool service::send( info_ptr_t a_info ) const
    {
        DEBUG( dlog, "Sending info with routing key <" << a_info->routing_key() << ">" );
        return send_noreply( static_pointer_cast< message >( a_info ) );
    }

    bool service::send( alert_ptr_t a_alert ) const
    {
        DEBUG( dlog, "Sending alert with routing key <" << a_alert->routing_key() << ">" );
        return send_noreply( static_pointer_cast< message >( a_alert ) );
    }

    amqp_channel_ptr service::send_withreply( message_ptr_t a_message, string& a_reply_consumer_tag ) const
    {
        amqp_message_ptr t_amqp_message = a_message->create_amqp_message();

        amqp_channel_ptr t_channel = open_channel();
        if( ! t_channel )
        {
            ERROR( dlog, "Unable to open channel to send a message to <" << a_message->routing_key() << "> using broker <" << f_address << ":" << f_port << ">" );
            return amqp_channel_ptr();
        }

        if( ! setup_exchange( t_channel, f_exchange ) )
        {
            ERROR( dlog, "Unable to setup the exchange <" << f_exchange << ">" );
            return amqp_channel_ptr();
        }

        // create the reply-to queue, and bind the queue to the routing key over the given exchange
        string t_reply_to = t_channel->DeclareQueue( "" );
        t_channel->BindQueue( t_reply_to, f_exchange, t_reply_to );
        a_message->reply_to() = t_reply_to;
        DEBUG( dlog, "Reply-to for request: " << t_reply_to );

        // begin consuming on the reply-to queue
        a_reply_consumer_tag = t_channel->BasicConsume( t_reply_to );
        DEBUG( dlog, "Consumer tag for reply: " << a_reply_consumer_tag );

        try
        {
            t_channel->BasicPublish( f_exchange, a_message->routing_key(), t_amqp_message, true, false );
            return t_channel;
        }
        catch( AmqpClient::MessageReturnedException& e )
        {
            ERROR( dlog, "Request message could not be sent: " << e.what() );
            return amqp_channel_ptr();
        }
        catch( std::exception& e )
        {
            ERROR( dlog, "Error publishing request to queue: " << e.what() );
            return amqp_channel_ptr();
        }
    }

    bool service::send_noreply( message_ptr_t a_message ) const
    {
        amqp_message_ptr t_amqp_message = a_message->create_amqp_message();

        amqp_channel_ptr t_channel = open_channel();
        if( ! t_channel )
        {
            ERROR( dlog, "Unable to open channel to send a message to <" << a_message->routing_key() << "> using broker <" << f_address << ":" << f_port << ">" );
            return false;
        }

        if( ! setup_exchange( t_channel, f_exchange ) )
        {
            ERROR( dlog, "Unable to setup the exchange <" << f_exchange << ">" );
            return false;
        }

        try
        {
            t_channel->BasicPublish( f_exchange, a_message->routing_key(), t_amqp_message, true, false );
        }
        catch( AmqpClient::MessageReturnedException& e )
        {
            ERROR( dlog, "Request message could not be sent: " << e.what() );
            return false;
        }
        catch( std::exception& e )
        {
            ERROR( dlog, "Error publishing request to queue: " << e.what() );
            return false;
        }
        return true;
    }

    bool service::stop()
    {
        INFO( dlog, "Stopping service on <" << f_queue_name << ">" );

        if( ! stop_consuming() ) return false;

        return true;
    }

    amqp_channel_ptr service::open_channel() const
    {
        try
        {
            DEBUG( dlog, "Opening AMQP connection and creating channel to " << f_address << ":" << f_port );
            DEBUG( dlog, "Using broker authentication: " << f_username << ":" << f_password );
            return AmqpClient::Channel::Create( f_address, f_port, f_username, f_password );
        }
        catch( amqp_exception& e )
        {
            ERROR( dlog, "AMQP exception caught while opening channel: (" << e.reply_code() << ") " << e.reply_text() );
            return amqp_channel_ptr();
        }
        catch( amqp_lib_exception& e )
        {
            ERROR( dlog, "AMQP Library Exception caught while creating channel: (" << e.ErrorCode() << ") " << e.what() );
            return amqp_channel_ptr();
        }
        catch( std::exception& e )
        {
            ERROR( dlog, "Standard exception caught while creating channel: " << e.what() );
            return amqp_channel_ptr();
        }
    }

    bool service::setup_exchange( amqp_channel_ptr a_channel, const string& a_exchange ) const
    {
        try
        {
            DEBUG( dlog, "Declaring exchange <" << a_exchange << ">" );
            a_channel->DeclareExchange( a_exchange, AmqpClient::Channel::EXCHANGE_TYPE_TOPIC, false, false, false );
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

    bool service::setup_queue( amqp_channel_ptr a_channel, const string& a_queue_name ) const
    {
        try
        {
            DEBUG( dlog, "Declaring queue <" << a_queue_name << ">" );
            a_channel->DeclareQueue( a_queue_name, false, false, true, true );
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
