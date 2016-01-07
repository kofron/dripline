/*
 * service.cc
 *
 *  Created on: Jan 5, 2016
 *      Author: nsoblath
 */

#include "service.hh"

#include "authentication.hh"
#include "logger.hh"

using scarab::authentication;
using scarab::param_node;
using scarab::param_value;

namespace dripline
{
    LOGGER( dlog, "service" );

    service::service( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_queue_name ) :
            f_address( a_address ),
            f_port( a_port ),
            f_exchange( a_exchange ),
            f_queue_name( a_queue_name ),
            f_channel( NULL ),
            f_consumer_tag()
    {
    }

    service::~service()
    {
    }

    bool service::start( bool authenticate )
    {
        if( ! open_channel( authenticate ) ) return false;

        if( ! setup_exchange() ) return false;

        if( ! setup_queue() ) return false;

        if( ! bind_keys( f_keys ) ) return false;

        if( ! start_consuming() ) return false;

        return true;
    }

    bool service::listen()
    {
        while( true )
        {
            INFO( dlog, "Waiting for incoming messages" );
            amqp_envelope_ptr t_envelope = f_channel->BasicConsumeMessage( f_consumer_tag );

            message* t_message = message::process_envelope( t_envelope, f_queue_name );

            if( t_message->is_request() )
            {
                msg_request* t_request = static_cast< msg_request* >( t_message );

            }
            else if( t_message->is_alert() )
            {
                msg_alert* t_alert = static_cast< msg_alert* >( t_message );

            }
            else if( t_message->is_info() )
            {
                msg_info* t_info = static_cast< msg_info* >( t_message );

            }
            else if( t_message->is_reply() )
            {
                msg_reply* t_reply = static_cast< msg_reply* >( t_message );

            }
        }
    }

    bool service::stop()
    {
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
                    return AmqpClient::Channel::ptr_t();
                }
                const param_node* t_amqp_auth = t_auth->node_at( "amqp" );
                if( t_amqp_auth == NULL || ! t_amqp_auth->has( "username" ) || ! t_amqp_auth->has( "password" ) )
                {
                    ERROR( dlog, "AMQP authentication is not available or is not complete" );
                    return AmqpClient::Channel::ptr_t();
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
            f_consumer_tag = f_channel->BasicCancel( f_consumer_tag );
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
