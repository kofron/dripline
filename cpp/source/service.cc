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

    service::service( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_name ) :
            f_address( a_address ),
            f_port( a_port ),
            f_exchange( a_exchange ),
            f_name( a_name ),
            f_channel( NULL ),
            f_consumer_tag()
    {
    }

    service::~service()
    {
    }

    bool service::initialize( const vector< string >& a_keys, bool authenticate )
    {
        if( ! open_channel( authenticate ) ) return false;

        if( ! setup_exchange() ) return false;

        if( ! bind_keys( a_keys ) ) return false;

        if( ! start_consuming() ) return false;

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
            ERROR( dlog, "AMQP library exception caught while declaring exchagne: (" << e.ErrorCode() << ") " << e.what() );
            return false;
        }
    }

    bool service::bind_keys( const vector< string >& a_keys )
    {

    }

    bool service::start_consuming()
    {
        try
        {
            DEBUG( dlog, "Starting to consumer messages" );
            // second bool is setting no_ack to false
            f_consumer_tag = f_channel->BasicConsume( f_name, "", true, false );
            return true;
        }
        catch( amqp_exception& e )
        {
            ERROR( dlog, "AMQP exception caught while declaring exchange: (" << e.reply_code() << ") " << e.reply_text() );
            return false;
        }
        catch( amqp_lib_exception& e )
        {
            ERROR( dlog, "AMQP library exception caught while declaring exchagne: (" << e.ErrorCode() << ") " << e.what() );
            return false;
        }

    }


} /* namespace dripline */
