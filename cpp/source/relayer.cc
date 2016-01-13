#define DRIPLINE_API_EXPORTS

#include "relayer.hh"

#include "dripline_error.hh"

#include "logger.hh"
#include "param.hh"

namespace dripline
{
    using std::make_shared;
    using std::string;
    using std::static_pointer_cast;

    using scarab::param_node;

    LOGGER( dlog, "relayer" );

    relayer::relayer() :
            service(),
            f_request_exchange( "requests" ),
            f_alert_exchange( "alerts" ),
            f_info_exchange( "info" ),
            f_queue(),
            f_canceled( false )
    {
    }

    relayer::~relayer()
    {
    }

    void relayer::execute_relayer()
    {
        DEBUG( dlog, "Dripline relayer starting" );
        while( ! f_canceled.load() )
        {
            mar_ptr t_mar;
            bool t_have_message = f_queue.timed_wait_and_pop( t_mar ); // blocking call for next message to send; timed so that cancellation can be rechecked
            scoped_lock lock( t_mar->f_receive_reply->f_mutex );
            if( ! t_have_message ) continue;

            switch( t_mar->f_message->message_type() )
            {
                case msg_t::request:
                    *t_mar->f_receive_reply = *service::send( static_pointer_cast< dripline::msg_request >( t_mar->f_message ), f_request_exchange );
                    t_mar->f_receive_reply->f_condition_var.notify_one();
                    break;
                case msg_t::alert:
                    if( ! service::send( static_pointer_cast< dripline::msg_alert >( t_mar->f_message ), f_alert_exchange ) )
                    {
                        WARN( dlog, "Unable to send alert" );
                    }
                    break;
                case msg_t::info:
                    if( ! send( static_pointer_cast< dripline::msg_info >( t_mar->f_message ), f_info_exchange ) )
                    {
                        WARN( dlog, "Unable to send info" );
                    }
                    break;
                default:
                    WARN( dlog, "Unsupported message type: " << t_mar->f_message->message_type() );
                    break;
            }
        }

        DEBUG( dlog, "Exiting the Dripline relayer" );

        return;
    }


    void relayer::cancel_relayer()
    {
        DEBUG( dlog, "Canceling relayer" );
        f_canceled.store( true );
        f_queue.interrupt();
        return;
    }

    relayer::cc_rr_pkg_ptr relayer::send_async( request_ptr_t a_request )
    {
        if( f_canceled.load() )
        {
            WARN( dlog, "Relayer has been canceled; request not sent" );
            cc_rr_pkg_ptr t_return;
            t_return->f_successful_send = false;
            return t_return;
        }
        DEBUG( dlog, "Sending request to <" << a_request->routing_key() << ">" );
        mar_ptr t_mar = make_shared< message_and_reply >();
        scoped_lock lock( t_mar->f_receive_reply->f_mutex );
        t_mar->f_message = static_pointer_cast< dripline::message >( a_request );
        t_mar->f_receive_reply = make_shared< cc_receive_reply_pkg >();
        f_queue.push( t_mar );
        return t_mar->f_receive_reply;
    }

    bool relayer::send_async( alert_ptr_t a_alert )
    {
        if( f_canceled.load() )
        {
            WARN( dlog, "Relayer has been canceled; request not sent" );
            return false;
        }
        DEBUG( dlog, "Sending request to <" << a_alert->routing_key() << ">" );
        mar_ptr t_mar = make_shared< message_and_reply >();
        t_mar->f_message = static_pointer_cast< dripline::message >( a_alert );
        f_queue.push( t_mar );
        return true;
    }

    bool relayer::send_async( info_ptr_t a_info )
    {
        if( f_canceled.load() )
        {
            WARN( dlog, "Relayer has been canceled; request not sent" );
            return false;
        }
        DEBUG( dlog, "Sending request to <" << a_info->routing_key() << ">" );
        mar_ptr t_mar = std::make_shared< message_and_reply >();
        t_mar->f_message = static_pointer_cast< dripline::message >( a_info );
        f_queue.push( t_mar );
        return true;
    }

    reply_ptr_t relayer::wait_for_reply( const cc_rr_pkg_ptr a_receive_reply, int a_timeout_ms ) const
    {
        bool t_temp;
        return wait_for_reply( a_receive_reply, t_temp, a_timeout_ms );
    }

    reply_ptr_t relayer::wait_for_reply( const cc_rr_pkg_ptr a_receive_reply, bool& a_chan_valid, int a_timeout_ms ) const
    {
        scoped_lock lock( a_receive_reply->f_mutex );
        // TODO: wait on condition (timed?)
        return service::wait_for_reply( static_pointer_cast< receive_reply_pkg >( a_receive_reply ), a_chan_valid, a_timeout_ms );
    }

}
