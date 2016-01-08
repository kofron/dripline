/*
 * service.hh
 *
 *  Created on: Jan 5, 2016
 *      Author: nsoblath
 */

#ifndef DRIPLINE_SERVICE_HH_
#define DRIPLINE_SERVICE_HH_

#include "amqp.hh"
#include "message.hh"

#include "member_variables.hh"

#include <string>
#include <set>

namespace dripline
{
    using std::string;
    using std::set;

    class service
    {
        public:
            service( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_queue_name = "", const string& a_auth_file = "" );
            virtual ~service();

        public:
            /// Creates a channel to the broker and establishes the queue for receiving messages.
            /// If no queue name was given, this does nothing.
            bool start();

            /// Starts listening on the queue for receiving messages.
            /// If no queue was created, this does nothing.
            bool listen();

            /// Stops receiving messages and closes the connection to the broker.
            /// If no queue was created, this does nothing.
            bool stop();

        public:
            /// Sends a request message and returns a channel on which to listen for a reply.
            amqp_channel_ptr send( request_ptr_t a_request, string& a_reply_consumer_tag ) const;

            /// Sends a reply message
            bool send( reply_ptr_t a_reply ) const;

            /// Sends an info message
            bool send( info_ptr_t a_info ) const;

            /// Sends an alert message
            bool send( alert_ptr_t a_alert ) const;

            /// Wait for a reply message
            /// If the timeout is <= 0 ms, there will be no timeout
            /// This function can be called multiple times to receive multiple replies
            /// The optional bool argument a_chan_valid will return whether or not the channel is still valid for use
            reply_ptr_t wait_for_reply( amqp_channel_ptr a_channel, const string& a_consumer_tag, int a_timeout_ms = 0 ) const;
            reply_ptr_t wait_for_reply( amqp_channel_ptr a_channel, const string& a_consumer_tag, bool& a_chan_valid, int a_timeout_ms = 0 ) const;

            /// Close a reply channel
            bool close_channel( amqp_channel_ptr a_channel ) const;


        protected:
            /// Default request handler; throws a dripline_error.
            /// Override this to enable handling of requests.
            virtual bool on_request_message( const request_ptr_t a_request );

            /// Default reply handler; throws a dripline_error.
            /// Override this to enable handling of replies.
            virtual bool on_reply_message( const reply_ptr_t a_reply );

            /// Default alert handler; throws a dripline_error.
            /// Override this to enable handling of alerts.
            virtual bool on_alert_message( const alert_ptr_t a_alert );

            /// Default info handler; throws a dripline_error.
            /// Override this to enable handling of infos.
            virtual bool on_info_message( const info_ptr_t a_info );

        private:
            amqp_channel_ptr open_channel() const;

            bool setup_exchange( amqp_channel_ptr a_channel, const string& a_exchange ) const;

            bool setup_queue( amqp_channel_ptr a_channel, const string& a_queue_name ) const;

            bool bind_keys( const set< string >& a_keys );

            bool start_consuming();

            /// return: if false, channel is no longer useable; if true, may be reused
            bool listen_for_message( amqp_envelope_ptr& a_envelope, amqp_channel_ptr a_channel, const string& a_consumer_tag, int a_timeout_ms = 0 ) const;

            bool stop_consuming();

            amqp_channel_ptr send_withreply( message_ptr_t a_message, string& a_reply_consumer_tag ) const;

            bool send_noreply( message_ptr_t a_message ) const;

        public:
            mv_referrable_const( string, address );
            mv_accessible_noset( unsigned, port );
            mv_referrable_const( string, username );
            mv_referrable_const( string, password );

            mv_referrable_const( string, exchange );
            mv_referrable_const( string, queue_name );

            mv_referrable_const( amqp_channel_ptr, channel );

            mv_referrable_const( string, consumer_tag );

            mv_referrable( set< string >, keys );
    };

} /* namespace dripline */

#endif /* DRIPLINE_SERVICE_HH_ */
