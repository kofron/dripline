#ifndef DRIPLINE_AMQP_RELAYER_HH_
#define DRIPLINE_AMQP_RELAYER_HH_


#include "scarab_api.hh"

#include "service.hh"

#include "member_variables.hh"

#include <atomic>
#include <string>

namespace scarab
{
    class param_node;
}

namespace dripline
{
    using std::atomic_bool;
    using std::string;

    using dripline::request_ptr_t;
    using dripline::alert_ptr_t;
    using dripline::info_ptr_t;
    using dripline::message_ptr_t;

    class SCARAB_API relayer : public service
    {
        public:
            relayer();
            virtual ~relayer();

        public:
            //*****************
            // thread functions
            //*****************

            /// main thread execution function: send any messages that are submitted via the send functions
            void execute_relayer();

            /// asynchronous cancel
            void cancel_relayer();

        public:
            //********************************
            // asynchronous message submission
            //********************************

            struct cc_receive_reply_pkg : service::receive_reply_pkg
            {
                mutable boost::mutex f_mutex;
                mutable boost::condition_variable f_condition_var;
                cc_receive_reply_pkg& operator=( service::receive_reply_pkg& a_orig )
                {
                    // not thread safe
                    f_channel = a_orig.f_channel;
                    f_consumer_tag = a_orig.f_consumer_tag;
                    f_successful_send = a_orig.f_successful_send;
                    return *this;
                }
            };
            typedef shared_ptr< cc_receive_reply_pkg > cc_rr_pkg_ptr;

            cc_rr_pkg_ptr send_async( request_ptr_t a_request );
            bool send_async( alert_ptr_t a_alert );
            bool send_async( info_ptr_t a_info );

            /// Wait for a reply message
            /// If the timeout is <= 0 ms, there will be no timeout
            /// This function can be called multiple times to receive multiple replies
            /// The optional bool argument a_chan_valid will return whether or not the channel is still valid for use
            virtual reply_ptr_t wait_for_reply( const cc_rr_pkg_ptr a_receive_reply, int a_timeout_ms = 0 ) const;
            virtual reply_ptr_t wait_for_reply( const cc_rr_pkg_ptr a_receive_reply, bool& a_chan_valid, int a_timeout_ms = 0 ) const;

        public:
            mv_referrable( string, request_exchange );
            mv_referrable( string, alert_exchange );
            mv_referrable( string, info_exchange );

        private:
            struct message_and_reply
            {
                message_ptr_t f_message;
                cc_rr_pkg_ptr f_receive_reply;
            };
            typedef shared_ptr< message_and_reply > mar_ptr;
            typedef boost::unique_lock< boost::mutex > scoped_lock;

            scarab::concurrent_queue< mar_ptr > f_queue;

            atomic_bool f_canceled;


    };

}

#endif
