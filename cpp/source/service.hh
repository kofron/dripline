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
using std::string;

#include <vector>
using std::vector;

namespace dripline
{

    class service
    {
        public:
            service( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_queue_name = "" );
            virtual ~service();

        public:
            bool start( bool authenticate = true );

            bool listen();

            bool send( /* . . . */ );

            bool stop();

        protected:
            virtual bool on_request_message( request_ptr_t a_request );
            virtual bool on_reply_message( reply_ptr_t a_reply );
            virtual bool on_alert_message( alert_ptr_t a_alert );
            virtual bool on_info_message( info_ptr_t a_info );

        private:
            bool open_channel( bool authenticate );

            bool setup_exchange();

            bool setup_queue();

            bool bind_keys( const vector< string >& a_keys );

            bool start_consuming();

            bool stop_consuming();

        public:
            mv_referrable_const( string, address );
            mv_accessible_noset( unsigned, port );
            mv_referrable_const( string, exchange );
            mv_referrable_const( string, queue_name );

            mv_referrable_const( amqp_channel_ptr, channel );

            mv_referrable_const( string, consumer_tag );

            mv_referrable( vector< string >, keys );
    };

} /* namespace dripline */

#endif /* DRIPLINE_SERVICE_HH_ */
