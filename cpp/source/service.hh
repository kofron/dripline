/*
 * service.hh
 *
 *  Created on: Jan 5, 2016
 *      Author: nsoblath
 */

#ifndef DRIPLINE_SERVICE_HH_
#define DRIPLINE_SERVICE_HH_

#include "amqp.hh"

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
            service( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_name );
            virtual ~service();

        public:
            bool initialize() = 0;

        protected:
            bool initialize( const vector< string >& a_keys, bool authenticate = true );

        private:
            bool open_channel( bool authenticate );

            bool setup_exchange();

            bool bind_keys( const vector< string >& a_keys );

            bool start_consuming();

        public:
            mv_referrable_const( string, address );
            mv_accessible_noset( unsigned, port );
            mv_referrable_const( string, exchange );
            mv_referrable_const( string, name );

            mv_referrable_const( amqp_channel_ptr, channel );

            mv_referrable_const( string, consumer_tag );
    };

} /* namespace dripline */

#endif /* DRIPLINE_SERVICE_HH_ */
