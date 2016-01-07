/*
 * request_hub.hh
 *
 *  Created on: Jan 7, 2016
 *      Author: nsoblath
 */

#ifndef DRIPLINE_REQUEST_HUB_HH_
#define DRIPLINE_REQUEST_HUB_HH_

#include "service.hh"

using std::weak_ptr;

namespace dripline
{

    struct SCARAB_API reply_package
    {
        const weak_ptr< service > f_service_ptr;
        string f_reply_to;
        string f_correlation_id;
        param_node f_payload;
        reply_package( const service* a_service, request_ptr_t a_request, amqp_channel_ptr a_channel ) :
            f_service_ptr( a_service ),
            f_reply_to( a_request->reply_to() ),
            f_correlation_id( a_request->correlation_id() ),
            f_payload()
        {}
        bool send_reply( retcode_t a_return_code, const std::string& a_return_msg );
    };


    class SCARAB_API request_hub : public service
    {
        public:
            request_hub( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_queue_name = "", bool a_authenticate = true );
            virtual ~request_hub();

        protected:
            /// Handle request messages
            virtual bool on_request_message( request_ptr_t a_request );

    };

} /* namespace dripline */

#endif /* DRIPLINE_REQUEST_HUB_HH_ */
