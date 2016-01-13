/*
 * hub.hh
 *
 *  Created on: Jan 7, 2016
 *      Author: nsoblath
 */

#ifndef DRIPLINE_HUB_HH_
#define DRIPLINE_HUB_HH_

#include "service.hh"

#include "dripline_error.hh"

namespace dripline
{
    using std::weak_ptr;

    class DRIPLINE_API hub : public service
    {
        public:
            struct reply_package
            {
                const weak_ptr< service > f_service_ptr;
                string f_reply_to;
                string f_correlation_id;
                param_node f_payload;
                reply_package( const shared_ptr< service >&  a_service, request_ptr_t a_request ) :
                    f_service_ptr( a_service ),
                    f_reply_to( a_request->reply_to() ),
                    f_correlation_id( a_request->correlation_id() ),
                    f_payload()
                {}
                bool send_reply( retcode_t a_return_code, const std::string& a_return_msg ) const;
                bool send_reply( const dripline_error& an_error ) const;
            };

        public:
            hub();
            hub( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_queue_name = "", const string& a_auth_file = "" );
            virtual ~hub();

            bool dripline_setup( const string& a_address, unsigned a_port, const string& a_exchange, const string& a_queue_name = "", const string& a_auth_file = "" );

        private:
            /// Handle request messages
            virtual bool on_request_message( const request_ptr_t a_request );

            //*****************************
            // Default request distributors
            //*****************************

            // Override the relevant function to implement use of that type of message

            virtual bool do_run_request( const request_ptr_t a_request, reply_package& a_reply_pkg );
            virtual bool do_get_request( const request_ptr_t a_request, reply_package& a_reply_pkg );
            virtual bool do_set_request( const request_ptr_t a_request, reply_package& a_reply_pkg );
            virtual bool do_cmd_request( const request_ptr_t a_request, reply_package& a_reply_pkg );

        private:
            bool __do_run_request( const request_ptr_t a_request, reply_package& a_reply_pkg );
            bool __do_get_request( const request_ptr_t a_request, reply_package& a_reply_pkg );
            bool __do_set_request( const request_ptr_t a_request, reply_package& a_reply_pkg );
            bool __do_cmd_request( const request_ptr_t a_request, reply_package& a_reply_pkg );

        private:
            //*****************
            // Request handlers
            //*****************

            bool handle_lock_request( const request_ptr_t a_request, reply_package& a_reply_pkg );
            bool handle_unlock_request( const request_ptr_t a_request, reply_package& a_reply_pkg );
            bool handle_is_locked_request( const request_ptr_t a_request, reply_package& a_reply_pkg );
            bool handle_ping_request( const request_ptr_t a_request, reply_package& a_reply_pkg );

        public:
            //******************
            // Lockout functions
            //******************

            /// enable lockout with randomly-generated key
            uuid_t enable_lockout( const param_node& a_tag );
            /// enable lockout with user-supplied key
            uuid_t enable_lockout( const param_node& a_tag, uuid_t a_key );
            bool disable_lockout( const uuid_t& a_key, bool a_force = false );

            bool is_locked() const;
            const param_node& get_lockout_tag() const;
            bool check_key( const uuid_t& a_key ) const;

        private:
            // Returns true if the server is unlocked or if it's locked and the key matches the lockout key; returns false otherwise.
            bool authenticate( const uuid_t& a_key ) const;

            param_node f_lockout_tag;
            uuid_t f_lockout_key;

    };

    inline bool hub::reply_package::send_reply( const dripline_error& an_error ) const
    {
        return send_reply( an_error.retcode(), an_error.what() );
    }

    inline uuid_t hub::enable_lockout( const param_node& a_tag )
    {
        return enable_lockout( a_tag, generate_random_uuid() );
    }

    inline bool hub::is_locked() const
    {
        return ! f_lockout_key.is_nil();
    }

    inline const param_node& hub::get_lockout_tag() const
    {
        return f_lockout_tag;
    }

    inline bool hub::check_key( const uuid_t& a_key ) const
    {
        return f_lockout_key == a_key;
    }



} /* namespace dripline */

#endif /* DRIPLINE_HUB_HH_ */
