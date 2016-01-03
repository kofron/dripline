/*
 * mt_message.hh
 *
 *  Created on: Jul 9, 2015
 *      Author: nsoblath
 */

#ifndef MT_MESSAGE_HH_
#define MT_MESSAGE_HH_

#include "mt_amqp.hh"
#include "mt_constants.hh"
#include "mt_param.hh"
#include "mt_uuid.hh"

#include <string>

namespace mantis
{

    //***********
    // Message
    //***********

    class MANTIS_API message
    {
        public:
            enum encoding
            {
                k_json,
                k_msgpack
            };

       public:
            message();
            virtual ~message();

            virtual bool is_request() const = 0;
            virtual bool is_reply() const = 0;
            virtual bool is_alert() const = 0;
            virtual bool is_info() const = 0;

        public:
            /// from AMQP to message object
            static message* process_envelope( amqp_envelope_ptr a_envelope, const std::string& a_queue_name );

            /// from message object to AMQP
            virtual bool do_publish( amqp_channel_ptr a_channel, const std::string& a_exchange, std::string& a_reply_consumer_tag ) = 0;

        protected:
            amqp_message_ptr create_amqp_message() const;

            virtual bool derived_modify_amqp_message( amqp_message_ptr t_amqp_msg ) const = 0;
            virtual bool derived_modify_message_body( param_node& a_node ) const = 0;

            bool encode_message_body( std::string& a_body ) const;
            std::string interpret_encoding() const;

        public:
            // sets routing key and mantis routing key; the latter by removing the queue name from the beginning of the routing key
            bool set_routing_keys( const std::string& a_rk, const std::string& a_queue_name);

            void set_routing_key( const std::string& a_rk );
            const std::string& get_routing_key() const;

            void set_mantis_routing_key( const std::string& a_mrk );
            const std::string& get_mantis_routing_key() const;

            void set_correlation_id( const std::string& a_id );
            const std::string& get_correlation_id() const;

            void set_encoding( encoding a_enc );
            encoding get_encoding() const;

            virtual unsigned get_message_type() const = 0;

            void set_timestamp( const std::string& a_ts );
            const std::string& get_timestamp() const;

            void set_sender_info( param_node* a_payload );
            const param_node& get_sender_info() const;
            param_node& get_sender_info();

            void set_sender_package( const std::string& a_pkg );
            const std::string& get_sender_package() const;

            void set_sender_exe( const std::string& a_exe );
            const std::string& get_sender_exe() const;

            void set_sender_version( const std::string& a_vsn );
            const std::string& get_sender_version() const;

            void set_sender_commit( const std::string& a_cmt );
            const std::string& get_sender_commit() const;

            void set_sender_hostname( const std::string& a_host );
            const std::string& get_sender_hostname() const;

            void set_sender_username( const std::string& a_user );
            const std::string& get_sender_username() const;

            void set_payload( param_node* a_payload );
            const param_node& get_payload() const;
            param_node& get_payload();

        protected:
            std::string f_routing_key;
            std::string f_mantis_routing_key;
            std::string f_correlation_id;
            encoding f_encoding;

            std::string f_timestamp;

            param_node* f_sender_info;
            std::string f_sender_package;
            std::string f_sender_exe;
            std::string f_sender_version;
            std::string f_sender_commit;
            std::string f_sender_hostname;
            std::string f_sender_username;

            param_node* f_payload;
    };


    //***********
    // Request
    //***********

    class msg_reply;

    class MANTIS_API msg_request : public message
    {
        public:
            msg_request();
            virtual ~msg_request();

            static msg_request* create( param_node* a_payload, unsigned a_msg_op, const std::string& a_routing_key, const std::string& a_queue_name, message::encoding a_encoding );

            bool is_request() const;
            bool is_reply() const;
            bool is_alert() const;
            bool is_info() const;

            msg_reply* reply( unsigned a_ret_code, const std::string& a_ret_msg ) const;

        public:
            bool do_publish( amqp_channel_ptr a_channel, const std::string& a_exchange, std::string& a_reply_consumer_tag );

        private:
            bool derived_modify_amqp_message( amqp_message_ptr t_amqp_msg ) const;
            bool derived_modify_message_body( param_node& a_node ) const;

        public:
            unsigned get_message_type() const;
            static unsigned message_type();

            void set_reply_to( const std::string& a_rt );
            const std::string& get_reply_to() const;

            void set_lockout_key( const uuid_t& a_key );
            const uuid_t& get_lockout_key() const;

            void set_lockout_key_valid( bool a_flag );
            bool get_lockout_key_valid() const;

            void set_message_op( unsigned a_op );
            unsigned get_message_op() const;

        private:
            std::string f_reply_to;
            uuid_t f_lockout_key;
            bool f_lockout_key_valid;

            static unsigned f_message_type;

            unsigned f_message_op;
    };


    //*********
    // Reply
    //*********

    class MANTIS_API msg_reply : public message
    {
        public:
            msg_reply();
            virtual ~msg_reply();

            static msg_reply* create( unsigned a_retcode, const std::string& a_ret_msg, param_node* a_payload, const std::string& a_routing_key, const std::string& a_queue_name, message::encoding a_encoding );

            bool is_request() const;
            bool is_reply() const;
            bool is_alert() const;
            bool is_info() const;

        public:
            bool do_publish( amqp_channel_ptr a_channel, const std::string& a_exchange, std::string& a_reply_consumer_tag );

        private:
            bool derived_modify_amqp_message( amqp_message_ptr t_amqp_msg ) const;
            bool derived_modify_message_body( param_node& a_node ) const;

        public:
            unsigned get_message_type() const;
            static unsigned message_type();

            void set_return_code( unsigned a_retcode );
            unsigned get_return_code() const;

            void set_return_message( const std::string& a_ret_msg );
            const std::string& get_return_message() const;

        private:
            static unsigned f_message_type;

            unsigned f_return_code;
            std::string f_return_msg;

            mutable std::string f_return_buffer;
    };

    //*********
    // Alert
    //*********

    class MANTIS_API msg_alert : public message
    {
        public:
            msg_alert();
            virtual ~msg_alert();

            static msg_alert* create( param_node* a_payload, const std::string& a_routing_key, const std::string& a_queue_name, message::encoding a_encoding );

            bool is_request() const;
            bool is_reply() const;
            bool is_alert() const;
            bool is_info() const;

        public:
            bool do_publish( amqp_channel_ptr a_channel, const std::string& a_exchange, std::string& a_reply_consumer_tag );

        private:
            bool derived_modify_amqp_message( amqp_message_ptr t_amqp_msg ) const;
            bool derived_modify_message_body( param_node& a_node ) const;

        public:
            unsigned get_message_type() const;
            static unsigned message_type();

        private:
            static unsigned f_message_type;
    };

    //********
    // Info
    //********

    class MANTIS_API msg_info : public message
    {
        public:
            msg_info();
            virtual ~msg_info();

            bool is_request() const;
            bool is_reply() const;
            bool is_alert() const;
            bool is_info() const;

        public:
            bool do_publish( amqp_channel_ptr a_channel, const std::string& a_exchange, std::string& a_reply_consumer_tag );

        private:
            bool derived_modify_amqp_message( amqp_message_ptr t_amqp_msg ) const;
            bool derived_modify_message_body( param_node& a_node ) const;

        public:
            unsigned get_message_type() const;
            static unsigned message_type();

        private:
            static unsigned f_message_type;
    };




    //***********
    // Message
    //***********

    inline bool message::set_routing_keys( const std::string& a_rk, const std::string& a_queue_name )
    {
        if( a_rk.find( a_queue_name ) != 0 ) return false;
        f_routing_key = a_rk;
        f_mantis_routing_key = a_rk;
        f_mantis_routing_key.erase( 0, a_queue_name.size() + 1 ); // 1 added to remove the '.' that separates nodes
        return true;
    }

    inline void message::set_routing_key( const std::string& a_rk )
    {
        f_routing_key = a_rk;
        return;
    }

    inline const std::string& message::get_routing_key() const
    {
        return f_routing_key;
    }

    inline void message::set_mantis_routing_key( const std::string& a_mrk )
    {
        f_mantis_routing_key = a_mrk;
        return;
    }

    inline const std::string& message::get_mantis_routing_key() const
    {
        return f_mantis_routing_key;
    }

    inline void message::set_correlation_id( const std::string& a_id )
    {
        f_correlation_id = a_id;
        return;
    }

    inline const std::string& message::get_correlation_id() const
    {
        return f_correlation_id;
    }

    inline void message::set_encoding( encoding a_enc )
    {
        f_encoding = a_enc;
        return;
    }

    inline message::encoding message::get_encoding() const
    {
        return f_encoding;
    }

    inline void message::set_timestamp( const std::string& a_ts )
    {
        f_timestamp = a_ts;
        return;
    }

    inline const std::string& message::get_timestamp() const
    {
        return f_timestamp;
    }


    inline void message::set_sender_info( param_node* a_sender_info )
    {
        delete f_sender_info;
        f_sender_info = a_sender_info;
        if( ! f_sender_info->has( "package" ) ) f_sender_info->add( "package", new param_value( "N/A" ) );
        f_sender_package = f_sender_info->get_value( "package" );
        if( ! f_sender_info->has( "exe" ) ) f_sender_info->add( "exe", new param_value( "N/A" ) );
        f_sender_exe = f_sender_info->get_value( "exe" );
        if( ! f_sender_info->has( "version" ) ) f_sender_info->add( "version", new param_value( "N/A" ) );
        f_sender_version = f_sender_info->get_value( "version" );
        if( ! f_sender_info->has( "commit" ) ) f_sender_info->add( "commit", new param_value( "N/A" ) );
        f_sender_commit = f_sender_info->get_value( "commit" );
        if( ! f_sender_info->has( "hostname" ) ) f_sender_info->add( "hostname", new param_value( "N/A" ) );
        f_sender_hostname = f_sender_info->get_value( "hostname" );
        if( ! f_sender_info->has( "username" ) ) f_sender_info->add( "username", new param_value( "N/A" ) );
        f_sender_username = f_sender_info->get_value( "username" );
    }

    inline const param_node& message::get_sender_info() const
    {
        return *f_sender_info;
    }

    inline param_node& message::get_sender_info()
    {
        return *f_sender_info;
    }

    inline void message::set_sender_package( const std::string& a_pkg )
    {
        f_sender_info->value_at( "package" )->set( a_pkg );
        f_sender_package = a_pkg;
        return;
    }

    inline const std::string& message::get_sender_package() const
    {
        return f_sender_package;
    }


    inline void message::set_sender_exe( const std::string& a_exe )
    {
        f_sender_info->value_at( "exe" )->set( a_exe );
        f_sender_exe = a_exe;
        return;
    }

    inline const std::string& message::get_sender_exe() const
    {
        return f_sender_exe;
    }


    inline void message::set_sender_version( const std::string& a_vsn )
    {
        f_sender_info->value_at( "version" )->set( a_vsn );
        f_sender_version = a_vsn;
        return;
    }

    inline const std::string& message::get_sender_version() const
    {
        return f_sender_version;
    }


    inline void message::set_sender_commit( const std::string& a_cmt )
    {
        f_sender_info->value_at( "commit" )->set( a_cmt );
        f_sender_commit = a_cmt;
        return;
    }

    inline const std::string& message::get_sender_commit() const
    {
        return f_sender_commit;
    }


    inline void message::set_sender_hostname( const std::string& a_host )
    {
        f_sender_info->value_at( "hostname" )->set( a_host );
        f_sender_hostname = a_host;
        return;
    }

    inline const std::string& message::get_sender_hostname() const
    {
        return f_sender_hostname;
    }


    inline void message::set_sender_username( const std::string& a_user )
    {
        f_sender_info->value_at( "username" )->set( a_user );
        f_sender_username = a_user;
        return;
    }

    inline const std::string& message::get_sender_username() const
    {
        return f_sender_username;
    }


    inline void message::set_payload( param_node* a_payload )
    {
        delete f_payload;
        f_payload = a_payload;
        return;
    }

    inline const param_node& message::get_payload() const
    {
        return *f_payload;
    }

    inline param_node& message::get_payload()
    {
        return *f_payload;
    }

    //***********
    // Request
    //***********

    inline bool msg_request::is_request() const
    {
        return true;
    }
    inline bool msg_request::is_reply() const
    {
        return false;
    }
    inline bool msg_request::is_alert() const
    {
        return false;
    }
    inline bool msg_request::is_info() const
    {
        return false;
    }

    inline bool msg_request::derived_modify_amqp_message( amqp_message_ptr a_amqp_msg ) const
    {
        a_amqp_msg->ReplyTo( f_reply_to );
        return true;
    }

    inline bool msg_request::derived_modify_message_body( param_node& a_node ) const
    {
        a_node.add( "msgop", new param_value( f_message_op ) );
        a_node.add( "lockout_key", new param_value( string_from_uuid( get_lockout_key() ) ) );
        return true;
    }

    inline msg_reply* msg_request::reply( unsigned a_ret_code, const std::string& a_ret_msg ) const
    {
        msg_reply* t_reply = new msg_reply();
        t_reply->set_return_code( a_ret_code );
        t_reply->set_return_message( a_ret_msg );
        t_reply->set_correlation_id( f_correlation_id );
        t_reply->set_routing_key( f_reply_to );
        return t_reply;
    }

    inline void msg_request::set_reply_to( const std::string& a_rt )
    {
        f_reply_to = a_rt;
        return;
    }

    inline const std::string& msg_request::get_reply_to() const
    {
        return f_reply_to;
    }

    inline void msg_request::set_lockout_key( const uuid_t& a_key )
    {
        f_lockout_key = a_key;
        return;
    }

    inline const uuid_t& msg_request::get_lockout_key() const
    {
        return f_lockout_key;
    }

    inline void msg_request::set_lockout_key_valid( bool a_flag )
    {
        f_lockout_key_valid = a_flag;
        return;
    }

    inline bool msg_request::get_lockout_key_valid() const
    {
        return f_lockout_key_valid;
    }

    inline void msg_request::set_message_op( unsigned a_op )
    {
        f_message_op = a_op;
        return;
    }

    inline unsigned msg_request::get_message_op() const
    {
        return f_message_op;
    }


    //*********
    // Reply
    //*********

    inline bool msg_reply::is_request() const
    {
        return false;
    }
    inline bool msg_reply::is_reply() const
    {
        return true;
    }
    inline bool msg_reply::is_alert() const
    {
        return false;
    }
    inline bool msg_reply::is_info() const
    {
        return false;
    }

    inline bool msg_reply::derived_modify_amqp_message( amqp_message_ptr /*a_amqp_msg*/ ) const
    {
        return true;
    }

    inline bool msg_reply::derived_modify_message_body( param_node& a_node ) const
    {
        a_node.add( "retcode", new param_value( f_return_code ) );
        a_node.add( "return_msg", new param_value( f_return_msg ) );
        return true;
    }

    inline void msg_reply::set_return_code( unsigned a_retcode )
    {
        f_return_code = a_retcode;
        return;
    }

    inline unsigned msg_reply::get_return_code() const
    {
        return f_return_code;
    }

    inline void msg_reply::set_return_message( const std::string& a_ret_msg )
    {
        f_return_msg = a_ret_msg;
        return;
    }

    inline const std::string& msg_reply::get_return_message() const
    {
        return f_return_msg;
    }


    //*********
    // Alert
    //*********

    inline bool msg_alert::is_request() const
    {
        return false;
    }
    inline bool msg_alert::is_reply() const
    {
        return false;
    }
    inline bool msg_alert::is_alert() const
    {
        return true;
    }
    inline bool msg_alert::is_info() const
    {
        return false;
    }

    inline bool msg_alert::derived_modify_amqp_message( amqp_message_ptr /*a_amqp_msg*/ ) const
    {
        return true;
    }

    inline bool msg_alert::derived_modify_message_body( param_node& /*a_node*/ ) const
    {
        return true;
    }



    //********
    // Info
    //********

    inline bool msg_info::is_request() const
    {
        return false;
    }
    inline bool msg_info::is_reply() const
    {
        return false;
    }
    inline bool msg_info::is_alert() const
    {
        return false;
    }
    inline bool msg_info::is_info() const
    {
        return true;
    }

    inline bool msg_info::derived_modify_amqp_message( amqp_message_ptr /*a_amqp_msg*/ ) const
    {
        return true;
    }

    inline bool msg_info::derived_modify_message_body( param_node& /*a_node*/ ) const
    {
        return true;
    }



} /* namespace mantis */

#endif /* MT_MESSAGE_HH_ */
