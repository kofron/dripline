#ifndef DRIPLINE_ERROR_HH_
#define DRIPLINE_ERROR_HH_

#include <sstream>
#include <exception>

#include "dripline_constants.hh"

#include "scarab_api.hh"

namespace dripline
{

    class SCARAB_API dripline_error : public ::std::exception
    {
        public:
            dripline_error();
            dripline_error( const dripline_error& );
            ~dripline_error() throw ();

            template< class x_streamable >
            dripline_error& operator<<( const x_streamable& a_fragment )
            {
                f_error << a_fragment;
                return *this;
            }

            dripline_error& operator<<( retcode_t a_code )
            {
                f_retcode = a_code;
                return *this;
            }

            virtual const char* what() const throw();

            retcode_t retcode() const
            {
                return f_retcode;
            }

        private:
            ::std::stringstream f_error;
            retcode_t f_retcode;
    };

}

#endif /* DRIPLINE_ERROR_HH_ */
