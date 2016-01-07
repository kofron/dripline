
#define SCARAB_API_EXPORTS

#include "dripline_error.hh"

namespace dripline
{

    dripline_error::dripline_error() :
            ::std::exception(),
            f_error( "" ),
            f_retcode( retcode_t::success )
    {
    }

    dripline_error::dripline_error( const dripline_error& an_error ) :
            std::exception(),
            f_error( an_error.f_error.str() ),
            f_retcode( an_error.f_retcode )
    {
    }

    dripline_error::~dripline_error() throw ()
    {
    }

    const char* dripline_error::what() const throw ()
    {
        return f_error.str().c_str();
    }

}
