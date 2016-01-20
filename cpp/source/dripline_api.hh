/*
 * mantis_api.hh
 *
 *  Created on: Jan 1, 2016
 *      Author: nsoblath
 */

#ifndef DRIPLINE_API_HH_
#define DRIPLINE_API_HH_

#include "scarab_api.hh"

namespace dripline
{
    // API export macros for windows
#ifdef _WIN32
#  ifdef DRIPLINE_API_EXPORTS
#    define DRIPLINE_API __declspec(dllexport)
#    define DRIPLINE_EXPIMP_TEMPLATE
#  else
#    define DRIPLINE_API __declspec(dllimport)
#    define DRIPLINE_EXPIMP_TEMPLATE extern
#  endif
#else
#  define DRIPLINE_API
#endif
}

#endif /* DRIPLINE_API_HH_ */
