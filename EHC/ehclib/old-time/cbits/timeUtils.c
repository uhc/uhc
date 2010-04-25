#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__) || defined(__UHC__)
/* 
 * (c) The University of Glasgow 2002
 *
 * Time Runtime Support
 */
#include "HsTime.h"

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32) /* to the end */

long *__hscore_timezone( void )
{ return &_timezone; }

char **__hscore_tzname( void )
{ return _tzname; }
#endif
#endif
