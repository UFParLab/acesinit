#ifndef _F77_NAME_H_
#define _F77_NAME_H_

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define F77_NAME(name, NAME) F77_FUNC(name, NAME)
#define F77_CB_NAME(name, NAME) F77_FUNC(name, NAME)

#ifdef OLD_MANGLING
#ifdef C_UPPER
#      define F77_NAME(name,NAME) NAME
#else
#   ifdef C_SUFFIX
#      define F77_NAME(name,NAME) name##_
#   else
#      define F77_NAME(name,NAME) name
#   endif /* C_SUFFIX */
#endif /* C_UPPER */

#ifdef CB_UPPER
#      define F77_CB_NAME(name,NAME) NAME
#else
#   ifdef CB_SUFFIX
#      define F77_CB_NAME(name,NAME) name##_
#   else
#      define F77_CB_NAME(name,NAME) name
#   endif /* CB_SUFFIX */
#endif /* CB_UPPER */
#endif /*OLD_MANGLING */

#endif /* _F77_NAME_H_ */
