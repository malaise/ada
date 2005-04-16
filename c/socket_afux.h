#ifndef __SOCKET_AFUX_H__
#define __SOCKET_AFUX_H__
/* AFUNIX socket implementation */
/* Map standard socket calls into our own AFUX conventions */
#include "socket_net.h"

/* Bind to a afux port */
extern int sun_bind (int fd, const struct sockaddr *addr, socklen_t addrlen);

/* Accept connection on a afux port */
extern int sun_accept(int fd, struct sockaddr *addr, socklen_t *addrlen);

/* Connect to a afux port */
extern int sun_connect(int fd, const struct sockaddr *addr, socklen_t addrlen);

/* Delete file of afux socket */
extern void sun_delete (const struct sockaddr *addr);

#endif

