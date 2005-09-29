#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

#include "socket_afux.h"
#include "socket.h"

/* AFUNIX socket implementation */
/* Map standard socket calls into our own AFUX conventions */
/* There are two strategies: */
/* - Either each connecting client binds to a file and the accepting server */
/*   can know this file */
/* - Or, more like in AFINET, there is no need of bind for connect and the */
/*   accepting server knows ?. This is the chosen strategy. */

/* AFUNIX needs a file: Create file as AFUX_PATH/<portnum> */
/* where portnum is on 5 digits (because port is an int16)  padded by 0 */
#define AFUX_PATH "/tmp/.socket_afux"

/* Fill afux addr from in addr */
static void in2sun (const struct sockaddr *addr, struct sockaddr_un *un_addr) {
  const struct sockaddr_in *in_addr = (const struct sockaddr_in*) addr;
  (void)  mkdir(AFUX_PATH, 0777);
  un_addr->sun_family = AF_UNIX;
  sprintf (un_addr->sun_path, "%s/%05d",
           AFUX_PATH, (int)ntohs(in_addr->sin_port));
}

/* Bind to a afux port */
extern int sun_bind (int fd, const struct sockaddr *addr, 
                     socklen_t addrlen __attribute__ ((unused)) ) {
  struct sockaddr_un un_addr;

  in2sun (addr, &un_addr);

  return bind(fd, (const struct sockaddr *)&un_addr, sizeof(un_addr));

}

/* Accept connection on a afux port */
extern int sun_accept(int fd, struct sockaddr *addr, socklen_t *addrlen) {
  int res;

  if ( (addr != NULL) && (addrlen != 0) ) {
    memset (addr, 0, *addrlen);
  }
  addrlen = 0;

  for (;;) {
    res = accept(fd, NULL, 0);
    if ( (res != -1) || (errno != EINTR) ) return res;
  }

}

/* Connect to a afux port */
extern int sun_connect(int  fd, const struct sockaddr *addr,
                       socklen_t addrlen __attribute__ ((unused)) ) {
  struct sockaddr_un un_addr;
  int res;

  in2sun (addr, &un_addr);

  for (;;) {
    res = connect(fd, (const struct sockaddr *)&un_addr, sizeof(un_addr));
    if ( (res == 0) || (errno != EINTR) ) break;
  }
  if (res != 0) {
    if (errno == ENOENT) {
      errno = ECONNREFUSED;
    }
  }
  return res;
}

/* Remove afux socket file */
extern void sun_delete (const struct sockaddr *addr) {
  struct sockaddr_un un_addr;

  in2sun (addr, &un_addr);

  (void) unlink (un_addr.sun_path);
}

