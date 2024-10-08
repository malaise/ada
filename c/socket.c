#include <sys/time.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <errno.h>
#include <ctype.h>

#include "socket_prv.h"
#include "socket_afux.h"

/* This is used to check, when closing a socket, that its fd is not */
/*  set in wait_evt */
/* Socket can be used without this feature (no check performed) */
#ifndef SOCKET_NO_EVT
#include "wait_evt.h"
#else
static int evt_fd_set (int fd __attribute__ ((unused)),
                       boolean read __attribute__ ((unused)) ) {
  return (FALSE);
}
#endif

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN NI_MAXHOST
#endif
#ifndef MAXSERVNAMELEN
#define MAXSERVNAMELEN NI_MAXSERV
#endif

#define MAX_ERROR 28
const char *errors[MAX_ERROR] = {
/* 00 */ "Ok: socket success",
/* 01 */ "Use Error: socket not or already open",
/* 02 */ "System Error: socket system error (see errno)",
/* 03 */ "Destination Error: socket destination set or not set",
/* 04 */ "Link Error: socket linked or not linked",
/* 05 */ "Connect Error: socket connected or no connected",
/* 06 */ "Broadcast Error: broadcast not allowed for this socket protocol",
/* 07 */ "Length Error: socket buffer length too small",
/* 08 */ "Reply Error: socket set_for_reply must not be set",
/* 09 */ "Tail Error: should have called socket soc_resend",
/* 10 */ "Protocol Error: invalid call for this socket protocol",
/* 11 */ "Fd Error: closing a socket while fd is set for select",
/* 12 */ "Format Error: Host, Lan or port string invalid format",
/* 13 */ "Unknown Error: unknown socket error",
/* 14 */ "Unknown Error: unknown socket error",
/* 15 */ "Unknown Error: unknown socket error",
/* 16 */ "Unknown Error: unknown socket error",
/* 17 */ "Unknown Error: unknown socket error",
/* 18 */ "Unknown Error: unknown socket error",
/* 19 */ "Unknown Error: unknown socket error",
/* 20 */ "Unknown Error: unknown socket error",
/* 21 */ "Connection Failure: socket connection refused",
/* 22 */ "Unknown name Failure: socket name not found by get_xxx_by_yyy",
/* 23 */ "Would block Failure: socket operation would block",
/* 24 */ "Connection lost Failure: socket connection lost",
/* 25 */ "Address in use Failure: socket address already in use",
/* 26 */ "Read zero Failure: socket read returns zero, disconnection?",
/* Last must be Unknown Error */
/* 27 */ "Unknown Error: unknown socket error"
};

/* String corresponding to an error code */
extern const char * soc_error (const int code) {
  int index = -code;

  if ( (index < 0) || (index >= MAX_ERROR) ) {
    index = MAX_ERROR - 1;
  }
  return errors[index];
}

typedef enum {a_class, b_class, c_class, d_class, reserved_class} net_class;

static net_class class_of (unsigned char first_byte) {

  if (first_byte <= 127) return a_class;
  if (first_byte <= 191) return b_class;
  if (first_byte <= 223) return c_class;
  if (first_byte <= 239) return d_class;
  return reserved_class;
}

static boolean is_ipm (struct sockaddr_in *addr) {
  soc_host host;
  host.integer = addr->sin_addr.s_addr;

  /* Valid ipm addr are d class */
  return (class_of(host.bytes[0]) == d_class);
}

/* Set the socket blocked mode according to blocking_mode and action */
static int set_blocked (soc_ptr soc, boolean for_read) {
  boolean setblock;
  int status;

  /* Compute target mode */
  if (for_read) {
    setblock = (soc->blocking == full_blocking);
  } else {
    setblock = (soc->blocking != non_blocking);
  }

  /* Nothing to do */
  if (soc->blocked == setblock) {
    return (SOC_OK);
  }

  /* Store state */
  soc->blocked = setblock;

  /* Get status */
  status = fcntl (soc->socket_id, F_GETFL, 0);
  if (status < 0) {
    perror("fcntl_get_block");
    return (SOC_SYS_ERR);
  }

  /* Set blocking or not */
  if (setblock) {
    status &= ~O_NONBLOCK;
  } else {
    status |= O_NONBLOCK;
  }

  /* Fcntl for having blocking ios */
  if (fcntl (soc->socket_id, F_SETFL, status)  == -1) {
    perror("fcntl_set_block");
    return (SOC_SYS_ERR);
  }

  /* Ok */
  return (SOC_OK);
}

/* Init a socket (used by open and accept) */
static int init (soc_ptr *p_soc,
                     int fd,
                     socket_protocol protocol) {
  int allow_sockopt;
  int bufsize_sockopt;
  int result;

  /* Check that socket is not already open */
  if (*p_soc != NULL) return (SOC_USE_ERR);

  /* Create structure */
  *p_soc = (soc_ptr) malloc (sizeof (soc_struct));
  if (*p_soc==NULL) {
    perror("malloc(soc_struct)");
    return (SOC_SYS_ERR);
  }
#ifdef SOCKET_MUTEX
  /* Create mutex */
  (*p_soc)->mutex = mutex_create();
  if ((*p_soc)->mutex == NULL) {
    perror("mutex_create");
    return (SOC_SYS_ERR);
  }
#endif

  /* Save protocol, tcp_kind, domain and id */
  (*p_soc)->socket_kind = protocol;
  if (protocol == udp_socket) {
    (*p_soc)->protocol = udp_protocol;
    (*p_soc)->domain = inet_domain;
    (*p_soc)->tcp_kind = tcp_raw;
  } else {
    (*p_soc)->protocol = tcp_protocol;
    if ( (protocol == tcp_socket) || (protocol == tcp_header_socket) ) {
      (*p_soc)->domain = inet_domain;
    } else {
      (*p_soc)->domain = unix_domain;
    }
    if ( (protocol == tcp_socket) || (protocol == tcp_afux_socket) ) {
      (*p_soc)->tcp_kind = tcp_raw;
    } else {
      (*p_soc)->tcp_kind = tcp_msg;
    }
  }
  (*p_soc)->socket_id = fd;

  /* Force blocking operations as default */
  (*p_soc)->blocked = FALSE;
  (*p_soc)->blocking = full_blocking;
  result = set_blocked (*p_soc, TRUE);
  if (result != SOC_OK) {
    return (result);
  }

  /* Init structures */
  if ( ((*p_soc)->protocol == tcp_protocol)
    && ((*p_soc)->domain == unix_domain) ) {
    (*p_soc)->send_struct.sin_family = AF_UNIX;
    (*p_soc)->rece_struct.sin_family = AF_UNIX;
  } else {
    (*p_soc)->send_struct.sin_family = AF_INET;
    (*p_soc)->rece_struct.sin_family = AF_INET;
  }
  /* Default TTL */
  if ( (*p_soc)->protocol == tcp_protocol) {
    (*p_soc)->ttl = 64;
  } else {
    (*p_soc)->ttl = 32;
  }
  (*p_soc)->send_struct.sin_addr.s_addr = htonl(INADDR_ANY);
  (*p_soc)->ipm_send_if.s_addr = htonl(INADDR_ANY);
  (*p_soc)->set_send_if = FALSE;
  (*p_soc)->rece_struct.sin_addr.s_addr = htonl(INADDR_ANY);
  (*p_soc)->ipm_rece_if.s_addr = htonl(INADDR_ANY);
  (*p_soc)->send_tail = NULL;
  (*p_soc)->send_len = 0;
  (*p_soc)->rece_head = NULL;
  (*p_soc)->rece_len = 0;
  (*p_soc)->expect_len = 0;
  (*p_soc)->local_port = 0;

  /* Allow UDP broadcast */
  allow_sockopt = 1;
  if ((*p_soc)->protocol == udp_protocol) {
    result = setsockopt((*p_soc)->socket_id, SOL_SOCKET, SO_BROADCAST,
                   &allow_sockopt, sizeof (allow_sockopt));
    if (result == -1) {
      perror("setsockopt(so_broadcast)");
      close ((*p_soc)->socket_id);
      free (*p_soc);
      *p_soc = NULL;
      return (SOC_SYS_ERR);
    }
  }

  /* Increase buffer size if AFUNIX */
  bufsize_sockopt = 96 * 1024;
  if ((*p_soc)->domain == unix_domain) {
    result = setsockopt((*p_soc)->socket_id, SOL_SOCKET, SO_RCVBUF,
              &bufsize_sockopt, sizeof(bufsize_sockopt));
    if (result == -1) {
      perror("setsockopt(so_rcvbuf)");
      close ((*p_soc)->socket_id);
      free (*p_soc);
      *p_soc = NULL;
      return (SOC_SYS_ERR);
    }
    result = setsockopt((*p_soc)->socket_id, SOL_SOCKET, SO_SNDBUF,
              &bufsize_sockopt, sizeof(bufsize_sockopt));
    if (result == -1) {
      perror("setsockopt(so_sndbuf)");
      close ((*p_soc)->socket_id);
      free (*p_soc);
      *p_soc = NULL;
      return (SOC_SYS_ERR);
    }
  }

  /* Allow ReuseAddr */
  result = setsockopt((*p_soc)->socket_id, SOL_SOCKET, SO_REUSEADDR,
                 &allow_sockopt, sizeof (allow_sockopt));
  if (result == -1) {
    perror("setsockopt(so_reuseaddr)");
    close ((*p_soc)->socket_id);
    free (*p_soc);
    *p_soc = NULL;
    return (SOC_SYS_ERR);
  }

#ifdef SO_REUSEPORT
  /* Allow ReusePort for UDP */
  if ((*p_soc)->protocol == udp_protocol) {
    result = setsockopt((*p_soc)->socket_id, SOL_SOCKET, SO_REUSEPORT,
                   &allow_sockopt, sizeof (allow_sockopt));
    if (result == -1) {
      perror("setsockopt(so_reuseport)");
      close ((*p_soc)->socket_id);
      free (*p_soc);
      *p_soc = NULL;
      return (SOC_SYS_ERR);
    }
  }
#endif

  /* Close on exec */
  if (fcntl((*p_soc)->socket_id, F_SETFD, FD_CLOEXEC) < 0) {
    perror("fcntl_cloexec_open");
    close ((*p_soc)->socket_id);
    free (*p_soc);
    *p_soc = NULL;
    return (SOC_SYS_ERR);
  }

  /* Ok */
  (*p_soc)->dest_set = FALSE;
  (*p_soc)->linked = FALSE;
  (*p_soc)->connection = not_connected;
  return (SOC_OK);
}

/* Open a socket */
extern int soc_open (soc_token *p_token,
                     socket_protocol protocol) {
  soc_ptr *p_soc = (soc_ptr*) p_token;
  int fd;

  /* Check that socket is not already open */
  if (*p_soc != NULL) return (SOC_USE_ERR);

  /* Stay alive on Sigpipe */
  (void) signal (SIGPIPE, SIG_IGN);

  /* Call to socket */
  if (protocol == udp_socket) {
    fd = socket(AF_INET, SOCK_DGRAM, 0);
  } else if ( (protocol == tcp_afux_socket)
           || (protocol == tcp_header_afux_socket) )  {
    fd = socket(AF_UNIX, SOCK_STREAM, 0);
  } else {
    fd = socket(AF_INET, SOCK_STREAM, 0);
  }
  if ( fd == -1) {
    perror("socket");
    free (*p_soc);
    *p_soc = NULL;
    return (SOC_SYS_ERR);
  }

  /* Init socket */
  return (init(p_soc, fd, protocol));
}

/* Close a socket */
extern int soc_close (soc_token *p_token) {
  soc_ptr *p_soc = (soc_ptr*) p_token;

  /* Check that socket is open */
  if (*p_soc == NULL) return (SOC_USE_ERR);

  /* Check that Fd is not used for select */
  if (evt_fd_set ((*p_soc)->socket_id, TRUE)
   || evt_fd_set ((*p_soc)->socket_id, FALSE) ) {
    return (SOC_FD_IN_USE);
  }
#ifdef SOCKET_MUTEX
  /* Destroy mutex */
  (void) mutex_destroy((*p_soc)->mutex);
#endif

  close ((*p_soc)->socket_id);
  /* Remove file if linked on tcp afux */
  if ( ((*p_soc)->protocol == tcp_protocol)
    && ((*p_soc)->domain == unix_domain)
    && ((*p_soc)->linked) ) {
    sun_delete( (struct sockaddr*)&(*p_soc)->rece_struct);
  }
  free (*p_soc);
  *p_soc = NULL;
  return (SOC_OK);
}

#ifdef SOCKET_MUTEX
#define LOCK (void) mutex_lock(soc->mutex, TRUE)
#define UNLOCK (void) mutex_unlock(soc->mutex)
#else
#define LOCK
#define UNLOCK
#endif

/* Gets the id of a socket (for select, ioctl ... ) */
extern int soc_get_id (soc_token token, int *p_id) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Ok */
  *p_id = soc->socket_id;
  UNLOCK;
  return (SOC_OK);
}

/* Set the socket blocking or non blocking */
/*  (for sendind and receiving) */
extern int soc_set_blocking (soc_token token, blocking_mode blocking) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Store mode */
  soc->blocking = blocking;

  UNLOCK;
  return (SOC_OK);
}

/* Is the socket in blocking mode or not */
extern int soc_get_blocking (soc_token token, blocking_mode *blocking) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  *blocking = soc->blocking;
  UNLOCK;
  return (SOC_OK);
}

/* Get socket protocol */
extern int soc_get_protocol (soc_token token, socket_protocol *protocol) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  *protocol = soc->socket_kind;
  UNLOCK;
  return (SOC_OK);
}

/* Set the ttl if possible */
static int set_ttl (soc_ptr soc) {
  int result;

  if ( (soc->socket_kind == tcp_afux_socket)
    || (soc->socket_kind == tcp_header_afux_socket) ) {
    /* No TTL on AFUX */
    return (SOC_OK);
  }

  if (soc->protocol == udp_protocol) {
    if (!soc->dest_set) {
      /* Dest must be set on udp (to know if udp or ipm) */
      return (SOC_OK);
    }
    if (is_ipm(& soc->send_struct) ) {
      result = setsockopt(soc->socket_id, IPPROTO_IP, IP_MULTICAST_TTL,
                   &(soc->ttl), sizeof (soc->ttl));
    } else {
      result = setsockopt(soc->socket_id, IPPROTO_IP, IP_TTL,
                   &(soc->ttl), sizeof (soc->ttl));
    }
  } else {
    result = setsockopt(soc->socket_id, IPPROTO_IP, IP_TTL,
                   &(soc->ttl), sizeof (soc->ttl));
  }
  if (result == -1) {
    perror("setsockopt(ip_(multicast_)ttl)");
    return (SOC_SYS_ERR);
  }
  return (SOC_OK);
}

/* Set the TTL. Socket must be either udp or tcp non afux */
/*  (otherwise SOC_PROTO_ERR) */
extern int soc_set_ttl (soc_token token, byte ttl) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  if ( (soc->socket_kind == tcp_afux_socket)
    || (soc->socket_kind == tcp_header_afux_socket) ) {
    /* No TTL on AFUX */
    UNLOCK;
    return (SOC_PROTO_ERR);
  }

  /* OK */
  soc->ttl = ttl;
  set_ttl (soc);

  UNLOCK;
  return (SOC_OK);
}

/* Get the TTL. Socket must be either udp or tcp non afux (otherwise */
/*  SOC_PROTO_ERR) */
extern int soc_get_ttl (soc_token token, byte *ttl) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  if ( (soc->socket_kind == tcp_afux_socket)
    || (soc->socket_kind == tcp_header_afux_socket) ) {
    /* No TTL on AFUX */
    UNLOCK;
    return (SOC_PROTO_ERR);
  }

  /* OK */
  *ttl = soc->ttl;

  UNLOCK;
  return (SOC_OK);
}


/* Do the connection */
static int do_connect (soc_ptr soc) {
  int result;

  /* Check that socket has dest set */
  if (! soc->dest_set) {
    return (SOC_DEST_ERR);
  }

  /* Set blocking or not */
  set_blocked (soc, FALSE);

  /* Connect */
  if (soc->domain == inet_domain) {
    do {
      result = connect (soc->socket_id,
                        (struct sockaddr*)&(soc->send_struct),
                        socklen);
    } while ( (result < 0) && (errno == EINTR) );
  } else {
    result = sun_connect (soc->socket_id,
                          (struct sockaddr*)&(soc->send_struct),
                          socklen);
  }

  /* Check result */
  if (result < 0) {
    if ( (errno == ECONNREFUSED)
      || (errno == ETIMEDOUT)
      || (errno == ENETUNREACH) ) {
      /* Not connected */
      return (SOC_CONN_REFUSED);
    } else if ( (errno == EINPROGRESS)
            ||  (errno == EWOULDBLOCK) ) {
      soc->connection = connecting;
      return (SOC_WOULD_BLOCK);
    } else if (errno == EALREADY) {
      return (SOC_CONN_ERR);
    } else {
      perror("connect");
      return (SOC_SYS_ERR);
    }
  }

  /* Ok */
  soc->connection = connected;
  return (SOC_OK);
}

/* Set ipm sending interface (when set_dest or set_for_reply) */
static int set_ipm_if (soc_ptr soc, boolean report_error) {
  int result;

  if ( !soc->set_send_if) {
    return (SOC_OK);
  }

  if ( (soc->protocol != udp_protocol)
    || (! is_ipm(& soc->send_struct) ) ) {
    return (SOC_PROTO_ERR);
  }
  /* Set interface for sending multicast */
  result = setsockopt(soc->socket_id, IPPROTO_IP, IP_MULTICAST_IF,
                 &(soc->ipm_send_if.s_addr),
                 sizeof(soc->ipm_send_if.s_addr));
  soc->set_send_if = FALSE;
  if (result == -1) {
    if (report_error) {
      perror("setsockopt(ip_multicast_if)");
    }
    return (SOC_SYS_ERR);
  }
  return (SOC_OK);
}

/* Get host IP address from its name */
static int get_host_addr (const char * name, soc_host *host) {
  int result;
  struct addrinfo hint, *info;

  /* Set hint (search criteria) */
  memset(&hint, 0, sizeof(struct addrinfo));
  hint.ai_family = AF_INET;
  hint.ai_socktype = 0;
  hint.ai_protocol = 0;
  hint.ai_flags = 0;

  /* Get host by name, copy first addr and free list */
  result = getaddrinfo(name, NULL, &hint, &info);
  if (result == 0) {
    host->integer = (((struct sockaddr_in *) info->ai_addr)->sin_addr.s_addr);
    freeaddrinfo(info);
    return (SOC_OK);
  } else {
    errno = ENOENT;
    return (SOC_NAME_NOT_FOUND);
  }
}

/* Set the IP interface for sending */
extern int soc_set_send_ipm_interface (soc_token token, const soc_host *host) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;
  if (soc->protocol != udp_protocol) {
    UNLOCK;
    return (SOC_PROTO_ERR);
  }
  /* Store for further setting (in soc_set_dest) */
  soc->ipm_send_if.s_addr = host->integer;
  soc->set_send_if = TRUE;
  UNLOCK;
  return (SOC_OK);
}

/* Bind the socket to a local port, before connecting - specify service */
extern int soc_bind_service (soc_token token, const char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct servent *serv_entry;
  struct sockaddr_in rece_struct;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Tcp inet, not linked nor dest set nor bound */
  if ( (soc->socket_kind != tcp_socket)
    && (soc->socket_kind != tcp_header_socket) ) {
      UNLOCK;
      return (SOC_PROTO_ERR);
  }
  if (soc->linked) {
    UNLOCK;
    return (SOC_LINK_ERR);
  }
  if (soc->dest_set) {
    UNLOCK;
    return (SOC_DEST_ERR);
  }
  if (soc->local_port != 0) {
    UNLOCK;
    return (SOC_BIND_ERR);
  }

  /* Read port num */
  if ((serv_entry = getservbyname(service, ns_proto[soc->protocol]))== NULL) {
    errno = ENOENT;
    UNLOCK;
    return (SOC_NAME_NOT_FOUND);
  }

  /* Bind */
  memset (&rece_struct, 0, sizeof (struct sockaddr_in));
  rece_struct.sin_port = serv_entry->s_port;
  res  = bind (soc->socket_id, (struct sockaddr*) &rece_struct, socklen);
  if (res < 0) {
    UNLOCK;
    if (errno == EADDRINUSE) {
      return (SOC_ADDR_IN_USE);
    } else {
      perror("bind");
      return (SOC_SYS_ERR);
    }
  }

  /* Done */
  soc->local_port = (soc_port) ntohs (rece_struct.sin_port);
  UNLOCK;
  return (SOC_OK);
}

/* Bind the socket to a local port, before connecting - specify port */
extern int soc_bind_port (soc_token token, soc_port port) {
  soc_ptr soc = (soc_ptr) token;
  struct sockaddr_in rece_struct;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Tcp inet, not linked nor dest set nor bound */
  if ( (soc->socket_kind != tcp_socket)
    && (soc->socket_kind != tcp_header_socket) ) {
      UNLOCK;
      return (SOC_PROTO_ERR);
  }
  if (soc->linked) {
    UNLOCK;
    return (SOC_LINK_ERR);
  }
  if (soc->dest_set) {
    UNLOCK;
    return (SOC_DEST_ERR);
  }
  if (soc->local_port != 0) {
    UNLOCK;
    return (SOC_BIND_ERR);
  }

  /* Check port num */
  if (port == 0) {
    UNLOCK;
    return (SOC_BIND_ERR);
  }

  /* Bind */
  memset (&rece_struct, 0, sizeof (struct sockaddr_in));
  rece_struct.sin_port = htons ((uint16_t)port);
  res  = bind (soc->socket_id, (struct sockaddr*) &rece_struct, socklen);
  if (res < 0) {
    UNLOCK;
    if (errno == EADDRINUSE) {
      return (SOC_ADDR_IN_USE);
    } else {
      perror("bind");
      return (SOC_SYS_ERR);
    }
  }

  /* Done */
  soc->local_port = port;
  UNLOCK;
  return (SOC_OK);
}

/* Gets the port to which is bound a socket */
extern int soc_get_bound_port (soc_token token, soc_port *p_port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Check that socket is bound */
  if (soc->local_port == 0) {
    UNLOCK;
    return (SOC_BIND_ERR);
  }

  *p_port = soc->local_port;
  UNLOCK;
  return (SOC_OK);
}


/* Set the destination host/lan name and port - specify service */
/* Broadcast if lan */
extern int soc_set_dest_name_service (soc_token token, const char *host_lan,
                                      boolean lan, const char *service) {
  soc_ptr soc = (soc_ptr) token;
  soc_host host;
  struct netent  *lan_entry;
  struct servent *serv_entry;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Not linked nor dest already set if tcp */
  if (soc->protocol == tcp_protocol) {
    if (soc->linked) {
      UNLOCK;
      return (SOC_LINK_ERR);
    } else if (soc->dest_set) {
      UNLOCK;
      return (SOC_DEST_ERR);
    }
  }

  /* Read port num */
  if ((serv_entry = getservbyname(service, ns_proto[soc->protocol]))== NULL) {
    errno = ENOENT;
    UNLOCK;
    return (SOC_NAME_NOT_FOUND);
  }

  if (! lan) {
    /* Read  IP adress of host */
    if (get_host_addr(host_lan, &host) != SOC_OK) {
      errno = ENOENT;
      UNLOCK;
      return (SOC_NAME_NOT_FOUND);
    }
    memcpy((void *) &(soc->send_struct.sin_addr.s_addr), &host, sizeof (host));
  } else {
    /* No tcp bcast */
    if (soc->protocol == tcp_protocol) {
      UNLOCK;
      return (SOC_BCAST_ERR);
    }

    /* Read IP prefix of LAN */
    if ((lan_entry = getnetbyname(host_lan)) == NULL) {
      errno = ENOENT;
      UNLOCK;
      return (SOC_NAME_NOT_FOUND);
    }
    soc->send_struct.sin_addr = inet_makeaddr(lan_entry->n_net, 0);
  }

  soc->send_struct.sin_port = serv_entry->s_port;

  /* Set ipm sending interface */
  if (set_ipm_if(soc, TRUE) != SOC_OK) {
    UNLOCK;
    return (SOC_SYS_ERR);
  }

  /* Set TTL */
  if (set_ttl (soc) != SOC_OK) {
    return (SOC_SYS_ERR);
  }

  /* Connect tcp */
  soc->dest_set = TRUE;
  if (soc->protocol == tcp_protocol) {
    res = do_connect (soc);
    UNLOCK;
    return (res);
  }

  /* Ok */
  UNLOCK;
  return (SOC_OK);
}

/* Set the destination host name and port - specify port */
/* Broadcast if lan */
extern int soc_set_dest_name_port (soc_token token, const char *host_lan,
                                   boolean lan, soc_port port) {

  soc_ptr soc = (soc_ptr) token;
  soc_host host;
  struct netent  *lan_entry;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Not linked nor dest already set if tcp */
  if (soc->protocol == tcp_protocol) {
    if (soc->linked) {
      UNLOCK;
      return (SOC_LINK_ERR);
    } else if (soc->dest_set) {
      UNLOCK;
      return (SOC_DEST_ERR);
    }
  }

  if (! lan) {
    /* Read  IP adress of host */
    if (get_host_addr(host_lan, &host) != SOC_OK) {
      errno = ENOENT;
      UNLOCK;
      return (SOC_NAME_NOT_FOUND);
    }
    memcpy((void *) &(soc->send_struct.sin_addr.s_addr), &host, sizeof (host));
  } else {
    /* No tcp bcast */
    if (soc->protocol == tcp_protocol) {
      UNLOCK;
      return (SOC_BCAST_ERR);
    }

    /* Read IP prefix of LAN */
    if ((lan_entry = getnetbyname(host_lan)) == NULL) {
      errno = ENOENT;
      UNLOCK;
      return (SOC_NAME_NOT_FOUND);
    }
    soc->send_struct.sin_addr = inet_makeaddr(lan_entry->n_net, 0);
  }

  soc->send_struct.sin_port = htons( (uint16_t)port);

  /* Set ipm sending interface */
  if (set_ipm_if(soc, TRUE) != SOC_OK) {
    UNLOCK;
    return (SOC_SYS_ERR);
  }

  /* Set TTL */
  if (set_ttl (soc) != SOC_OK) {
    return (SOC_SYS_ERR);
  }

  /* Connect tcp */
  soc->dest_set = TRUE;
  if (soc->protocol == tcp_protocol) {
    res = do_connect (soc);
    UNLOCK;
    return (res);
  }

  /* Ok */
  UNLOCK;
  return (SOC_OK);
}

extern int soc_set_dest_host_service (soc_token token, const soc_host *host,
                                      const char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct servent *serv_entry;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Not linked nor dest already set if tcp */
  if (soc->protocol == tcp_protocol) {
    if (soc->linked) {
      UNLOCK;
      return (SOC_LINK_ERR);
    } else if (soc->dest_set) {
      UNLOCK;
      return (SOC_DEST_ERR);
    }
  }

  /* Read port num */
  if ((serv_entry = getservbyname(service, ns_proto[soc->protocol]))== NULL) {
    errno = ENOENT;
    UNLOCK;
    return (SOC_NAME_NOT_FOUND);
  }

  soc->send_struct.sin_addr.s_addr = host->integer;
  soc->send_struct.sin_port = serv_entry->s_port;

  /* Set ipm sending interface */
  if (set_ipm_if(soc, TRUE) != SOC_OK) {
    UNLOCK;
    return (SOC_SYS_ERR);
  }

  /* Set TTL */
  if (set_ttl (soc) != SOC_OK) {
    return (SOC_SYS_ERR);
  }

  /* Connect tcp */
  soc->dest_set = TRUE;
  if (soc->protocol == tcp_protocol) {
    res = do_connect (soc);
    UNLOCK;
    return (res);
  }

  /* Ok */
  UNLOCK;
  return (SOC_OK);
}

extern int soc_set_dest_host_port (soc_token token, const soc_host *host,
                                   soc_port port) {
  soc_ptr soc = (soc_ptr) token;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Not linked nor dest already set if tcp */
  if (soc->protocol == tcp_protocol) {
    if (soc->linked) {
      UNLOCK;
      return (SOC_LINK_ERR);
    } else if (soc->dest_set) {
      UNLOCK;
      return (SOC_DEST_ERR);
    }
  }

  soc->send_struct.sin_addr.s_addr = host->integer;
  soc->send_struct.sin_port = htons( (uint16_t)port);

  /* Set ipm sending interface */
  if (set_ipm_if(soc, TRUE) != SOC_OK) {
    UNLOCK;
    return (SOC_SYS_ERR);
  }

  /* Set TTL */
  if (set_ttl (soc) != SOC_OK) {
    return (SOC_SYS_ERR);
  }

  /* Connect tcp */
  soc->dest_set = TRUE;
  if (soc->protocol == tcp_protocol) {
    res = do_connect (soc);
    UNLOCK;
    return (res);
  }

  /* Ok */
  UNLOCK;
  return (SOC_OK);
}

/* Change destination host_lan name (same port) */
/* Destination must have been previously set (by a set or a rece) */
/* Broadcast if lan */
extern int soc_change_dest_name (soc_token token, const char *host_lan, boolean lan) {
  soc_ptr soc = (soc_ptr) token;
  soc_host host;
  struct netent  *lan_entry;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Check not tcp */
  if (soc->protocol == tcp_protocol) {
    UNLOCK;
    return (SOC_PROTO_ERR);
  }

  /* Check that a destination is already set */
  if (!soc->dest_set) {
    UNLOCK;
    return (SOC_DEST_ERR);
  }

  if (! lan) {
    /* Read  IP adress of host */
    if (get_host_addr(host_lan, &host) != SOC_OK) {
      errno = ENOENT;
      UNLOCK;
      return (SOC_NAME_NOT_FOUND);
    }
    memcpy((void *) &(soc->send_struct.sin_addr.s_addr), &host, sizeof (host));
  } else {
    /* Read IP prefix of LAN */
    if ((lan_entry = getnetbyname(host_lan)) == NULL) {
      errno = ENOENT;
      UNLOCK;
      return (SOC_NAME_NOT_FOUND);
    }
    soc->send_struct.sin_addr = inet_makeaddr(lan_entry->n_net, 0);
  }

  /* Set ipm sending interface */
  if (set_ipm_if(soc, TRUE) != SOC_OK) {
    UNLOCK;
    return (SOC_SYS_ERR);
  }

  /* Set TTL */
  if (set_ttl (soc) != SOC_OK) {
    return (SOC_SYS_ERR);
  }


  /* Ok */
  UNLOCK;
  return (SOC_OK);
}

/* Change destination host (same port) */
extern int soc_change_dest_host (soc_token token, const soc_host *host) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Check not tcp */
  if (soc->protocol == tcp_protocol) {
    UNLOCK;
    return (SOC_PROTO_ERR);
  }

  /* Check that a destination is already set */
  if (!soc->dest_set) {
    UNLOCK;
    return (SOC_DEST_ERR);
  }

  soc->send_struct.sin_addr.s_addr = host->integer;
  /* Set ipm sending interface */
  if (set_ipm_if(soc, TRUE) != SOC_OK) {
    UNLOCK;
    return (SOC_SYS_ERR);
  }

  /* Set TTL */
  if (set_ttl (soc) != SOC_OK) {
    return (SOC_SYS_ERR);
  }


  /* Ok */
  UNLOCK;
  return (SOC_OK);
}

/* Change destination port (same host) - specify service */
extern int soc_change_dest_service (soc_token token, const char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct servent *serv_entry;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Check not tcp */
  if (soc->protocol == tcp_protocol) {
    UNLOCK;
    return (SOC_PROTO_ERR);
  }

  /* Check that a destination is already set */
  if (!soc->dest_set) {
    UNLOCK;
    return (SOC_DEST_ERR);
  }

  /* Read IP adress of port */
  if ((serv_entry = getservbyname(service, ns_proto[soc->protocol]))== NULL) {
    errno = ENOENT;
    UNLOCK;
    return (SOC_NAME_NOT_FOUND);
  }

  /* Init structure */
  soc->send_struct.sin_port = serv_entry->s_port;

  /* Ok */
  UNLOCK;
  return (SOC_OK);
}

/* Change destination port (same host) - specify port */
extern int soc_change_dest_port (soc_token token, soc_port port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Check not tcp */
  if (soc->protocol == tcp_protocol) {
    UNLOCK;
    return (SOC_PROTO_ERR);
  }

  /* Check that a destination is already set */
  if (!soc->dest_set) {
    UNLOCK;
    return (SOC_DEST_ERR);
  }

  /* Init structure */
  soc->send_struct.sin_port = htons( (uint16_t)port);

  /* Ok */
  UNLOCK;
  return (SOC_OK);
}

/* Get the destination port */
extern int soc_get_dest_port (soc_token token, soc_port *p_port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Check that a destination is already set */
  if (!soc->dest_set) {
    UNLOCK;
    return (SOC_DEST_ERR);
  }

  /* Ok */
  *p_port = (soc_port) ntohs(soc->send_struct.sin_port);
  UNLOCK;
  return (SOC_OK);
}

extern int soc_get_dest_host (soc_token token, soc_host *p_host) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Check that a destination is already set */
  if (!soc->dest_set) {
    UNLOCK;
    return (SOC_DEST_ERR);
  }

  /* Ok */
  p_host->integer = soc->send_struct.sin_addr.s_addr;
  UNLOCK;
  return (SOC_OK);
}

/* Send to a socket, the destination of which must set */
/* May return SOC_WOULD_BLOCK, then next sends have to be made */
/*  with length=0, util soc_send returns ok */
static int do_send (soc_ptr soc, soc_message message, soc_length length) {
  int cr;
  soc_header header;
  struct iovec vector[VECTOR_LEN];
  soc_length len2send;
  char* msg2send;
  int vector_len;

  /* Check that desination is set */
  if (!soc->dest_set) {
    return (SOC_DEST_ERR);
  }

  /* Connected if tcp */
  if ( (soc->protocol == tcp_protocol) && (soc->connection != connected) ) {
    return (SOC_CONN_ERR);
  }

  /* Tail / length consistency */
  if ( (soc->send_tail != NULL) && (length != 0) ) {
    return (SOC_TAIL_ERR);
  }

  if (soc->send_tail == NULL) {
    /* No previous overflow */
    if ( (soc->protocol == tcp_protocol) && (soc->tcp_kind == tcp_msg) ) {
      /* Fill tcp header and vector */
      header.magic_number = htonl(MAGIC_NUMBER);
      header.size = htonl(length);
      vector[0].iov_base = (void*)&header;
      vector[0].iov_len = sizeof(header);
      vector[1].iov_base = message;
      vector[1].iov_len = length;
      vector_len = VECTOR_LEN;
      len2send = sizeof(header) + length;
    } else {
      msg2send = (char*) message;
      len2send = length;
    }
  } else {
    /* Previous overflow: send tail */
    if ( (soc->protocol == tcp_protocol) && (soc->tcp_kind == tcp_msg) ) {
      vector[0].iov_base = soc->send_tail;
      vector[0].iov_len = soc->send_len;
      vector_len = 1;
      len2send = soc->send_len;
    } else {
      msg2send = soc->send_tail;
      len2send = soc->send_len;
    }
  }

  /* Send */
  do {
    if (soc->protocol == tcp_protocol) {
      if (soc->tcp_kind == tcp_msg) {
        /* Tcp header */
        cr = writev(soc->socket_id, vector, vector_len);
      } else {
        /* Tcp raw */
        cr = send(soc->socket_id, msg2send, (size_t)len2send, 0);
      }
    } else {
      /* Udp */
      cr = sendto(soc->socket_id, msg2send, (size_t) len2send, 0,
       (struct sockaddr*) &(soc->send_struct), socklen);
    }
  } while ( (cr == -1 ) && (errno == EINTR) );


  /* Nothing sent but not an error */
  if ( (cr == -1) && (errno == EAGAIN) ) {
    cr = 0;
  }


  /* Check result */
  if (cr == -1) {
    if ( (errno == EPIPE)
      || (errno == ECONNRESET)
      || (errno == ECONNREFUSED) ) {
      return (SOC_CONN_LOST);
    } else {
      /* Error */
      perror("send");
      return (SOC_SYS_ERR);
    }
  } else if (cr == len2send) {
    /* Everything sent */
    if (soc->send_tail != NULL) {
      free (soc->send_tail);
      soc->send_tail = NULL;
      soc->send_len = 0;
    }
    return (SOC_OK);
  } else if (soc->blocking != non_blocking) {
    /* Not blocked despite blocking send */
    return (SOC_CONN_LOST);
  } else if ((cr == 0) && (soc->protocol == udp_protocol) ) {
    /* Udp */
    return (SOC_WOULD_BLOCK);
  } else {
    /* Tcp Overflow: save tail */
    len2send  -= cr;
    msg2send = (char*)malloc ((size_t)len2send);
    if (msg2send == NULL) {
      perror("malloc(len2send)");
      return (SOC_SYS_ERR);
    }
    if ( (soc->protocol == tcp_protocol)
      && (soc->tcp_kind == tcp_msg)
      && (soc->send_tail == NULL)
      && (cr < (int)sizeof(header)) ) {

      /* First send of vector and header not completly sent */
      /* Save rest of header and all message */
      memcpy (msg2send, ((char*)&header) + cr, sizeof(header) - cr);
      memcpy (msg2send + sizeof(header) - cr, (char*)message, (size_t)length);

    } else if (soc->send_tail == NULL) {
      /* First send of vector but either header sent or no header */
      /* Save rest of message */
      if ( (soc->protocol == tcp_protocol) && (soc->tcp_kind == tcp_msg) ) {
        /* Cr = header + nbmessage. Start at cr - header */
        cr -= sizeof(header);
      }
      memcpy (msg2send , (char*)message + cr, (size_t)len2send);
    } else {
      /* We were in overflow: save rest of tail */
      memcpy (msg2send, soc->send_tail + cr, (size_t)len2send);
    }

    /* Set new tail */
    if (soc->send_tail != NULL) {
      free (soc->send_tail);
    }
    soc->send_tail = msg2send;
    soc->send_len = len2send;
    return (SOC_WOULD_BLOCK);
  }

}

/* Send to a socket, the destination of which must set */
/* May return SOC_WOULD_BLOCK, then next sends have to be made */
/*  with length=0, util soc_send returns ok */
extern int soc_send (soc_token token, soc_message message, soc_length length) {
  soc_ptr soc = (soc_ptr) token;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Set blocking or not */
  set_blocked (soc, FALSE);

  res = do_send (soc, message, length);
  UNLOCK;
  return (res);
}

/* Resend tail of previous message */
extern int soc_resend (soc_token token) {
  soc_ptr soc = (soc_ptr) token;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Set blocking or not */
  set_blocked (soc, FALSE);

  /* Check socket is in send overflow */
  if (soc->send_tail == NULL) {
    UNLOCK;
    return (SOC_TAIL_ERR);
  }

  /* Send */
  res = do_send (soc, NULL, 0);
  UNLOCK;
  return (res);
}

/* Find name of soc_host and vice versa */
extern int soc_host_name_of (const soc_host *p_host, char *host_name,
                             unsigned int host_name_len) {
  struct sockaddr_in addr;
  int result;

  /* Read name of host */
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = p_host->integer;
  result = getnameinfo((struct sockaddr*) &addr, sizeof (addr),
                       host_name, host_name_len,
                       NULL, 0, NI_NAMEREQD);
  if (result == 0) {
    /* Ok */
    return (SOC_OK);
  } else if (result == EAI_OVERFLOW) {
    return (SOC_LEN_ERR);
  } else {
    errno = ENOENT;
    return (SOC_NAME_NOT_FOUND);
  }

}

extern int soc_host_of (const char *host_name, soc_host *p_host) {

  /* Read  IP adress of host */
  return get_host_addr(host_name, p_host);
}

extern int soc_lan_name_of (const soc_host *p_lan, char *lan_name,
                             unsigned int lan_name_len) {
  struct netent *lan_entry;

  lan_entry = getnetbyaddr((uint32_t)(p_lan->integer), AF_INET);
  if (lan_entry == (struct netent *) NULL) {
    errno = ENOENT;
    return (SOC_NAME_NOT_FOUND);
  }

  if (strlen(lan_entry->n_name) >= lan_name_len) {
    return (SOC_LEN_ERR);
  }
  strcpy (lan_name, lan_entry->n_name);

  /* Ok */
  return (SOC_OK);
}

extern int soc_lan_of (const char *lan_name, soc_host *p_lan) {
  struct netent  *lan_entry;

  lan_entry = getnetbyname(lan_name);
  if (lan_entry == (struct netent *) NULL) {
    errno = ENOENT;
    return (SOC_NAME_NOT_FOUND);
  }
  memcpy((void *) &(p_lan->integer),
     (void *) &(lan_entry->n_net), sizeof(p_lan->integer));

  /* Ok */
  return (SOC_OK);
}

static protocol_list protocol_of (socket_protocol proto) {
  if (proto == udp_socket) return udp_protocol;
  else return tcp_protocol;
}

/* Find name of soc_port and vice versa */
extern int soc_port_name_of (const soc_port port,
                             const socket_protocol proto,
                             char *port_name,
                             unsigned int port_name_len) {

  struct servent *port_struct;

  /* Read name of port */
  port_struct = getservbyport((int)htons((uint16_t)port),
                              ns_proto[protocol_of(proto)]);
  if (port_struct == (struct servent *)NULL) {
    errno = ENOENT;
    return (SOC_NAME_NOT_FOUND);
  }

  if (strlen(port_struct->s_name) >= port_name_len) {
    return (SOC_LEN_ERR);
  }
  strcpy (port_name, port_struct->s_name);

  /* Ok */
  return (SOC_OK);
}

extern int soc_port_of (const char *port_name,
                        const socket_protocol proto,
                        soc_port *p_port) {

  struct servent *port_struct;

  /* Read  num of port */
  port_struct = getservbyname(port_name, ns_proto[protocol_of(proto)]);
  if (port_struct == (struct servent *)NULL) {
    errno = ENOENT;
    return (SOC_NAME_NOT_FOUND);
  }
  *p_port = (soc_port) ntohs((uint16_t)port_struct->s_port);

  /* Ok */
  return (SOC_OK);
}

/* Gets local host */
extern int soc_get_local_host_name (char *host_name,
                                    unsigned int host_name_len) {
  /* Get current host name */
  if (gethostname(host_name, host_name_len) != 0) {
    if ( (errno == ENAMETOOLONG) || (errno == EINVAL) ) {
      return (SOC_LEN_ERR);
    } else {
      perror("gethostname");
      return (SOC_NAME_NOT_FOUND);
    }
  }
  /* Ok */
  return (SOC_OK);
}

extern int soc_get_local_host_id (soc_host *p_host) {
  char hostname[MAXHOSTNAMELEN];
  int res;

  /* Get current host name */
  res = soc_get_local_host_name(hostname, sizeof(hostname));
  if (res != SOC_OK) {
    perror("gethostname of soc_get_local_host_name");
    return (res);
  }
  /* Get its addr */
  res = soc_host_of(hostname, p_host);
  if (res != SOC_OK) {
    perror("getaddrinfo of soc_host_of");
  }
  return (res);
}

/* Get current lan name (computed from local host name) */
/* lan_name must be big enough */
extern int soc_get_local_lan_name (char *lan_name, unsigned int lan_name_len) {

  char host_name[MAXHOSTNAMELEN];
  soc_host host;
  struct in_addr host_address;
  unsigned int net_mask;
  struct netent *netentp;

  if (gethostname(host_name, sizeof(host_name)) == -1) {
    errno = ENOENT;
    return (SOC_NAME_NOT_FOUND);
  }

  if (get_host_addr(host_name, &host) != SOC_OK) {
    errno = ENOENT;
    return (SOC_NAME_NOT_FOUND);
  }

  /* First of aliases */
  host_address.s_addr = host.integer;
  net_mask = inet_netof(host_address);
  if ((int)net_mask == -1) {
    perror("inet_netof");
    return (SOC_SYS_ERR);
  }

  netentp = getnetbyaddr(net_mask, AF_INET);
  if (netentp == (struct netent *) NULL) {
    return (SOC_NAME_NOT_FOUND);
  }

  if (strlen(netentp->n_name) >= lan_name_len) {
    return (SOC_LEN_ERR);
  }

  strcpy (lan_name, netentp->n_name);
  /* Ok */
  return (SOC_OK);
}

extern int soc_get_local_lan_id (soc_host *p_lan) {
  char host_name[MAXHOSTNAMELEN];
  soc_host host;
  struct in_addr host_address;

  if (gethostname(host_name, sizeof(host_name)) == -1) {
    errno = ENOENT;
    return (SOC_NAME_NOT_FOUND);
  }

  if (get_host_addr(host_name, &host) != SOC_OK) {
    errno = ENOENT;
    return (SOC_NAME_NOT_FOUND);
  }

  /* First of aliases */
  host_address.s_addr = host.integer;
  p_lan->integer = inet_netof(host_address);
  if ((int)p_lan->integer == -1) {
    perror("inet_netof");
    return (SOC_SYS_ERR);
  }

  /* Ok */
  return (SOC_OK);
}

/* Close socket preserving errno */
static void close_sock (int sock) {
  int saved_errno = errno;
  close (sock);
  errno = saved_errno;
}

/* Get all the interfaces, return the number or error */
static int get_all_ifaces (struct ifreq ifreqs[], unsigned nb_ifaces) {
  struct ifconf ifconf;
  int sock;
  int res;

  /* Get list of interfaces */
  memset (&ifconf, 0, sizeof(ifconf));
  ifconf.ifc_req = ifreqs;
  ifconf.ifc_len = nb_ifaces;
  sock = socket (AF_INET, SOCK_STREAM, 0);
  if (sock < 0) {
    perror("socket for ioctl siocgifconf");
    return (SOC_SYS_ERR);
  }
  res = ioctl (sock, SIOCGIFCONF , (char*) &ifconf);
  if (res < 0 ) {
    perror("ioctl siocgifconf");
    close_sock (sock);
    return (SOC_SYS_ERR);
  }
  close (sock);
  return ifconf.ifc_len/sizeof(struct ifreq);
}

/* Get the interface decriptor (struct ifreq) of interface denoted by host
   and mask */
static int get_iface (const soc_host *host, const soc_host *mask,
                      struct ifreq *iface) {
  struct ifreq ifreqs[MAX_NB_IFACES];
  int  nifaces, i, n;
  soc_host crit;
  struct sockaddr_in *addr;

  /* Get list of interfaces */
  nifaces = get_all_ifaces (ifreqs, MAX_NB_IFACES);
  if (nifaces < 0) {
    return nifaces;
  }

  /* Find if_host in list */
  crit.integer = host->integer & mask->integer;
  n = -1;
  for (i = 0; i < nifaces; i++) {
    addr = (struct sockaddr_in *)&ifreqs[i].ifr_addr;

    if ( (ifreqs[i].ifr_addr.sa_family == AF_INET)
      && ( (addr->sin_addr.s_addr & mask->integer) == crit.integer) ) {
      n = i;
      break;
    }
  }

  if (n == -1) {
    /* Not found */
    return (SOC_NAME_NOT_FOUND);
  } else {
    /* Found */
    memcpy (iface, &ifreqs[n], sizeof(struct ifreq));
    return (SOC_OK);
  }
}


/* Get the broadcast address for a given interface (denoted by if_host) */
extern int soc_get_bcast (const soc_host *if_host, soc_host *p_bcast_host) {
  int sock;
  soc_host ff;
  int res;
  struct ifreq iface;
  struct sockaddr_in *addr;

  /* Get descriptor for this interface */
  ff.integer = 0xFFFFFFFF;
  res = get_iface (if_host, &ff, &iface);
  if (res != SOC_OK) {
    return (res);
  }

  /* Get its broadcast address */
  /* Get the field ifr_broadaddr of the interface */
  sock = socket (AF_INET, SOCK_STREAM, 0);
  if (sock < 0) {
    perror("socket for ioctl siocgifbrdaddr");
    return (SOC_SYS_ERR);
  }
  res = ioctl (sock, SIOCGIFBRDADDR, &iface);
  if (res < 0) {
    close_sock (sock);
    perror("ioctl siocgifbrdaddr");
    return (SOC_SYS_ERR);
  }
  close (sock);

  addr = (struct sockaddr_in *)&iface.ifr_broadaddr;
  p_bcast_host->integer = addr->sin_addr.s_addr;
  return (SOC_OK);
}


/* Get the soc_host of local host on a given LAN and netmask */
extern int soc_get_host_iface (soc_host *lan, soc_host *netmask,
                               soc_host *p_host) {
  int res;
  struct ifreq iface;
  struct sockaddr_in *addr;

  /* Get descriptor for this LAN */
  res = get_iface (lan, netmask, &iface);
  if (res != SOC_OK) {
    return (res);
  }

  /* Get local its local address */
  addr = (struct sockaddr_in *)&iface.ifr_addr;
  p_host->integer = addr->sin_addr.s_addr;
  return (SOC_OK);
}


/* String "x.y.z.t" to host, and string to port conversions */
/* Parse a byte from a string, for str2host */
static boolean parse_byte (const char *str, const int start, const int stop,
                           byte *b) {

  char buffer[4];
  unsigned long val;

  /* Check 1 <= Length <= 3 */
  if ( (stop - start) < 0 || (stop - start) > 2) {
    return FALSE;
  }

  /* Copy string from start to stop and NUL terminate */
  (void) strncpy (buffer, &str[start], stop - start + 1);
  buffer[stop - start + 1] = NUL;

  /* Convert to int then to byte */
  errno = 0;
  val = strtoul (buffer, NULL, 0);
  if ( (errno != 0) || (val > 255) ) {
    return FALSE;
  }
  *b = (byte) val;
  return TRUE;
}

/* Converts a string "x.y.z.t" to host */
extern int soc_str2host (const char *str, soc_host *p_host) {
  int i;
  /* Number and indexes of dots */
  int di, ds[3];
  boolean ok;

  /* Try to parse a LAN address byte.byte.byte.byte */
  /*  otherwise consider it is a lan name */

  /* Look for a dot, count dosts and store indexes */
  /* Check it is a digit otherwise */
  /* di must be set 3 if OK */
  di = 0;
  i = 0;
  for (;;) {
    if (str[i] == '.') {
      /* Check and update dot number */
      if (di == 3) {
        /* Too many dots */
        di = 0;
        break;
      }
      /* Store this dot index */
      ds[di] = i;
      di++;
    } else if (str[i] == NUL) {
      /* Done */
      break;
    } else if (! isdigit(str[i])) {
      /* Not a dot nor a digit */
      di = 0;
      break;
    }
    i++;
  }

  /* Check Nb of dots is 3 and that dots define 4 numbers */
  ok = (di == 3);
  ok = ok && (ds[0] != 0);
  ok = ok && (ds[1] != ds[0] + 1);
  ok = ok && (ds[2] != ds[1] + 1);
  ok = ok && ((int)strlen(str)-1 != ds[2]);

  ok = ok && parse_byte (str, 0, ds[0]-1, &p_host->bytes[0]);
  ok = ok && parse_byte (str, ds[0]+1, ds[1]-1, &p_host->bytes[1]);
  ok = ok && parse_byte (str, ds[1]+1, ds[2]-1, &p_host->bytes[2]);
  ok = ok && parse_byte (str, ds[2]+1, strlen(str)-1,
                      &p_host->bytes[3]);
  /* Done */
  return (ok ? SOC_OK : SOC_FMT_ERR);
}

extern int soc_str2port (const char *str, soc_port *p_port) {
  int i;
  unsigned long val;

  /* Check length */
  if (strlen(str) > 5) {
    return SOC_FMT_ERR;
  }

  /* Check if it is digits */
  i = 0;
  for (;;) {
    if (str[i] == NUL) {
      /* Done */
      break;
    } else if (! isdigit(str[i])) {
      /* Not a digit */
      return SOC_FMT_ERR;
    }
    i++;
  }

  /* Convert to int then to short */
  errno = 0;
  val = strtoul (str, NULL, 0);
  if ( (errno != 0) || (val > 65535) ) {
    return SOC_FMT_ERR;
  }
  *p_port = (soc_port) val;
  return SOC_OK;
}

/*******************************************************************/

/* Set the IP interface for receiveing */
extern int soc_set_rece_interface (soc_token token, const soc_host *host) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;
  if (soc->domain != inet_domain) {
    UNLOCK;
    return (SOC_PROTO_ERR);
  }
  /* Store for further setting (in bind_and_co) */
  soc->rece_struct.sin_addr.s_addr = host->integer;
  UNLOCK;
  return (SOC_OK);
}

/* Bind and listen + IPM add membership */
static int bind_and_co (soc_token token, boolean dynamic) {

  soc_ptr soc = (soc_ptr) token;
  int res;
  soc_port linked_port;
  boolean do_ipm;

  /* Ipm if udp, dest_set with an ipm adress */
  /*  and same port (if not dynamic) */
  do_ipm = ( (soc->protocol == udp_protocol)
           && (soc->dest_set)
           && (is_ipm(& soc->send_struct) )
           && ( dynamic || (soc->send_struct.sin_port
                         == soc->rece_struct.sin_port) ) );

  /* Bind */
  if ( (soc->protocol == tcp_protocol) && (soc->domain == unix_domain) ) {
    /* Afux */
    res = sun_bind (soc->socket_id, (struct sockaddr*) &(soc->rece_struct),
                    socklen);
  } else if (do_ipm) {
    /* IPM: bind on the mcast address */
    res = bind (soc->socket_id, (struct sockaddr*) &(soc->send_struct),
                socklen);
  } else {
    /* TCP & UDP: Bind on the interface (set_rece_interface) or INADDR_ANY */
    res  = bind (soc->socket_id, (struct sockaddr*) &(soc->rece_struct),
                 socklen);
  }
  if (res < 0) {
    if (errno == EADDRINUSE) {
      return (SOC_ADDR_IN_USE);
    } else {
      perror("bind");
      return (SOC_SYS_ERR);
    }
  }

  /* Set linked port in rece struct (for the case of link dynamic) */
  soc->linked = TRUE;
  res = soc_get_linked_port (token, &linked_port);
  soc->linked = FALSE;
  if (res != SOC_OK) {
     return res;
  }

  /* Listen for tcp */
  if (soc->protocol == tcp_protocol) {
    if (listen (soc->socket_id, SOMAXCONN) < 0) {
      perror("listen");
      return (SOC_SYS_ERR);
    }
  } else if (do_ipm) {
    struct ip_mreq ipm_addr;
    ipm_addr.imr_multiaddr.s_addr = soc->send_struct.sin_addr.s_addr;

    if (soc->rece_struct.sin_addr.s_addr == htonl(INADDR_ANY)) {
      /* Add membership on all IPM capable interfaces */
      struct ifreq ifreqs[MAX_NB_IFACES];
      int  nifaces, i;
      struct sockaddr_in *addr;

      /* Get list of interfaces and update the flags */
      nifaces = get_all_ifaces (ifreqs, MAX_NB_IFACES);
      for (i = 0; i < nifaces; i++) {
        if (ioctl(soc->socket_id, SIOCGIFFLAGS, &ifreqs[i]) < 0) {
          perror("ioctl(siocgifflags)");
          return (SOC_SYS_ERR);
        }
        /* Interface must be up and multicast capable */
        if ( (ifreqs[i].ifr_flags & IFF_UP)
          && (ifreqs[i].ifr_flags & IFF_MULTICAST) ) {
          /* Add membership on interface: */
          addr = (struct sockaddr_in *)&ifreqs[i].ifr_addr;
          ipm_addr.imr_interface.s_addr = addr->sin_addr.s_addr;
          if (setsockopt (soc->socket_id, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                          (char*) &ipm_addr, sizeof(ipm_addr)) != 0) {
            perror("setsockopt(ip_add_membership1)");
            return (SOC_SYS_ERR);
          }
        }
      }
    } else {
      /* Add membership on interface: */
      ipm_addr.imr_interface.s_addr = soc->rece_struct.sin_addr.s_addr;
      if (setsockopt (soc->socket_id, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                      (char*) &ipm_addr, sizeof(ipm_addr)) != 0) {
        perror("setsockopt(ip_add_membership2)");
        return (SOC_SYS_ERR);
      }
    }
    /* Copy the IPM address for check in Set_For_Reply */
    soc->ipm_rece_if = soc->send_struct.sin_addr;
  }

  /* Ok */
  soc->linked = TRUE;
  return (SOC_OK);
}


/* Links the socket to a port specified by the service */
/* The socket must be open and not already linked */
extern int soc_link_service (soc_token token, const char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct servent *serv_entry;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Not linked nor already connected if tcp */
  if (soc->protocol == tcp_protocol) {
    if (soc->linked) {
      UNLOCK;
      return (SOC_LINK_ERR);
    } else if (soc->connection != not_connected) {
      UNLOCK;
      return (SOC_CONN_ERR);
    }
  }

  /* Read IP adress of port */
  if ((serv_entry = getservbyname(service, ns_proto[soc->protocol]))== NULL) {
    errno = ENOENT;
    UNLOCK;
    return (SOC_NAME_NOT_FOUND);
  }

  /* Init structure */
  soc->rece_struct.sin_port = serv_entry->s_port;

  /* Bind */
  res = bind_and_co (token, FALSE);
  UNLOCK;
  return (res);
}

/* Links the socket to a port specified by it's value */
/* The socket must be open and not already linked */
extern int soc_link_port  (soc_token token, soc_port port) {
  soc_ptr soc = (soc_ptr) token;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Not linked nor already connected if tcp */
  if (soc->protocol == tcp_protocol) {
    if (soc->linked) {
      UNLOCK;
      return (SOC_LINK_ERR);
    } else if (soc->connection != not_connected) {
      UNLOCK;
      return (SOC_CONN_ERR);
    }
  }

  /* Init structure */
  soc->rece_struct.sin_port = htons((uint16_t) port);

  /* Bind */
  res = bind_and_co (token, FALSE);
  UNLOCK;
  return (res);
}

/* Links the socket to a port dynamically choosen by the system */
/* The socket must be open and not already linked */
extern int soc_link_dynamic  (soc_token token) {
  soc_ptr soc = (soc_ptr) token;
  int res;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Not linked nor already connected if tcp */
  if (soc->protocol == tcp_protocol) {
    if (soc->domain == unix_domain) {
      /* No dynamic port in tcp afux */
      UNLOCK;
      return (SOC_PROTO_ERR);
    }
    if (soc->linked) {
      UNLOCK;
      return (SOC_LINK_ERR);
    } else if (soc->connection != not_connected) {
      UNLOCK;
      return (SOC_CONN_ERR);
    }
  }

  /* Init structure Port is not set */
  soc->rece_struct.sin_port = htons(0);

  /* Bind */
  res = bind_and_co (token, TRUE);
  UNLOCK;
  return (res);
}

/* Gets the port to which is linked a socket */
extern int soc_get_linked_port  (soc_token token, soc_port *p_port) {
  soc_ptr soc = (soc_ptr) token;
  socklen_t len = (socklen_t)socklen;
  struct sockaddr_in sock_addr;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Check that a port is already set */
  if (!soc->linked) {
    UNLOCK;
    return (SOC_LINK_ERR);
  }

  /* Get port from structure except if 0 */
  *p_port = (soc_port) ntohs((uint16_t)soc->rece_struct.sin_port);
  if (*p_port != htons(0)) {
    UNLOCK;
    return (SOC_OK);
  }

  /* Get the real port from system */
  if (getsockname (soc->socket_id,
                   (struct sockaddr*) (&sock_addr), &len) < 0) {
    perror("getsockname");
    UNLOCK;
    return (SOC_SYS_ERR);
  }

  /* Ok */
  soc->rece_struct.sin_port = sock_addr.sin_port;
  *p_port = (soc_port) ntohs((uint16_t)soc->rece_struct.sin_port);
  UNLOCK;
  return (SOC_OK);
}

/* To test if there is a message to receive and receive it */
/* Length must be initialized with the size of the buffer */
/* The socket must be open, linked in udp and not linked in tcp */
/*  After success, the socket may be ready for a send to reply */
/* No set_for_reply if tcp */
/* Returned values: */
/*  In tcp no header non blocking: */
/*   - the length of bytes read, which may be anything from 0 to length */
/*   - SOC_READ_0: disconnection? */
/*   - Error */
/*  In others: */
/*   - SOC_OK: total_len has been read */
/*   - SOC_WOULD_BLOCK: new read has to be done */
/*   - SOC_READ_0: disconnection? */
/*   - Error */

/* Receive data and appends to rece_head if needed */
/* Until total_len is read */
static int rec1 (soc_ptr soc, char *buffer, int total_len) {
  int res;
  char * addr2read;
  int len2read;

  /* How many to read and where (append to head if already underflow) */
  if (soc->rece_head == NULL) {
    addr2read = buffer;
    len2read = total_len;
  } else {
    addr2read = &(soc->rece_head[soc->rece_len]);
    len2read = total_len - soc->rece_len;
  }

  /* Receive data */
  do {
    res = recv(soc->socket_id, addr2read, (size_t)len2read, 0);
  } while ( (res == -1) && (errno == EINTR) );

  if (res == -1) {
    if (errno == EWOULDBLOCK) {
      res = 0;
    } else if ( (errno == EPIPE)
             || (errno == ECONNRESET)
             || (errno == ECONNREFUSED) ) {
      return (SOC_CONN_LOST);
    } else {
      /* Reception error */
      perror("recv");
      return (SOC_SYS_ERR);
    }
  } else if (res == 0) {
    return (SOC_READ_0);
  }

  if (res == len2read) {
    /* Done */
    if (soc->rece_head != NULL) {
      /* Return full head and clear it */
      memcpy (buffer, soc->rece_head, (size_t)total_len);
      free (soc->rece_head);
      soc->rece_head = NULL;
      soc->rece_len = 0;
      return (SOC_OK);
    } else if ( (soc->protocol == udp_protocol) || (soc->tcp_kind == tcp_msg) ) {
      return (SOC_OK);
    } else {
      /* Tcp no header */
      return (res);
    }
  } else {
    /* Underflow */
    if ( (soc->protocol == tcp_protocol) && (soc->tcp_kind == tcp_raw) ) {
      /* Not managed in tcp no header */
      return (res);
    }
    if (soc->rece_head == NULL) {
      soc->rece_len = res;
      soc->rece_head = (char*)malloc ((size_t)total_len);
      if (soc->rece_head == NULL) {
        perror("malloc(total_len)");
        return (SOC_SYS_ERR);
      }
      memcpy (soc->rece_head, buffer, (size_t)res);
    } else {
      soc->rece_len += res;
    }
    return (SOC_WOULD_BLOCK);
  }

}

/* If socket is non blocking, call rec1 directly, */
/* Else (blocking socket) call rec1 until it does not */
/* return SOC_WOULD_BLOCK (which may happen just before disconnection) */
static int rec2 (soc_ptr soc, char *buffer, int total_len) {
  int res;

  for (;;) {
    res = rec1(soc, buffer, total_len);
    if ( (soc->blocking == non_blocking) || (res != SOC_WOULD_BLOCK) ) {
      return (res);
    }
  }
}


extern int soc_receive (soc_token token,
                        soc_message message, soc_length length,
                        boolean set_for_reply) {
  soc_ptr soc = (soc_ptr) token;
  int result;
  socklen_t addr_len;
  struct sockaddr_in *from_addr;
  soc_header header;


  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Set blocking or not */
  set_blocked (soc, TRUE);

  /* Check set_for_reply and linked vs proto */
  if (soc->protocol == tcp_protocol) {
    if (set_for_reply) {
      UNLOCK;
      return (SOC_REPLY_ERR);
    }
    if (soc->linked) {
      UNLOCK;
      return (SOC_LINK_ERR);
    }
    if (soc->connection != connected) {
      UNLOCK;
      return (SOC_CONN_ERR);
    }
  } else {
    if (!soc->linked) {
      UNLOCK;
      return (SOC_LINK_ERR);
    }
  }

  /* Receive */
  if (soc->protocol == tcp_protocol) {
    if (soc->tcp_kind == tcp_msg) {
      /* Read header or data */
      if (soc->expect_len == 0) {
        /* Header not fully received yet */
        result = rec2(soc, (char *)&header, sizeof(header));
        if (result == SOC_OK) {
          /* Disconnect if invalid magic num */
          if (ntohl(header.magic_number) != MAGIC_NUMBER) {
            fprintf (stderr, "Bad magic number\n");
            UNLOCK;
            return (SOC_CONN_LOST);
          }
          /* Header is read and correct. Save expected length */
          soc->expect_len = ntohl(header.size);
        } else {
          UNLOCK;
          return (result);
        }
      }

      /* Got a header now? */
      if (soc->expect_len == 0) {
        UNLOCK;
        return (SOC_WOULD_BLOCK);
      }

      /* Check length vs size */
      if (length < soc->expect_len) {
        UNLOCK;
        return (SOC_LEN_ERR);
      }
      result = rec2(soc, (char *)message, soc->expect_len);
      if (result == SOC_OK) {
        result = soc->expect_len;
        /* Expect header next */
        soc->expect_len = 0;
      }
      UNLOCK;
      return (result);
    } else {
      /* Tcp raw */
      result = rec1(soc, (char *)message, length);
      UNLOCK;
      return (result);
    }
  }

  /* Prepare for reply or not */
  if (set_for_reply) {
    from_addr = &(soc->send_struct);
    addr_len = (socklen_t) socklen;
    /* In case of error */
    soc->dest_set = FALSE;
  } else {
    from_addr = NULL;
    addr_len = 0;
  }

  /* Udp */
  do {
    /* Recvfrom. MSG_TRUNC => real packet length returned i.o read length */
    result = recvfrom(soc->socket_id, (char *)message,
       (size_t)length, MSG_TRUNC, (struct sockaddr*) from_addr, &addr_len);
  } while ( (result == -1) && (errno == EINTR) );

  if (result < 0) {
    if (set_for_reply) {
      soc->dest_set = FALSE;
    }
    if (errno == EWOULDBLOCK) {
      UNLOCK;
      return (SOC_WOULD_BLOCK);
    } else if ( (errno == EPIPE)
             || (errno == ECONNRESET)
             || (errno == ECONNREFUSED) ) {
      UNLOCK;
      return (SOC_CONN_LOST);
    } else {
      /* Reception error */
      perror("recvfrom");
      UNLOCK;
      return (SOC_SYS_ERR);
    }
  } else {
    /* A message read, even if empty */
    if (set_for_reply) {
      /* The send_struct is already set to the sender's address */
      soc->dest_set = TRUE;
    }
    UNLOCK;
    return (result);
  }
}


/* Tcp specific calls */

/* Accept a connection. */
/* The socket must be open, tcp and linked */
/* A new socket is created (tcp) with dest set */
extern int soc_accept (soc_token token, soc_token *p_token) {
  soc_ptr soc = (soc_ptr) token;
  soc_ptr *p_soc = (soc_ptr*) p_token;

  int result;
  int fd;
  struct sockaddr from_addr;
  socklen_t len = (socklen_t) socklen;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;

  /* Set blocking or not */
  set_blocked (soc, TRUE);

  /* Check that new socket is not already open */
  if (*p_soc != NULL) {
    UNLOCK;
    return (SOC_USE_ERR);
  }

  /* Check tcp and linked */
  if (soc->protocol != tcp_protocol) {
    UNLOCK;
    return (SOC_PROTO_ERR);
  }
  if (! soc->linked) {
    UNLOCK;
    return (SOC_LINK_ERR);
  }

  /* Accept */
  if (soc->domain == inet_domain) {
    do {
      fd = accept (soc->socket_id, &from_addr, &len);
    } while ( (fd == -1) && (errno == EINTR) );
  } else {
    fd = sun_accept (soc->socket_id, &from_addr, &len);
  }

  /* Check result */
  if (fd < 0) {
    /* Accept may be blocking. Also Linux propagates pending netrwork errors */
    if ( (errno == EAGAIN) || (errno == EWOULDBLOCK)  || (errno ==  ENETDOWN)
      || (errno == EPROTO) | (errno == ENOPROTOOPT)  || (errno == EHOSTDOWN)
      || (errno == ENONET) || (errno == EHOSTUNREACH) || (errno == EOPNOTSUPP)
      || (errno == ENETUNREACH) ) {
      return SOC_WOULD_BLOCK;
    }

    perror("accept");
    UNLOCK;
    return (SOC_SYS_ERR);
  }

  /* Init socket of same kind */
  result = init (p_soc, fd, soc->socket_kind);
  if (result != SOC_OK) {
    UNLOCK;
    return (result);
  }

  /* Set dest and connected */
  memcpy (&(*p_soc)->send_struct , &from_addr, len);
  (*p_soc)->dest_set = TRUE;
  (*p_soc)->connection = connected;

  /* Set TTL */
  if (set_ttl (soc) != SOC_OK) {
    return (SOC_SYS_ERR);
  }

  /* Ok */
  UNLOCK;
  return (SOC_OK);
}

extern int soc_is_connected (soc_token token, boolean *p_connected) {
  soc_ptr soc = (soc_ptr) token;
  int res;
  struct sockaddr socname;
  socklen_t soclen;

  /* Check that socket is open and tcp */
  if (soc == NULL) return (SOC_USE_ERR);
  LOCK;
  if (soc->protocol != tcp_protocol) {
    UNLOCK;
    return (SOC_PROTO_ERR);
  }

  /* Status is known for not pending connection */
  if (soc->connection == connecting) {
    /* Pending connection */
    /* If socket is connected, we sould be able to get the peer addr */
    /* Set connection status accordingly */
    soclen = sizeof(socname);
    res = getpeername (soc->socket_id, &socname, &soclen);
    if (res == 0) {
      soc->connection = connected;
    } else {
      soc->connection = not_connected;
    }
  }

  /* Set out parameter */
  *p_connected = (soc->connection == connected);

  UNLOCK;
  return (SOC_OK);
}

