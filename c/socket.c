#include <sys/time.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <errno.h>

#include "socket_prv.h"

#ifndef SOCKET_NO_EVT
#include "wait_evt.h"
#else
static int evt_fd_set (int fd, boolean read) {
  return (FALSE);
}
#endif

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

/**********************************************/
/*
static void h_perror (const char *msg) {
  fprintf (stderr, "%s: ", msg);
  switch (h_errno) {
    case HOST_NOT_FOUND :
      fprintf (stderr, "Host not found");
    break;
    case TRY_AGAIN :
      fprintf (stderr, "Try again");
    break;
    case NO_RECOVERY :
      fprintf (stderr, "Nonrecoverable error");
    break;
    case NO_ADDRESS :
      fprintf (stderr, "No address");
    break;
    default :
      fprintf (stderr, "Unknown error");
    break;
  }
  fprintf (stderr, "\n");
}
*/
/**********************************************/

/* Init a socket (for open and accept) */
static int soc_init (soc_ptr *p_soc,
                     int fd,
                     socket_protocol protocol) {
  int allow_sockopt;
  int result;

  /* Check that socket is not already open */
  if (*p_soc != NULL) return (SOC_USE_ERR);

  /* Create structure */
  *p_soc = (soc_ptr) malloc (sizeof (soc_struct));
  if (*p_soc==NULL) {
    perror ("malloc1");
    return (SOC_SYS_ERR);
  }

  /* Save protocol and id */
  (*p_soc)->proto = protocol;
  (*p_soc)->socket_id = fd;

  /* Blocking operations as default */
  (*p_soc)->blocking = FALSE;
  result = soc_set_blocking ((soc_token)*p_soc, TRUE);
  if (result != SOC_OK) {
    return (result);
  }

  /* Init structures */
  (*p_soc)->send_struct.sin_family = AF_INET;
  (*p_soc)->rece_struct.sin_addr.s_addr = htonl(INADDR_ANY);
  (*p_soc)->rece_struct.sin_family = AF_INET;
  (*p_soc)->send_tail = NULL;
  (*p_soc)->send_len = 0;
  (*p_soc)->rece_head = NULL;
  (*p_soc)->rece_len = 0;
  (*p_soc)->expect_len = 0;

  /* Allow UDP broadcast or TCP ReuseAddr */
  allow_sockopt = 1;
  if (protocol == udp_socket) {
    result = setsockopt((*p_soc)->socket_id, SOL_SOCKET, SO_BROADCAST,
                   &allow_sockopt, sizeof (allow_sockopt));
    if (result == -1) {
      perror ("setsockopt1");
      close ((*p_soc)->socket_id);
      free (*p_soc);
      *p_soc = NULL;
      return (SOC_SYS_ERR);
    }
  }

  result = setsockopt((*p_soc)->socket_id, SOL_SOCKET, SO_REUSEADDR,
                 &allow_sockopt, sizeof (allow_sockopt));
  if (result == -1) {
    perror ("setsockopt2");
    close ((*p_soc)->socket_id);
    free (*p_soc);
    *p_soc = NULL;
    return (SOC_SYS_ERR);
  }

  /* Close on exec */
  if (fcntl((*p_soc)->socket_id, F_SETFD, FD_CLOEXEC) < 0) {
    perror ("fcntl_cloexec_open");
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
  } else {
    fd = socket(AF_INET, SOCK_STREAM, 0);
  }
  if ( fd == -1) {
    perror ("socket");
    free (*p_soc);
    *p_soc = NULL;
    return (SOC_SYS_ERR);
  }

  /* Init socket */
  return (soc_init(p_soc, fd, protocol));
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
 
  close ((*p_soc)->socket_id);
  free (*p_soc);
  *p_soc = NULL;
  return (SOC_OK);

}

/* Gets the id of a socket (for select, ioctl ... ) */
extern int soc_get_id (soc_token token, int *p_id) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Ok */
  *p_id = soc->socket_id;
  return (SOC_OK);
}

/* Set the socket blocking or non blocking */
/*  (for sendind and receiving) */ 
extern int soc_set_blocking (soc_token token, boolean blocking) {
  soc_ptr soc = (soc_ptr) token;
  int status;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Nothing to do */
  if (soc->blocking == blocking) {
    return (SOC_OK);
  }

  /* Store state */
  soc->blocking = blocking; 

  /* Get status */
  status = fcntl (soc->socket_id, F_GETFL, 0);
  if (status < 0) {
    perror ("fcntl_get_block");
    return (SOC_SYS_ERR);
  }

  /* Blocking receiving or not */
  if (blocking) {
    status &= ~O_NONBLOCK;
  } else {
    status |= O_NONBLOCK;
  }

  /* Fcntl for having blocking ios */
  if (fcntl (soc->socket_id, F_SETFL, status)  == -1) {
    perror ("fcntl_set_block");
    return (SOC_SYS_ERR);
  }

  /* Ok */
  return (SOC_OK);
}

/* Do the connection */
static int soc_connect (soc_ptr soc) {

  int result;

  /* Check that socket is open and dest set */
  if (soc == NULL) return (SOC_USE_ERR);
  if (! soc->dest_set) return (SOC_DEST_ERR);

  /* Connect */
  do {
    result = connect (soc->socket_id,
                      (struct sockaddr*)&(soc->send_struct),
                      socklen);
  } while ( (result < 0) && (errno == EINTR) );

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
      perror ("connect");
      return (SOC_SYS_ERR);
    }
  }

  /* Ok */
  soc->connection = connected;
  return (SOC_OK);

}


/* Set the destination host/lan name and port - specify service */
/* Broadcast if lan */
extern int soc_set_dest_name_service (soc_token token, const char *host_lan,
                                      boolean lan, const char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct hostent *host_name;
  struct netent  *lan_name;
  struct servent *serv_name;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Not linked nor already connected if tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    if (soc->linked) {
      return (SOC_LINK_ERR);
    } else if (soc->connection != not_connected) {
      return (SOC_CONN_ERR);
    }
  }

  /* Read port num */
  if ((serv_name = getservbyname(service, ns_proto[soc->proto]))== NULL) {
    return (SOC_NAME_NOT_FOUND);
  }

  if (! lan) {
    /* Read  IP adress of host */
    if ((host_name = gethostbyname(host_lan)) == NULL) {
      return (SOC_NAME_NOT_FOUND);
    }
    memcpy((void *) &(soc->send_struct.sin_addr.s_addr),
     (void *) host_name->h_addr, (size_t) host_name->h_length);
  } else {
    /* No tcp bcast */
    if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
      return (SOC_BCAST_ERR);
    }

    /* Read IP prefix of LAN */
    if ((lan_name = getnetbyname(host_lan)) == NULL) {
      return (SOC_NAME_NOT_FOUND);
    }
    soc->send_struct.sin_addr = inet_makeaddr(lan_name->n_net, 0);
  }

  soc->send_struct.sin_port = serv_name->s_port;

  /* Connect tcp */
  soc->dest_set = TRUE;
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    return soc_connect (soc);
  }

  /* Ok */
  return (SOC_OK);
}

/* Set the destination host name and port - specify port */
/* Broadcast if lan */
extern int soc_set_dest_name_port (soc_token token, const char *host_lan,
                                   boolean lan, soc_port port) {

  soc_ptr soc = (soc_ptr) token;
  struct hostent *host_name;
  struct netent  *lan_name;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Not linked nor already connected if tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    if (soc->linked) {
      return (SOC_LINK_ERR);
    } else if (soc->connection != not_connected) {
      return (SOC_CONN_ERR);
    }
  }

  if (! lan) {
    /* Read  IP adress of host */
    if ((host_name = gethostbyname(host_lan)) == NULL) {
      return (SOC_NAME_NOT_FOUND);
    }
    memcpy((void *) &(soc->send_struct.sin_addr.s_addr),
     (void *) host_name->h_addr, (size_t) host_name->h_length);
  } else {
    /* No tcp bcast */
    if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
      return (SOC_BCAST_ERR);
    }

    /* Read IP prefix of LAN */
    if ((lan_name = getnetbyname(host_lan)) == NULL) {
      return (SOC_NAME_NOT_FOUND);
    }
    soc->send_struct.sin_addr = inet_makeaddr(lan_name->n_net, 0);
  }

  soc->send_struct.sin_port = htons (port);

  /* Connect tcp */
  soc->dest_set = TRUE;
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    return soc_connect (soc);
  }

  /* Ok */
  return (SOC_OK);
}

extern int soc_set_dest_host_service (soc_token token, const soc_host *host,
                                      const char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct servent *serv_name;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Not linked nor already connected if tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    if (soc->linked) {
      return (SOC_LINK_ERR);
    } else if (soc->connection != not_connected) {
      return (SOC_CONN_ERR);
    }
  }

  /* Read port num */
  if ((serv_name = getservbyname(service, ns_proto[soc->proto]))== NULL) {
    return (SOC_NAME_NOT_FOUND);
  }

  soc->send_struct.sin_addr.s_addr = host->integer;
  soc->send_struct.sin_port = serv_name->s_port;

  /* Connect tcp */
  soc->dest_set = TRUE;
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    return soc_connect (soc);
  }

  /* Ok */
  return (SOC_OK);
}

extern int soc_set_dest_host_port (soc_token token, const soc_host *host,
                                   soc_port port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Not linked nor already connected if tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    if (soc->linked) {
      return (SOC_LINK_ERR);
    } else if (soc->connection != not_connected) {
      return (SOC_CONN_ERR);
    }
  }

  soc->send_struct.sin_addr.s_addr = host->integer;
  soc->send_struct.sin_port = htons (port);

  /* Connect tcp */
  soc->dest_set = TRUE;
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    return soc_connect (soc);
  }

  /* Ok */
  return (SOC_OK);
}

/* Change destination host_lan name (same port) */
/* Destination must have been previously set (by a set or a rece) */
/* Broadcast if lan */
extern int soc_change_dest_name (soc_token token, const char *host_lan, boolean lan) {
  soc_ptr soc = (soc_ptr) token;
  struct hostent *host_name;
  struct netent  *lan_name;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check not tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    return (SOC_PROTO_ERR);
  }

  /* Check that a destination is already set */
  if (!soc->dest_set) return (SOC_DEST_ERR);

  if (! lan) {
    /* Read  IP adress of host */
    if ((host_name = gethostbyname(host_lan)) == NULL) {
      return (SOC_NAME_NOT_FOUND);
    }
    memcpy((void *) &(soc->send_struct.sin_addr.s_addr),
     (void *) host_name->h_addr, (size_t) host_name->h_length);
  } else {
    /* Read IP prefix of LAN */
    if ((lan_name = getnetbyname(host_lan)) == NULL) {
      return (SOC_NAME_NOT_FOUND);
    }
    soc->send_struct.sin_addr = inet_makeaddr(lan_name->n_net, 0);
  }

  /* Ok */
  return (SOC_OK);
}

/* Change destination host (same port) */
extern int soc_change_dest_host (soc_token token, const soc_host *host) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check not tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    return (SOC_PROTO_ERR);
  }

  /* Check that a destination is already set */
  if (!soc->dest_set) return (SOC_DEST_ERR);

  soc->send_struct.sin_addr.s_addr = host->integer;

  /* Ok */
  return (SOC_OK);
}

/* Change destination port (same host) - specify service */
extern int soc_change_dest_service (soc_token token, const char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct servent *serv_name;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check not tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    return (SOC_PROTO_ERR);
  }

  /* Check that a destination is already set */
  if (!soc->dest_set) return (SOC_DEST_ERR);

  /* Read IP adress of port */
  if ((serv_name = getservbyname(service, ns_proto[soc->proto]))== NULL) {
    return (SOC_NAME_NOT_FOUND);
  }

  /* Init structure */
  soc->send_struct.sin_port = serv_name->s_port;

  /* Ok */
  return (SOC_OK);
}

/* Change destination port (same host) - specify port */
extern int soc_change_dest_port (soc_token token, soc_port port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check not tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    return (SOC_PROTO_ERR);
  }

  /* Check that a destination is already set */
  if (!soc->dest_set) return (SOC_DEST_ERR);

  /* Init structure */
  soc->send_struct.sin_port = htons (port);

  /* Ok */
  return (SOC_OK);
}

/* Get current lan name (computed from local host name) */
/* lan_name must be big enough */
extern int soc_get_lan_name (char *lan_name, unsigned int lan_name_len) {

  char host_name[MAXHOSTNAMELEN];
  struct hostent *hostentp;
  struct in_addr host_address;
  unsigned int net_mask;
  struct netent  *netentp;

  if (gethostname(host_name, sizeof(host_name)) == -1) {
    return (SOC_NAME_NOT_FOUND);
  }

  hostentp = gethostbyname (host_name);
  if (hostentp ==  (struct hostent *)NULL) {
    return (SOC_NAME_NOT_FOUND);
  }

  /* First of aliases */
  host_address.s_addr = * (unsigned int*) hostentp->h_addr_list[0];
  net_mask = inet_netof(host_address);
  if (net_mask == -1) {
    perror ("inet_netof");
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

/* Get the destination port */
extern int soc_get_dest_port (soc_token token, soc_port *p_port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check that a destination is already set */
  if (!soc->dest_set) return (SOC_DEST_ERR);

  /* Ok */
  *p_port = ntohs(soc->send_struct.sin_port);
  return (SOC_OK);
}

extern int soc_get_dest_host (soc_token token, soc_host *p_host) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check that a destination is already set */
  if (!soc->dest_set) return (SOC_DEST_ERR);

  /* Ok */
  p_host->integer = soc->send_struct.sin_addr.s_addr;
  return (SOC_OK);
}

/* Find name of soc_host and vice versa */
extern int soc_host_name_of (const soc_host *p_host, char *host_name,
                             unsigned int host_name_len) {
  struct hostent *host_struct;

  /* Read name of host */
  host_struct = gethostbyaddr((void*)p_host, sizeof(*p_host), AF_INET);
  if (host_struct == (struct hostent *)NULL) {
    return (SOC_NAME_NOT_FOUND);
  }

  if (strlen(host_struct->h_name) >= host_name_len) {
    return (SOC_LEN_ERR);
  }
  strcpy (host_name, host_struct->h_name);

  /* Ok */
  return (SOC_OK);
}

extern int soc_host_of (const char *host_name, soc_host *p_host) {
  struct hostent *host_struct;

  /* Read  IP adress of host */
  host_struct = gethostbyname(host_name);
  if (host_struct == (struct hostent *)NULL) {
    return (SOC_NAME_NOT_FOUND);
  }
  memcpy((void *) &(p_host->integer),
     (void *) host_struct->h_addr, sizeof(p_host->integer));

  /* Ok */
  return (SOC_OK);
}

static const char *proto_name[3] = {"udp", "tcp", "tcp"};

/* Find name of soc_port and vice versa */
extern int soc_port_name_of (const soc_port port,
                             const socket_protocol proto,
                             char *port_name,
                             unsigned int port_name_len) {

  struct servent *port_struct;

  /* Read name of port */
  port_struct = getservbyport((int)port, proto_name[proto]);
  if (port_struct == (struct servent *)NULL) {
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
  port_struct = getservbyname(port_name, proto_name[proto]);
  if (port_struct == (struct servent *)NULL) {
    return (SOC_NAME_NOT_FOUND);
  }
  *p_port = (soc_port) port_struct->s_port;

  /* Ok */
  return (SOC_OK);
}
/* Gets local host */
extern int soc_get_local_host_name (char *host_name,
                                    unsigned int host_name_len) {
  /* Get current host name */
  if (gethostname(host_name, host_name_len) != 0) {
    perror("gethostname");
    return (SOC_NAME_NOT_FOUND);
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
    return (res);
  }
  /* Get its addr */
  return soc_host_of(hostname, p_host);
}

/* Send to a socket, the destination of which must set */
/* May return SOC_WOULD_BLOCK, then next sends have to be made */
/*  with length=0, util soc_send returns ok */
extern int soc_send (soc_token token, soc_message message, soc_length length) {
  soc_ptr soc = (soc_ptr) token;
  boolean cr;
  soc_header header;
  struct iovec vector[VECTOR_LEN];
  soc_length len2send;
  char* msg2send;
  int vector_len;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check that desination is set */
  if (!soc->dest_set) return (SOC_DEST_ERR); 

  /* Connected if tcp */
  if ( ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) )
    && (soc->connection != connected) ) {
     return (SOC_CONN_ERR);
  }

  /* Tail / length consistency */
  if ( (soc->send_tail != NULL) && (length != 0) ) {
    return (SOC_TAIL_ERR);
  }

  if (soc->send_tail == NULL) {
    /* No previous overflow */
    if (soc->proto == tcp_header_socket) {
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
    if (soc->proto == tcp_header_socket) {
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
    if (soc->proto == tcp_header_socket) {
      cr = writev(soc->socket_id, vector, vector_len);
    } else if (soc->proto == tcp_socket) {
      cr = send(soc->socket_id, msg2send, len2send, 0);
    } else {
      cr = sendto(soc->socket_id, msg2send, len2send, 0,
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
      perror ("send");
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
  } else if (soc->blocking) {
    /* Not blocked despite blocking */
    return (SOC_CONN_LOST);
  } else if ((cr == 0) && (soc->proto == udp_socket) ) {
    /* Udp */
    return (SOC_WOULD_BLOCK);
  } else {
    /* Tcp Overflow: save tail */
    len2send  -= cr;
    msg2send = malloc (len2send);
    if (msg2send == NULL) {
      perror ("malloc2");
      return (SOC_SYS_ERR);
    }
    if ( (soc->proto == tcp_header_socket)
      && (soc->send_tail == NULL)
      && (cr < sizeof(header)) ) {

      /* First send of vector and header not completly sent */
      /* Save rest of header and all message */
      memcpy (msg2send, ((char*)&header) + cr, sizeof(header) - cr);
      memcpy (msg2send + sizeof(header) - cr, (char*)message, length);

    } else if (soc->send_tail == NULL) {
      /* First send of vector but either header sent or no header */
      /* Save rest of message */
      if (soc->proto == tcp_header_socket) {
        /* Cr = header + nbmessage. Start at cr - header */
        cr -= sizeof(header);
      }
      memcpy (msg2send , (char*)message + cr, len2send);
    } else {
      /* We were in overflow: save rest of tail */
      memcpy (msg2send, soc->send_tail + cr, len2send);
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

/* Resend tail of previous message */
extern int soc_resend (soc_token token) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check socket is in send overflow */
  if (soc->send_tail == NULL) return (SOC_TAIL_ERR);

  /* Send */
  return (soc_send (token, NULL, 0));

}

/*******************************************************************/
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


static int bind_and_co (soc_token token, boolean dynamic) {

  soc_ptr soc = (soc_ptr) token;
  int res;
  soc_port linked_port;
  boolean do_ipm;

  /* Bind */
  if (bind (soc->socket_id, (struct sockaddr*) &(soc->rece_struct),
            socklen ) < 0) {
    if (errno == EADDRINUSE) {
      return (SOC_ADDR_IN_USE);
    } else {
      perror ("bind");
      return (SOC_SYS_ERR);
    }
  }

  /* Set linked port */
  soc->linked = TRUE;
  res = soc_get_linked_port (token, &linked_port);
  soc->linked = FALSE;
  if (res != SOC_OK) {
     return res;
  }

  /* Listen for tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    if (listen (soc->socket_id, SOMAXCONN) < 0) {
      perror ("listen");
      return (SOC_SYS_ERR);
    }
  }


  /* Ipm if udp, dest_set with an ipm adress */
  /*  and same port (if not dynamic) */
  do_ipm =  ( (soc->proto == udp_socket)
           && (soc->dest_set) 
           && (is_ipm(& soc->send_struct) )
           && ( dynamic || (soc->send_struct.sin_port
                         == soc->rece_struct.sin_port) ) );

  if (do_ipm) {
    struct ip_mreq ipm_addr;
    ipm_addr.imr_multiaddr.s_addr = soc->send_struct.sin_addr.s_addr;
    ipm_addr.imr_interface.s_addr = INADDR_ANY;
    if (setsockopt (soc->socket_id, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                    (char*) &ipm_addr, sizeof(ipm_addr)) != 0) {
      perror ("setsockopt3");
      return (SOC_SYS_ERR);
    }
  }

  /* Ok */
  soc->linked = TRUE;
  return (SOC_OK);
}


/* Links the socket to a port specified by the service */
/* The socket must be open and not already linked */
extern int soc_link_service (soc_token token, const char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct servent *serv_name;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Not linked nor already connected if tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    if (soc->linked) {
      return (SOC_LINK_ERR);
    } else if (soc->connection != not_connected) {
      return (SOC_CONN_ERR);
    }
  }

  /* Read IP adress of port */
  if ((serv_name = getservbyname(service, ns_proto[soc->proto]))== NULL) {
    return (SOC_NAME_NOT_FOUND);
  }

  /* Init structure */
  soc->rece_struct.sin_port = serv_name->s_port;

  /* Bind */
  return bind_and_co (token, FALSE);
}


/* Links the socket to a port specified by it's value */
/* The socket must be open and not already linked */
extern int soc_link_port  (soc_token token, soc_port port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Not linked nor already connected if tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    if (soc->linked) {
      return (SOC_LINK_ERR);
    } else if (soc->connection != not_connected) {
      return (SOC_CONN_ERR);
    }
  }

  /* Init structure */
  soc->rece_struct.sin_port = htons((u_short) port);

  /* Bind */
  return bind_and_co (token, FALSE);
}

/* Links the socket to a port dynamically choosen by the system */
/* The socket must be open and not already linked */
extern int soc_link_dynamic  (soc_token token) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Not linked nor already connected if tcp */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    if (soc->linked) {
      return (SOC_LINK_ERR);
    } else if (soc->connection != not_connected) {
      return (SOC_CONN_ERR);
    }
  }

  /* Init structure Port is not set */
  soc->rece_struct.sin_port = htons(0);

  /* Bind */
  return bind_and_co (token, TRUE);
}

/* Gets the port to which is linked a socket */
extern int soc_get_linked_port  (soc_token token, soc_port *p_port) {
  soc_ptr soc = (soc_ptr) token;
  int len = socklen;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check that a port is already set */
  if (!soc->linked) return (SOC_LINK_ERR);

  /* Get the structure */
  if (getsockname (soc->socket_id,
   (struct sockaddr*) (&soc->rece_struct), &len) < 0) {
    perror ("getsockname");
    return (SOC_SYS_ERR);
  }

  /* Ok */
  *p_port = ntohs(soc->rece_struct.sin_port);
  return (SOC_OK);
}
 
/* To test if there is a message to receive and receive it */
/* Length must be initialized with the size of the buffer */
/* The socket must be open, linked in udp and not linked in tcp */
/*  After success, the socket may be ready for a send to reply */
/* No set_for_reply if tcp */
/* Returned values: */
/*  In tcp no header: */
/*   - the length of bytes read, which the length of the message sent */
/*     except in tcp (no header) where the length read my me anything */
/*     from 0 to length */
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
    res = recv(soc->socket_id, addr2read, len2read, 0);
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
      perror ("recv");
      return (SOC_SYS_ERR);
    }
  } else if (res == 0) {
    return (SOC_READ_0);
  }

  if (res == len2read) {
    /* Done */
    if (soc->rece_head != NULL) {
      /* Return full head and clear it */
      memcpy (buffer, soc->rece_head, total_len);
      free (soc->rece_head);
      soc->rece_head = NULL;
      soc->rece_len = 0;
    }
    return (SOC_OK);
  } else {
    /* Underflow */
    if (soc->proto == tcp_socket) {
      /* Not managed in tcp no header */
      return (res);
    }
    if (soc->rece_head == NULL) {
      soc->rece_len = res;
      soc->rece_head = malloc (total_len);
      if (soc->rece_head == NULL) {
        perror ("malloc3");
        return (SOC_SYS_ERR);
      }
      memcpy (soc->rece_head, buffer, res);
    } else {
      soc->rece_len += res;
    }
    return (SOC_WOULD_BLOCK);
  }

}

/* If socket is non blocking, call rec1 directly, */
/* Else (blocking socket) call rec1 until it does not */
/* return SOC_WOULD_BLOCK */
static int rec2 (soc_ptr soc, char *buffer, int total_len) {
  int res;

  for (;;) {
    res = rec1(soc, buffer, total_len);
    if ( (! soc->blocking) || (res != SOC_WOULD_BLOCK) ) {
      return (res);
    }
  }
}


extern int soc_receive (soc_token token,
                        soc_message message, soc_length length,
                        boolean set_for_reply) {
  soc_ptr soc = (soc_ptr) token;
  int result;
  int addr_len;
  struct sockaddr_in *from_addr;
  soc_header header;


  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check set_for_reply and linked vs proto */
  if ( (soc->proto == tcp_socket) || (soc->proto == tcp_header_socket) ) {
    if (set_for_reply) return (SOC_REPLY_ERR);
    if (soc->linked) return (SOC_LINK_ERR);
    if (soc->connection != connected) return (SOC_CONN_ERR);
  } else {
    if (!soc->linked) return (SOC_LINK_ERR);
  }

  /* Prepare for reply or not */
  if (set_for_reply) {
    from_addr = &(soc->send_struct);
    addr_len = socklen;
    /* In case of error */
    soc->dest_set = FALSE;
  } else {
    from_addr = NULL;
    addr_len = 0;
  }

  /* Receive */
  if (soc->proto == tcp_header_socket) {
    /* Read header or data */
    if (soc->expect_len == 0) {
      /* Header not fully received yet */
      result = rec2(soc, (char *)&header, sizeof(header));
      if (result == SOC_OK) {
        /* Disconnect if invalid magic num */
        if (ntohl(header.magic_number) != MAGIC_NUMBER) {
          fprintf (stderr, "Bad magic number\n");
          return (SOC_CONN_LOST);
        }
        /* Header is read and correct. Save expected length */
        soc->expect_len = ntohl(header.size);
      } else {
        return (result);
      }
    }

    /* Got a header now? */
    if (soc->expect_len == 0) {
      return (SOC_WOULD_BLOCK);
    }

    /* Check length vs size */
    if (length < soc->expect_len) {
      return (SOC_LEN_ERR);
    }
    result = rec2(soc, (char *)message, soc->expect_len);
    if (result == SOC_OK) {
      result = soc->expect_len;
      /* Expect header next */
      soc->expect_len = 0;
    }
    return (result);
  }

  if (soc->proto == tcp_socket) {
    return (rec1(soc, (char *)message, length));
  }
  

  do {
    result = recvfrom(soc->socket_id, (char *)message, 
       length, 0, (struct sockaddr*) from_addr, &addr_len);
  } while ( (result == -1) && (errno == EINTR) );

  if (result < 0) {
    if (set_for_reply) {
      soc->dest_set = FALSE;
    }
    if (errno == EWOULDBLOCK) {
      return (SOC_WOULD_BLOCK);
    } else if ( (errno == EPIPE) 
             || (errno == ECONNRESET) 
             || (errno == ECONNREFUSED) ) {
      return (SOC_CONN_LOST);
    } else {
      /* Reception error */
      perror ("recv");
      return (SOC_SYS_ERR);
    }
  } else if (result == 0) {
    return (SOC_READ_0);
  } else {
    /* A message read */
    if (set_for_reply) {
      soc->dest_set = TRUE;
    }
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
  int len = socklen;

  /* Check that socket is open */
  if (soc == NULL) return (SOC_USE_ERR);

  /* Check that new socket is not already open */
  if (*p_soc != NULL) return (SOC_USE_ERR);

  /* Check tcp and linked */
  if ( (soc->proto != tcp_socket) && (soc->proto != tcp_header_socket) ) {
    return (SOC_PROTO_ERR);
  }
  if (! soc->linked) return (SOC_LINK_ERR);

  /* Accept */
  do {
    fd = accept (soc->socket_id, &from_addr, &len);
  } while ( (fd == -1) && (errno == EINTR) );

  /* Check result */
  if (fd < 0) {
    perror ("accept");
    return (SOC_SYS_ERR);
  }

  /* Init socket */
  result = soc_init(p_soc, fd, soc->proto);
  if (result != SOC_OK) {
    return (result);
  }

  /* Set dest and connected */
  memcpy (&(*p_soc)->send_struct , &from_addr, len);
  (*p_soc)->dest_set = TRUE;
  (*p_soc)->connection = connected;

  /* Ok */
  return (SOC_OK);

}

extern int soc_is_connected (soc_token token, boolean *p_connected) {
  soc_ptr soc = (soc_ptr) token;
  int res;
  int status;
  int len;

  /* Check that socket is open and tcp */
  if (soc == NULL) return (SOC_USE_ERR);
  if ( (soc->proto != tcp_socket) && (soc->proto != tcp_header_socket) ) {
    return (SOC_PROTO_ERR);
  }

  if (soc->connection == connected) {
    *p_connected = TRUE;
    return (SOC_OK);
  } else if (soc->connection == not_connected) {
    *p_connected = FALSE;
    return (SOC_OK);
  }

  /* Get pending connect result */
  len = sizeof (status);
  res = getsockopt(soc->socket_id, SOL_SOCKET, SO_ERROR, &status, &len);
  if (res == -1) {
    perror ("getsockopt");
    soc->connection = not_connected;
    return (SOC_SYS_ERR);
  }

  if (status == 0) {
    soc->connection = connected;
    *p_connected = TRUE;
  } else {
    soc->connection = not_connected;
    *p_connected = FALSE;
  }

  return (SOC_OK);

}

