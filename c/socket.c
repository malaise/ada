
#include <unistd.h>
#include <fcntl.h>

#include <stdio.h>
#include <malloc.h>
#include <sys/time.h>
#include <errno.h>

#include "socket_prv.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

static void h_perror (const char *msg) {
  printf ("%s: ", msg);
  switch (h_errno) {
    case HOST_NOT_FOUND :
      printf ("Host not found");
    break;
    case TRY_AGAIN :
      printf ("Try again");
    break;
    case NO_RECOVERY :
      printf ("Nonrecoverable error");
    break;
    case NO_ADDRESS :
      printf ("No address");
    break;
    default :
      printf ("Unknown error");
    break;
  }
  printf ("\n");
}

/* Open a socket */
extern int soc_open (soc_token *p_token,
                     socket_protocol protocol) {
  soc_ptr *p_soc = (soc_ptr*) p_token;
  int allow_sockopt;

  /* Check that socket is not already open */
  if (*p_soc != NULL) return (BS_ERROR);

  /* Create structure */
  *p_soc = (soc_ptr) malloc (sizeof (soc_struct));
  if (*p_soc==NULL) return (BS_ERROR);

  /* Save protocol */
  (*p_soc)->proto = protocol;

  /* Call to socket */
  if (protocol == udp_socket) {
    (*p_soc)->socket_id = socket(AF_INET, SOCK_DGRAM, 0);
  } else {
    (*p_soc)->socket_id = socket(AF_INET, SOCK_STREAM, 0);
  }
  if ( (*p_soc)->socket_id == -1) {
    perror ("socket");
    free (*p_soc);
    *p_soc = NULL;
    return (BS_ERROR);
  }

  /* Blocking operations as default */
  /* Ioctl for having blocking receive */
  if (ioctl ((*p_soc)->socket_id, FIONBIO, (char *) &BLOCKINGIO) < 0) {
    perror ("ioctl");
    close ((*p_soc)->socket_id);
    free (*p_soc);
    *p_soc = NULL;
    return (BS_ERROR);
  }

  /* Init structures */
  (*p_soc)->send_struct.sin_family = AF_INET;
  (*p_soc)->rece_struct.sin_addr.s_addr = htonl(INADDR_ANY);
  (*p_soc)->rece_struct.sin_family = AF_INET;

  /* Allow UDP broadcast */
  if (protocol == udp_socket) {
    allow_sockopt = 1;
    if (setsockopt((*p_soc)->socket_id, SOL_SOCKET, SO_BROADCAST,
                   &allow_sockopt, sizeof (allow_sockopt)) < 0) {
      perror ("setsockopt1");
      close ((*p_soc)->socket_id);
      free (*p_soc);
      *p_soc = NULL;
      return (BS_ERROR);
    }
  }

  /* Close on exec */
  if (fcntl((*p_soc)->socket_id, F_SETFD, FD_CLOEXEC) < 0) {
    perror ("fcntl");
    close ((*p_soc)->socket_id);
    free (*p_soc);
    *p_soc = NULL;
    return (BS_ERROR);
  }

  /* Ok */
  (*p_soc)->dest_set = FALSE;
  (*p_soc)->linked = FALSE;
  (*p_soc)->blocking = TRUE;
  (*p_soc)->connected = FALSE;
  return (BS_OK);

}

/* Close a socket */
extern int soc_close (soc_token *p_token) {
  soc_ptr *p_soc = (soc_ptr*) p_token;

  /* Check that socket is open */
  if (*p_soc == NULL) return (BS_ERROR);
 
  close ((*p_soc)->socket_id);
  free (*p_soc);
  *p_soc = NULL;
  return (BS_OK);

}

/* Gets the id of a socket (for select, ioctl ... ) */
extern int soc_get_id (soc_token token, int *p_id) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Ok */
  *p_id = soc->socket_id;
  return (BS_OK);
}

/* Set the socket blocking or non blocking */
/*  (for sendind and receiving) */ 
extern int soc_set_blocking (soc_token token, boolean blocking) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Nothing to do */
  if (soc->blocking == blocking) {
    return (BS_OK);
  }

  /* Store state */
  soc->blocking = blocking; 

  /* Blocking receiving or not */
  if (blocking) {
    /* Ioctl for having blocking receive */
    if (ioctl (soc->socket_id, FIONBIO,
     (char *) &BLOCKINGIO) < 0) {
      perror ("ioctl");
      return (BS_ERROR);
    }
  } else {
    /* Ioctl for having non blocking receive */
    if (ioctl (soc->socket_id, FIONBIO,
     (char *) &NONBLOCKINGIO) < 0) {
      perror ("ioctl");
      return (BS_ERROR);
    }
  }

  /* Ok */
  return (BS_OK);
}

/* Do the connection */
static int soc_connect (soc_ptr soc) {

  int result;

  /* Check that socket is open and dest set */
  if (soc == NULL) return (BS_ERROR);
  if (! soc->dest_set) return (BS_ERROR);

  /* Connect */
  do {
    result = connect (soc->socket_id,
                      (struct sockaddr*)&(soc->send_struct),
                      socklen);
  } while ( (result < 0) && (errno == EINTR) );

  /* Check result */
  if (result < 0) {
    if (errno == ECONNREFUSED) {
      /* Not connected */
      return (BS_OK);
    } else {
      perror ("connect");
      return (BS_ERROR);
    }
  }

  /* Ok */
  soc->connected = TRUE;
  return BS_OK;

}


/* Set the destination host/lan name and port - specify service */
/* Broadcast if lan */
extern int soc_set_dest_service (soc_token token, char *host_lan, boolean lan,
                                 const char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct hostent *host_name;
  struct netent  *lan_name;
  struct servent *serv_name;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Read IP adress of port */
  if ((serv_name = getservbyname(service, ns_proto[soc->proto]))== NULL) {
    perror ("getservbyname");
    return (BS_ERROR);
  }

  /* Not linked nor already connected if tcp */
  if ( (soc->proto == tcp_socket) &&
       (soc->linked || soc->connected) ) {
     return (BS_ERROR);
  }

  if (! lan) {
    /* Read  IP adress of host */
    if ((host_name = gethostbyname(host_lan)) == NULL) {
      h_perror ("gethostbyname");
      return (BS_ERROR);
    }
    memcpy((void *) &(soc->send_struct.sin_addr.s_addr),
     (void *) host_name->h_addr, (size_t) host_name->h_length);
  } else {
    /* No tcp bcast */
    if (soc->proto == tcp_socket) return (BS_ERROR);

    /* Read IP prefix of LAN */
    if ((lan_name = getnetbyname(host_lan)) == NULL) {
      perror ("getnetbyname");
      return (BS_ERROR);
    }
    soc->send_struct.sin_addr = inet_makeaddr(lan_name->n_net, 0);
  }

  soc->send_struct.sin_port = serv_name->s_port;

  /* Connect tcp */
  soc->dest_set = TRUE;
  if (soc->proto == tcp_socket) {
    return soc_connect (soc);
  }

  /* Ok */
  return (BS_OK);
}

/* Set the destination host name and port - specify port */
/* Broadcast if lan */
extern int soc_set_dest_port (soc_token token, char *host_lan, boolean lan,
                              soc_port port) {

  soc_ptr soc = (soc_ptr) token;
  struct hostent *host_name;
  struct netent  *lan_name;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Not linked nor already connected if tcp */
  if ( (soc->proto == tcp_socket) &&
       (soc->linked || soc->connected) ) {
     return (BS_ERROR);
  }

  if (! lan) {
    /* Read  IP adress of host */
    if ((host_name = gethostbyname(host_lan)) == NULL) {
      h_perror ("gethostbyname");
      return (BS_ERROR);
    }
    memcpy((void *) &(soc->send_struct.sin_addr.s_addr),
     (void *) host_name->h_addr, (size_t) host_name->h_length);
  } else {
    /* No tcp bcast */
    if (soc->proto == tcp_socket) return (BS_ERROR);

    /* Read IP prefix of LAN */
    if ((lan_name = getnetbyname(host_lan)) == NULL) {
      perror ("getnetbyname");
      return (BS_ERROR);
    }
    soc->send_struct.sin_addr = inet_makeaddr(lan_name->n_net, 0);
  }

  soc->send_struct.sin_port = htons (port);

  /* Connect tcp */
  soc->dest_set = TRUE;
  if (soc->proto == tcp_socket) {
    return soc_connect (soc);
  }

  /* Ok */
  return (BS_OK);
}

extern int soc_set_dest (soc_token token, soc_host host, soc_port port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Not linked nor already connected if tcp */
  if ( (soc->proto == tcp_socket) &&
       (soc->linked || soc->connected) ) {
     return (BS_ERROR);
  }

  soc->send_struct.sin_addr.s_addr = host.integer;
  soc->send_struct.sin_port = htons (port);

  /* Connect tcp */
  soc->dest_set = TRUE;
  if (soc->proto == tcp_socket) {
    return soc_connect (soc);
  }

  /* Ok */
  return (BS_OK);
}

/* Change destination host_lan name (same port) */
/* Destination must have been previously set (by a set or a rece) */
/* Broadcast if lan */
extern int soc_change_dest_host (soc_token token, char *host_lan, boolean lan) {
  soc_ptr soc = (soc_ptr) token;
  struct hostent *host_name;
  struct netent  *lan_name;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Check udp */
  if (soc->proto == tcp_socket) return (BS_ERROR);

  /* Check that a destination is already set */
  if (!soc->dest_set) return (BS_ERROR);

  if (! lan) {
    /* Read  IP adress of host */
    if ((host_name = gethostbyname(host_lan)) == NULL) {
      h_perror ("gethostbyname");
      return (BS_ERROR);
    }
    memcpy((void *) &(soc->send_struct.sin_addr.s_addr),
     (void *) host_name->h_addr, (size_t) host_name->h_length);
  } else {
    /* Read IP prefix of LAN */
    if ((lan_name = getnetbyname(host_lan)) == NULL) {
      perror ("getnetbyname");
      return (BS_ERROR);
    }
    soc->send_struct.sin_addr = inet_makeaddr(lan_name->n_net, 0);
  }

  /* Ok */
  return (BS_OK);
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
    perror ("gethostname");
    return (BS_ERROR);
  }

  hostentp = gethostbyname (host_name);
  if (hostentp ==  (struct hostent *)NULL) {
    h_perror ("gethostbyname");
    return (BS_ERROR);
  }

  host_address.s_addr = * (unsigned int*) hostentp->h_addr_list[0];
  net_mask = inet_netof(host_address);
  if (net_mask == -1) {
    return (BS_ERROR);
  }

  netentp = getnetbyaddr(net_mask, AF_INET);
  if (netentp == (struct netent *) NULL) {
    perror ("getnetbyaddr");
    return (BS_ERROR);
  }

  if (strlen(netentp->n_name) >= lan_name_len) {
    return (BS_ERROR);
  }

  strcpy (lan_name, netentp->n_name);
  /* Ok */
  return (BS_OK);

}

/* Change destination port (same host) - specify service */
extern int soc_change_dest_service (soc_token token, char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct servent *serv_name;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Check udp */
  if (soc->proto == tcp_socket) return (BS_ERROR);

  /* Check that a destination is already set */
  if (!soc->dest_set) return (BS_ERROR);

  /* Read IP adress of port */
  if ((serv_name = getservbyname(service, ns_proto[soc->proto]))== NULL) {
    perror ("getservbyname");
    return (BS_ERROR);
  }

  /* Init structure */
  soc->send_struct.sin_port = serv_name->s_port;

  /* Ok */
  return (BS_OK);
}

/* Change destination port (same host) - specify port */
extern int soc_change_dest_port (soc_token token, soc_port port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Check udp */
  if (soc->proto == tcp_socket) return (BS_ERROR);

  /* Check that a destination is already set */
  if (!soc->dest_set) return (BS_ERROR);

  /* Init structure */
  soc->send_struct.sin_port = htons (port);

  /* Ok */
  return (BS_OK);
}

/* Get the destination port */
extern int soc_get_dest_port (soc_token token, soc_port *p_port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Check that a destination is already set */
  if (!soc->dest_set) return (BS_ERROR);

  /* Ok */
  *p_port = ntohs(soc->send_struct.sin_port);
  return (BS_OK);
}

extern int soc_get_dest_host (soc_token token, soc_host *p_host) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Check that a destination is already set */
  if (!soc->dest_set) return (BS_ERROR);

  /* Ok */
  p_host->integer = soc->send_struct.sin_addr.s_addr;
  return (BS_OK);
}

/* Find name of soc_host and vice versa */
extern int soc_host_name_of (soc_host *p_host, char *host_name,
                             unsigned int host_name_len) {
  struct hostent *host_struct;

  /* Read name of host */
  host_struct = gethostbyaddr((void*)p_host, sizeof(*p_host), AF_INET);
  if (host_struct == (struct hostent *)NULL) {
    h_perror ("gethostbyaddr");
    return (BS_ERROR);
  }

  if (strlen(host_struct->h_name) >= host_name_len) {
    return (BS_ERROR);
  }
  strcpy (host_name, host_struct->h_name);

  /* Ok */
  return (BS_OK);
}

extern int soc_host_of (char *host_name, soc_host *p_host) {
  struct hostent *host_struct;

  /* Read  IP adress of host */
  if ((host_struct = gethostbyname(host_name)) == NULL) {
    h_perror ("gethostbyname");
    return (BS_ERROR);
  }
  memcpy((void *) &(p_host->integer),
     (void *) host_struct->h_addr, sizeof(p_host->integer));

  /* Ok */
  return (BS_OK);
}

/* Send to a socket, the destination of which must set */
extern int soc_send (soc_token token, soc_message message, soc_length length) {
  soc_ptr soc = (soc_ptr) token;
  boolean cr;
  soc_header header;
  struct iovec vector[VECTOR_LEN];

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Check that desination is set */
  if (!soc->dest_set) return (BS_ERROR); 

  /* Connected if tcp */
  if ( (soc->proto == tcp_socket) && (!soc->connected) ) {
     return (BS_ERROR);
  }

  /* Fill tcp header and vector */
  if (soc->proto == tcp_socket) {
    header.magic_number = htonl(MAGIC_NUMBER);
    header.size = htonl(length);
    vector[0].iov_base = (void*)&header;
    vector[0].iov_len = sizeof(header);
    vector[1].iov_base = message;
    vector[1].iov_len = length;
  }

    

  if (!soc->blocking) {

    /* If non blocking socket set it blocking to send */
    soc_set_blocking (soc, TRUE);

    /* Send */
    do {
      if (soc->proto == tcp_socket) {
        cr = (writev(soc->socket_id, vector, VECTOR_LEN) >= 0);
      } else {
        cr = (sendto(soc->socket_id, (char *)message, length, 0,
         (struct sockaddr*) &(soc->send_struct), socklen) >= 0);
      }
    } while ( ( ! cr ) && (errno == EINTR) );

    /* Set it back to non blocking */
    soc_set_blocking (soc, FALSE);

  } else { 

    /* Send */
    do {
      if (soc->proto == tcp_socket) {
        cr = (writev(soc->socket_id, vector, VECTOR_LEN) >= 0);
      } else {
        cr = (sendto(soc->socket_id, (char *)message, length, 0,
         (struct sockaddr*) &(soc->send_struct), socklen) >= 0);
      }
    } while ( ( ! cr ) && (errno == EINTR) );

  }

  /* Ok */
  if ( cr ) return (BS_OK);
  else {
    perror ("sendto");
    return (BS_ERROR);
  }
}

/* Links the socket to a port specified by the service */
/* The socket must be open and not already linked */
extern int soc_link_service (soc_token token, const char *service) {
  soc_ptr soc = (soc_ptr) token;
  struct servent *serv_name;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Not connected nor already linked if tcp */
  if ( (soc->proto == tcp_socket) &&
       (soc->linked || soc->connected) ) {
     return (BS_ERROR);
  }

  /* Read IP adress of port */
  if ((serv_name = getservbyname(service, ns_proto[soc->proto]))== NULL) {
    perror ("getservbyname");
    return (BS_ERROR);
  }

  /* Init structure */
  soc->rece_struct.sin_port = serv_name->s_port;

  /* Bind */
  if (bind (soc->socket_id, (struct sockaddr*) &(soc->rece_struct),
            socklen ) < 0) {
    perror ("bind");
    return (BS_ERROR);
  }

  /* Listen for tcp */
  if (soc->proto == tcp_socket) {
    if (listen (soc->socket_id, SOMAXCONN) < 0) {
      perror ("listen");
      return (BS_ERROR);
    }
  }

  /* Ok */
  soc->linked = TRUE;
  return (BS_OK);
}	

/* Links the socket to a port specified by it's value */
/* The socket must be open and not already linked */
extern int soc_link_port  (soc_token token, soc_port port) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Not connected nor already linked if tcp */
  if ( (soc->proto == tcp_socket) &&
       (soc->linked || soc->connected) ) {
     return (BS_ERROR);
  }

  /* Init structure */
  soc->rece_struct.sin_port = htons((u_short) port);

  /* Bind */
  if (bind (soc->socket_id,
   (struct sockaddr*) &(soc->rece_struct), socklen ) < 0) {
    perror ("bind");
    return (BS_ERROR);
  }

  /* Listen for tcp */
  if (soc->proto == tcp_socket) {
    if (listen (soc->socket_id, SOMAXCONN) < 0) {
      perror ("listen");
      return (BS_ERROR);
    }
  }

  /* Ok */
  soc->linked = TRUE;
  return (BS_OK);
}	

/* Links the socket to a port dynamically choosen by the system */
/* The socket must be open and not already linked */
extern int soc_link_dynamic  (soc_token token) {
  soc_ptr soc = (soc_ptr) token;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Not connected nor already linked if tcp */
  if ( (soc->proto == tcp_socket) &&
       (soc->linked || soc->connected) ) {
     return (BS_ERROR);
  }

  /* Init structure Port is not set */
  soc->rece_struct.sin_port = htons(0);

  /* Bind */
  if (bind (soc->socket_id,
   (struct sockaddr*) &(soc->rece_struct), socklen ) < 0) {
    perror ("bind");
    return (BS_ERROR);
  }

  /* Listen for tcp */
  if (soc->proto == tcp_socket) {
    if (listen (soc->socket_id, SOMAXCONN) < 0) {
      perror ("listen");
      return (BS_ERROR);
    }
  }

  /* Ok */
  soc->linked = TRUE;
  return (BS_OK);
}	

/* Gets the port to which is linked a socket (0 if failure) */
extern int soc_get_linked_port  (soc_token token, soc_port *p_port) {
  soc_ptr soc = (soc_ptr) token;
  int len = socklen;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Check that a port is already set */
  if (!soc->linked) return (BS_ERROR);

  /* Get the structure */
  if (getsockname (soc->socket_id,
   (struct sockaddr*) (&soc->rece_struct), &len) < 0) {
    perror ("getsockname");
    *p_port = 0;
    return (BS_OK);
  }

  /* Ok */
  *p_port = ntohs(soc->rece_struct.sin_port);
  return (BS_OK);
}
 
/* To test if there is a message to receive and receive it */
/* Err if error, recu=TRUE if a message is ok and FALSE elsewhere */
/* CARE : length is an in out parameter and must be initialized with */
/*  the size of the buffer */
/* The socket must be open, linked in udp and not linked in tcp */
/*  After success, the socket may be ready for a send to reply */
/* No set_for_reply if tcp */
extern int soc_receive (soc_token token, boolean *p_received,
                        soc_message message, soc_length *p_length,
                        boolean set_for_reply) {
  soc_ptr soc = (soc_ptr) token;
  int result;
  soc_length loc_len = *p_length;
  int len;
  struct sockaddr_in *from_addr;
  soc_header header;


  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Check set_for_reply and linked vs proto */
  if (soc->proto == tcp_socket) {
    if (set_for_reply)  return (BS_ERROR);
    if (soc->linked) return (BS_ERROR);
    if (!soc->connected) return (BS_ERROR);
  } else {
    if (!soc->linked) return (BS_ERROR);
  }

  /* Prepare for reply or not */
  if (set_for_reply) {
    from_addr = &(soc->send_struct);
    len = socklen;
    /* In case of error */
    soc->dest_set = FALSE;
  } else {
    from_addr = NULL;
    len = 0;
  }

  /* Init out values (CARE: length is in out) */
  *p_received = FALSE;
  *p_length = loc_len;

  /* Receive */
  if (soc->proto == tcp_socket) {
    do {
      result = recv(soc->socket_id, (char *)&header, sizeof(header), 0);
    } while ( (result == -1) && (errno == EINTR) );

    /* Check data, header and size */
    if (result > 0) {
      /* Disconnect if invalid magic num */
      if (ntohl(header.magic_number) != MAGIC_NUMBER) {
        *p_length = 0;
        return (BS_OK);
      }
      /* Check length vs size: drop message */
      if (*p_length < ntohl(header.size) ) {
        do {
          result = recv(soc->socket_id, (char *)message, ntohl(header.size), 0);
        } while ( (result == -1) && (errno == EINTR) );
        return (BS_ERROR);
      }
      /* Read message */
      *p_length = ntohl(header.size);
      do {
        result = recv(soc->socket_id, (char *)message, *p_length, 0);
      } while ( (result == -1) && (errno == EINTR) );
    }
  } else {
    do {
      result = recvfrom(soc->socket_id, (char *)message, 
       *p_length, 0, (struct sockaddr*) from_addr, &len);
    } while ( (result == -1) && (errno == EINTR) );
  }

  *p_length = 0;

  if (result < 0) {
    if ( (errno != EWOULDBLOCK)
      && (errno != ECONNRESET) 
      && (errno != EPIPE) 
      && (errno != ECONNREFUSED) ) {
      /* Reception error */
        perror ("recvfrom");
        return (BS_ERROR);
    } else {
      /* No message to read */
      return (BS_OK);
    }
  } else {
    /* A message read */
    if (set_for_reply) {
      soc->dest_set = TRUE;
    }
    *p_received = TRUE;
    *p_length = result;
    return (BS_OK);

  }
}


/* Tcp specific calls */

/* Accept a connection.
/* The socket must be open, tcp and linked */
/* A new socket is created (tcp) with dest set */
extern int soc_accept (soc_token token, soc_token *p_token) {
  soc_ptr soc = (soc_ptr) token;
  soc_ptr *p_soc = (soc_ptr*) p_token;

  int result;
  struct sockaddr from_addr;
  int len = socklen;

  /* Check that socket is open */
  if (soc == NULL) return (BS_ERROR);

  /* Check that new socket is not already open */
  if (*p_soc != NULL) return (BS_ERROR);

  /* Check tcp and linked */
  if (soc->proto != tcp_socket)  return (BS_ERROR);
  if (! soc->linked) return (BS_ERROR);

  /* Accept */
  do {
    result = accept (soc->socket_id, &from_addr, &len);
  } while ( (result == -1) && (errno == EINTR) );

  /* Check result */
  if (result < 0) {
    perror ("accept");
    return (BS_ERROR);
  }

  /* Create structure */
  *p_soc = (soc_ptr) malloc (sizeof (soc_struct));
  if (*p_soc==NULL) return (BS_ERROR);

  /* Save id and protocol */
  (*p_soc)->socket_id = result;
  (*p_soc)->proto = tcp_socket;
  (*p_soc)->blocking = TRUE;
  (*p_soc)->connected = TRUE;

  /* Blocking operations as default */
  /* Ioctl for having blocking receive */
  if (ioctl ((*p_soc)->socket_id, FIONBIO, (char *) &BLOCKINGIO) < 0) {
    perror ("ioctl");
    close ((*p_soc)->socket_id);
    free (*p_soc);
    *p_soc = NULL;
    return (BS_ERROR);
  }

  /* Init structures */
  (*p_soc)->send_struct.sin_family = AF_INET;
  (*p_soc)->rece_struct.sin_addr.s_addr = htonl(INADDR_ANY);
  (*p_soc)->rece_struct.sin_family = AF_INET;

  /* Close on exec */
  if (fcntl((*p_soc)->socket_id, F_SETFD, FD_CLOEXEC) < 0) {
    perror ("fcntl");
    close ((*p_soc)->socket_id);
    free (*p_soc);
    *p_soc = NULL;
    return (BS_ERROR);
  }

  /* Set dest */
  memcpy (&(*p_soc)->send_struct , &from_addr, len);
  (*p_soc)->dest_set = TRUE;

  /* Ok */
  (*p_soc)->linked = FALSE;
  (*p_soc)->blocking = TRUE;
  return (BS_OK);

}

extern int soc_is_connected (soc_token token, boolean *p_connected) {
  soc_ptr soc = (soc_ptr) token;


  /* Check that socket is open and tcp */
  if (soc == NULL) return (BS_ERROR);
  if (soc->proto != tcp_socket)  return (BS_ERROR);

  *p_connected = soc->connected;
  return (BS_OK);

}

