#ifndef __UDP_H__
#define __UDP_H__

#include <stdio.h>
#include "boolean.h"

#ifndef BS_OK
#define BS_OK 0
#endif
#ifndef BS_ERROR
#define BS_ERROR -1
#endif

/* String terminator */
#define NUL '\0'

/* Byte of 8 bits.  Values from 0 to   255. */
#define byte unsigned char

/* Word of 16 bits. Values from 0 to 65535. */
#define word unsigned short int


/* Variables and types */
/* ------------------- */

/* token socket : a pointer (abstract data type) */
/* Has to be initialised to init_soc */
typedef void * soc_token;
#define init_soc  ((soc_token*) NULL)

/* Port of socket : a word */
#define soc_port word 

/* Host address (network format) */
typedef union soc_host_t {
    /* 4 bytes in network order */
    unsigned int integer;
    unsigned char bytes[4];
} soc_host;

/* String length for a host or a port */
#define SOC_MAX_LEN	50

/* A message */
typedef int soc_length;
typedef void * soc_message;


/* Open a socket (in blocking) */
extern int soc_open (soc_token *p_token);

/* Close a socket */
extern int soc_close (soc_token *p_token);

/* Gets the id of a socket (for select, ioctl ... ) */
extern int soc_get_id (soc_token token, int *p_id);

/* Set the socket blocking or non blocking */
/*  (for sendind and receiving) */ 
extern int soc_set_blocking (soc_token token, boolean blocking);

/* Set the destination host/lan name and port - specify service */
/* Broadcast if lan */
extern int soc_set_dest_service (soc_token token, char *host_lan, boolean lan, 
                                 const char *service);

/* Set the destination host name and port - specify port */
/* Broadcast if lan */
extern int soc_set_dest_port (soc_token token, char *host_lan, boolean lan, 
                              soc_port port);

/* Change destination host_lan name (same port) */
/* Destination must have been previously set (by a set or a rece) */
/* Broadcast if lan */
extern int soc_change_dest_host (soc_token token, char *host_lan, boolean lan);

/* Set dest to a host, port */
extern int soc_set_dest (soc_token token, soc_host host, soc_port port);

/* Get current lan name (computed from local host name) */
/* lan_name must be big enough */
extern int soc_get_lan_name (char *lan_name, unsigned int lan_name_len);

/* Change destination port (same host_lan) - specify service */
extern int soc_change_dest_service (soc_token token, char *service);

/* Change destination port (same host_lan) - specify port */
extern int soc_change_dest_port (soc_token token, soc_port port);

/* Get the destination port */
extern int soc_get_dest_port (soc_token token, soc_port *p_port);

/* Get the destination host */
extern int soc_get_dest_host (soc_token token, soc_host *p_host);

/* Find name of soc_host and vice versa */
extern int soc_host_name_of (soc_host *p_host, char *host_name,
                             unsigned int host_name_len);
extern int soc_host_of (char *host_name, soc_host *p_host);

/* Send to a socket, the destination of which must set */
extern int soc_send (soc_token token, soc_message message, soc_length length);

/* Links the socket to a port specified by the service */
/* The socket must be open and not already linked */
extern int soc_link_service (soc_token token, const char *service);

/* Links the socket to a port specified by it's value */
/* The socket must be open and not already linked */
extern int soc_link_port (soc_token token, soc_port port);

/* Links the socket to a port dynamically choosen by the system */
/* The socket must be open and not already linked */
extern int soc_link_dynamic (soc_token token);

/* Gets the port to which is linked a socket */
extern int soc_get_linked_port (soc_token token, soc_port *p_port);
 
/* To test if there is a message to receive and receive it */
/* Err if error, p_receive=true if a message is ok and false otherwise */
/* CARE : length is an in out parameter and must be initialized with */
/*  the size of the buffer */
/* The socket must be open and linked */
/*  After success, the socket may be ready for a send to reply */
extern int soc_receive (soc_token token, boolean *p_received,
                        soc_message message, soc_length *p_length,
                        boolean set_for_reply);

#endif /* __UDP_H__ */

