#ifndef __SOCKET_H__
#define __SOCKET_H__

#include <stdio.h>
#include "boolean.h"

/* Variables and types */
/* ------------------- */

/* OK */
#define SOC_OK        0

/* Errors */
/* Socket not open, already open */
#define SOC_USE_ERR     -1
/* System error (traced and errno set) */
#define SOC_SYS_ERR     -2
/* Dest set or not set */
#define SOC_DEST_ERR    -3
/* Linked or not linked */
#define SOC_LINK_ERR    -4
/* Connected or not connected */
#define SOC_CONN_ERR    -5
/* Bcast when not allowed by proto */
#define SOC_BCAST_ERR   -6
/* Length to short */
#define SOC_LEN_ERR     -7
/* Set_for_reply set or not set */
#define SOC_REPLY_ERR   -8
/* Sent message length is not 0 while prev send returned SOC_WOULD_BLOCK */
#define SOC_TAIL_ERR    -9
/* Call not allowed for this protocol */
#define SOC_PROTO_ERR  -10


/* Failures */
/* Connection refused */
#define SOC_CONN_REFUSED   -21
/* Get*by*: entry not found */
#define SOC_NAME_NOT_FOUND -22
/* Operation would block - do it again */
#define SOC_WOULD_BLOCK    -23
/* Connection lost */
#define SOC_CONN_LOST      -24
/* Address in use. Close-wait? */
#define SOC_ADDR_IN_USE    -25
/* Read -> 0 : disconnection if after a select */
#define SOC_READ_0         -26



/* String terminator */
#define NUL '\0'

/* Byte of 8 bits.  Values from 0 to   255. */
#define byte unsigned char

/* Word of 16 bits. Values from 0 to 65535. */
#define word unsigned short int

/* Protocols */
typedef enum {udp_socket=0, tcp_socket=1, tcp_header_socket=2} socket_protocol;

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
extern int soc_open (soc_token *p_token, socket_protocol protocol);

/* Close a socket */
extern int soc_close (soc_token *p_token);

/* Gets the id of a socket (for select, ioctl ... ) */
extern int soc_get_id (soc_token token, int *p_id);

/* Set the socket blocking or non blocking */
/*  (for sendind and receiving) */ 
extern int soc_set_blocking (soc_token token, boolean blocking);

/* No broadcast nor change dest in tcp */
/* Socket must not be linked in tcp    */
/* ------------------------------------*/

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
/* May return SOC_WOULD_BLOCK, then next sends have to be made */
/*  with length=0, util soc_send returns ok */
extern int soc_send (soc_token token, soc_message message, soc_length length);

/* Resed tail of previous message */
extern int soc_resend (soc_token token);

/* Socket must not be connected in tcp */
/* ------------------------------------*/

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
/* Length must be initialized with the size of the buffer */
/* The socket must be open, linked in udp and not linked in tcp */
/*  After success, the socket may be ready for a send to reply */
/* No set_for_reply if tcp */
/* Returned values: */
/*   - the length of bytes read, which the length of the message sent */
/*     except in tcp (no header) where the length read my me anything */
/*     from 0 to length */
/*   - SOC_WOULD_BLOCK (in  non blocking), new read has to be done */
/*   - any other (fatal) error */
extern int soc_receive (soc_token token,
                        soc_message message, soc_length length,
                        boolean set_for_reply);

/* Tcp specific calls */

/* Accept a connection.
/* The socket must be open, tcp or tcp_header and linked */
/* A new socket is created (tcp or tcp_header, same as token) with dest set */
extern int soc_accept (soc_token token, soc_token *p_token);

/* Is a tcp socket connected fllowing a soc_set_... or a soc_accept */
extern int soc_is_connected (soc_token token, boolean *p_connected);


/* __SOCKET_H__ */
#endif

