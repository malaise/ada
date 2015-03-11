#ifndef __SOCKET_H__
#define __SOCKET_H__

#include <stdio.h>
#include "boolean.h"

/* Variables and types */
/* ------------------- */

/* OK */
#define SOC_OK        0

/* Errors, see also soc_error() */
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
/* Called soc_send instead of soc_resend despite prev send returned SOC_WOULD_BLOCK */
#define SOC_TAIL_ERR    -9
/* Call not allowed for this protocol */
#define SOC_PROTO_ERR  -10
/* Close while fd used in select */
#define SOC_FD_IN_USE  -11
/* Lan/host address or port string format error */
# define SOC_FMT_ERR   -12

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
/* Set_for_reply could not set the ipm sending interface */
#define SOC_REPLY_IFACE    -27

/* String terminator */
#define NUL '\0'

/* Byte of 8 bits.  Values from 0 to   255. */
#define byte unsigned char

/* Word of 16 bits. Values from 0 to 65535. */
#define word unsigned short int

/* Protocols */
typedef enum {udp_socket=0, tcp_socket=1, tcp_header_socket=2,
              tcp_afux_socket=3, tcp_header_afux_socket=4} socket_protocol;

/* Blocking mode */
/* full_blocking: soc_accept, soc_receive, soc_send, soc_resend and */
/*                soc_set_dest_xxx are blocking */
/* blocking_send: soc_accept and soc_receive are non blocking */
/* non_blocking: connecting, sending, accepting and receiving are non */
/*               blocking */
/* The mode where only reception is blocking is not usefull */
typedef enum {full_blocking, blocking_send, non_blocking} blocking_mode;

/* Token socket : a pointer (abstract data type) */
/* Has to be initialised to init_soc */
typedef void * soc_token;
#define init_soc  ((soc_token*) NULL)

/* Port of socket : a word */
#define soc_port word

/* Host address (network format) */
typedef union soc_host_t {
    /* 4 bytes in network order */
    unsigned int integer;
    byte bytes[4];
} soc_host;

/* String length for a host or a port */
#define SOC_MAX_LEN 50

/* A message */
typedef int soc_length;
typedef void * soc_message;

/* Returns the string associated to an error code */
extern const char * soc_error (const int code);

/*----------------------------------------------------------------*/
/* All functions return an error or SOC_OK except soc_receive:    */
/*  on non full_blocking tcp no header it returns the read length */
/*  on udp it returns the real length of the message sent         */
/*----------------------------------------------------------------*/

/*-------------------------------------------------------------------------*/
/*  Note for Multicast IP (using Udp socket):                              */
/*  For sending IPM, simply Set_Destination to a LAN name                  */
/*    which is defined with a D class address, and a port.                 */
/*  For receiving IPM, first Set_Destination to the LAN name and port,     */
/*    then Link to the same port as this destination.                      */
/*    It is possible to link to a dynamic port (then port set in           */
/*    destination port is    not used).                                    */
/*  By default IPM emission and reception are from/to the "natural"        */
/*    interface (local host id). A specific interface can be specified     */
/*    before setting destination / before linking to port. If interface is */
/*    set for reception, then the set_for_reply flag of soc_receive sets   */
/*    the sending interface to it.                                         */
/*-------------------------------------------------------------------------*/


/* ------------------*/
/* General functions */
/* ------------------*/
/* Open a socket (in blocking mode) */
extern int soc_open (soc_token *p_token, socket_protocol protocol);

/* Close a socket */
extern int soc_close (soc_token *p_token);

/* Gets the id of a socket (for select, ioctl ... ) */
extern int soc_get_id (soc_token token, int *p_id);

/* Set the socket blocking or non blocking */
/*  (for connecting and sending, for receiving) */
/* Socket is full_blocking at creation (open/accept) */
extern int soc_set_blocking (soc_token token, blocking_mode blocking);

/* What is the socket blocking mode */
extern int soc_get_blocking (soc_token token, blocking_mode *blocking);

/* Get socket protocol */
extern int soc_get_protocol (soc_token token, socket_protocol *protocol);

/* Set the TTL. Socket must be either udp or tcp non afux */
/*  (otherwise SOC_PROTO_ERR) */
extern int soc_set_ttl (soc_token token, byte ttl);

/* Get the TTL. Socket must be either udp or tcp non afux (otherwise */
/*  SOC_PROTO_ERR) */
extern int soc_get_ttl (soc_token token, byte *ttl);

/*-------------------------------------*/
/* Emission                            */
/* No broadcast nor change dest in tcp */
/* Socket must not be linked in tcp    */
/*-------------------------------------*/

/* Set the interface on which send mutlicast IP (udp_socket). */
/* Set it before setting destination. */
/* 0 to reset */
/* Beware that setting the sending interface is not always supported */
/*  and may require to be root (Set_Destination will return SOC_SYS_ERR). */
extern int soc_set_send_ipm_interface (soc_token token, const soc_host *host);

/* Set the destination host/lan name and port - specify service */
/* Broadcast if lan */
extern int soc_set_dest_name_service (soc_token token, const char *host_lan,
                                      boolean lan, const char *service);

/* Set the destination host name and port - specify port */
/* Broadcast if lan */
extern int soc_set_dest_name_port (soc_token token, const char *host_lan,
                                   boolean lan, soc_port port);

/* Set dest to a host, service */
extern int soc_set_dest_host_service (soc_token token, const soc_host *host,
                                      const char *service);
/* Set dest to a host, port */
extern int soc_set_dest_host_port (soc_token token, const soc_host *host,
                                   soc_port port);

/* Change destination host_lan name (same port) */
/* Destination must have been previously set (by a set or a rece) */
/* Broadcast if lan */
extern int soc_change_dest_name (soc_token token, const char *host_lan,
                                 boolean lan);

/* Change destination host (same port) */
extern int soc_change_dest_host (soc_token token, const soc_host *host);

/* Change destination port (same host_lan) - specify service */
extern int soc_change_dest_service (soc_token token, const char *service);

/* Change destination port (same host_lan) - specify port */
extern int soc_change_dest_port (soc_token token, soc_port port);

/* Get the destination port */
extern int soc_get_dest_port (soc_token token, soc_port *p_port);

/* Get the destination host */
extern int soc_get_dest_host (soc_token token, soc_host *p_host);

/* Send to a socket, the destination of which must be set */
/* May return SOC_WOULD_BLOCK, then next tries have to be made */
/*  with soc_resend util it returns ok */
extern int soc_send (soc_token token, soc_message message, soc_length length);

/* Resend tail of previous message */
extern int soc_resend (soc_token token);

/*---------------------------------------------*/
/* Search in local host/port/network databases */
/*---------------------------------------------*/

/* Find name of soc_host host and vice versa */
extern int soc_host_name_of (const soc_host *p_host, char *host_name,
                             unsigned int host_name_len);
extern int soc_host_of (const char *host_name, soc_host *p_host);

/* Find name of soc_lan LAN and vice versa */
extern int soc_lan_name_of (const soc_host *p_lan, char *lan_name,
                             unsigned int lan_name_len);
extern int soc_lan_of (const char *lan_name, soc_host *p_lan);

/* Find name of soc_port and vice versa */
extern int soc_port_name_of (const soc_port port,
                             const socket_protocol proto,
                             char *port_name,
                             unsigned int port_name_len);
extern int soc_port_of (const char *port_name,
                        const socket_protocol proto,
                        soc_port *p_port);

/* Gets local host and LAN (computed from local host) */
extern int soc_get_local_host_name (char *host_name,
                                    unsigned int host_name_len);
extern int soc_get_local_host_id (soc_host *p_host);
extern int soc_get_local_lan_name (char *lan_name, unsigned int lan_name_len);
extern int soc_get_local_lan_id (soc_host *p_lan);

/* Get bcast address for a given interface (designated by if_host) */
extern int soc_get_bcast (const soc_host *if_host, soc_host *p_bcast_host);

/* Get the soc_host local host on a given LAN and netmask */
extern int soc_get_host_iface (soc_host *lan, soc_host *netmask,
                               soc_host *p_host);

/* String "x.y.z.t" to host, and string to port conversions */
extern int soc_str2host (const char *str, soc_host *p_host);

extern int soc_str2port (const char *str, soc_port *p_port);

/* ------------------------------------*/
/* Receive                             */
/* Socket must not be connected in tcp */
/* ------------------------------------*/

/* Set the interface on which to link, except for afux. */
/* Set it before linking. */
extern int soc_set_rece_interface (soc_token token, const soc_host *host);

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
/*   - the length of bytes read, which is usually the length of the message */
/*     sent. In tcp no header it may be anything from 0 to length. */
/*     In udp it is the sent message length (maybe > length, or 0). */
/*   - SOC_WOULD_BLOCK (in non blocking), new read has to be done */
/*   - SOC_READ_0, tcp disconnection? */
/*   - any other (fatal) error */
extern int soc_receive (soc_token token,
                        soc_message message, soc_length length,
                        boolean set_for_reply);

/*--------------------*/
/* Tcp specific calls */
/*--------------------*/

/* Accept a connection. */
/* The socket must be open, tcp or tcp_header and linked */
/* A new socket is created (tcp or tcp_header, same as token) with dest set */
/* May return SOC_WOULD_BLOCK (either in blocking OR non blocking) if no    */
/*  valid connection to accept. Go back to wait. */
extern int soc_accept (soc_token token, soc_token *p_token);

/* Is a tcp socket connected following a soc_set_... or a soc_accept */
extern int soc_is_connected (soc_token token, boolean *p_connected);


/* __SOCKET_H__ */
#endif

