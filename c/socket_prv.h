#include <errno.h>

#ifdef SOCKET_MUTEX
#include "mutex.h"
#endif

#include "socket.h"
#include "socket_net.h"

typedef enum {tcp_protocol, udp_protocol} protocol_list;
const char* ns_proto[2] = {"tcp", "udp"};

typedef enum {tcp_raw, tcp_msg} tcp_list;

typedef enum {unix_domain, inet_domain} domain_list;

typedef enum {not_connected, connecting, connected} connect_state;

/* Structure pointed by the token */
typedef struct {
#ifdef SOCKET_MUTEX
        mutex_t         mutex;
#endif
        socket_protocol socket_kind;
        protocol_list   protocol;
        tcp_list        tcp_kind;
        domain_list     domain;
        boolean         dest_set;
        boolean         linked;
        boolean         blocking;
        connect_state   connection;
        int    socket_id;
        struct sockaddr_in send_struct;
        struct in_addr     ipm_send_if;
        char * send_tail;
        soc_length send_len;
        struct sockaddr_in rece_struct;
        struct in_addr     ipm_rece_if;
        char * rece_head;
        soc_length rece_len;
        soc_length expect_len;
}soc_struct, *soc_ptr;

const socklen_t socklen = sizeof (struct sockaddr_in);
int    BLOCKINGIO = 0;
int NONBLOCKINGIO = 1;

/* In tcp we send a vector of 2 member: header and data */

#define VECTOR_LEN 2

#define MAGIC_NUMBER 21
typedef struct {
       unsigned int magic_number;
       unsigned int size; /* Size of data */
}soc_header;

