#include <errno.h>

#include "socket.h"
#include "socket_net.h"


const char* ns_proto[3] = {"udp", "tcp", "tcp"};
typedef enum {not_connected, connecting, connected} connect_state;

/* Structure pointed by the token */
typedef struct {
        socket_protocol proto;
        boolean         dest_set;
        boolean         linked;
        boolean         blocking;
        connect_state   connection;
        int    socket_id;
        struct sockaddr_in send_struct;
        struct sockaddr_in rece_struct;
        char * send_tail;
        soc_length send_len;
        char * rece_head;
        soc_length rece_len;
        soc_length expect_len;
}soc_struct, *soc_ptr;

int socklen = sizeof (struct sockaddr_in);
int    BLOCKINGIO = 0;
int NONBLOCKINGIO = 1;

/* In tcp we send a vector of 2 member: header and data */

#define VECTOR_LEN 2

#define MAGIC_NUMBER 21
typedef struct {
       unsigned int magic_number;
       unsigned int size; /* Size of data */
}soc_header;

