#include <errno.h>

#include "socket.h"
#include "socket_net.h"


char* ns_proto[2] = {"udp", "tcp"};

/* Structure pointed by the token */
typedef struct {
        socket_protocol proto;
	boolean		dest_set;
	boolean		linked;
        boolean		blocking;
	int		socket_id;
	struct sockaddr_in send_struct;
	struct sockaddr_in rece_struct;
}soc_struct, *soc_ptr;

int socklen = sizeof (struct sockaddr_in);
int    BLOCKINGIO = 0;
int NONBLOCKINGIO = 1;


