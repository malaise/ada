#include <errno.h>

#include "udp.h"
#include "udp_net.h"



#define         NAME_SERVER_PROTO              "udp"

/* Structure pointed by the token */
typedef struct {
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


