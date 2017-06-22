#include <nice/agent.h>
#include <sys/socket.h>

typedef struct sockaddr_storage sa_storage;

void set_foundation(NiceCandidate* cand, char* found);

int copy_to_sockaddr_check(NiceAddress* addr, struct sockaddr* sa);

void get_nice_candidate_addr_ptr(NiceAddress** dst, NiceCandidate* cand);
void get_nice_candidate_base_addr_ptr(NiceAddress** dst, NiceCandidate* cand);
