#include <stdlib.h>
#include <nice/agent.h>
#include <sys/socket.h>
#include <string.h>

#include "marshal.h"

#include <stdio.h>

void set_foundation(NiceCandidate* cand, char* found) {
  strncpy(cand->foundation, found, NICE_CANDIDATE_MAX_FOUNDATION);
}

void get_nice_candidate_addr_ptr(NiceAddress** dst, NiceCandidate* cand) {
    *dst = &cand->addr;
}

void get_nice_candidate_base_addr_ptr(NiceAddress** dst, NiceCandidate* cand) {
    *dst = &cand->base_addr;
}

int copy_to_sockaddr_check(NiceAddress* addr, struct sockaddr* sa) {
    nice_address_copy_to_sockaddr(addr, sa);
    switch (addr->s.addr.sa_family) {
        case AF_INET: return 1; break;
        case AF_INET6: return 1; break;
    default: return 0;
    }
}
