/*
 * include/types/lb_fwdns.h
 * Types for Fast Weighted Round Robin load balancing algorithm.
 *
 * Copyright (C) 2000-2009 Willy Tarreau - w@1wt.eu
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation, version 2.1
 * exclusively.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifndef _TYPES_LB_FWDNS_H
#define _TYPES_LB_FWDNS_H

#include <common/config.h>
#include <ebtree.h>

/* This structure is used to apply fast weighted round robin on a server group */
struct fwdns_group {
	struct eb_root curr;    /* tree for servers in "current" time range */
	struct eb_root t0, t1;  /* "init" and "next" servers */
	struct eb_root *init;   /* servers waiting to be placed */
	struct eb_root *next;   /* servers to be placed at next run */
	int curr_pos;           /* current position in the tree */
	int curr_weight;        /* total weight of the current time range */
	int next_weight;        /* total weight of the next time range */
};

struct lb_fwdns {
	struct fwdns_group act;	/* weighted round robin on the active servers */
	struct fwdns_group bck;	/* weighted round robin on the backup servers */
};

#endif /* _TYPES_LB_FWDNS_H */

/*
 * Local variables:
 *  c-indent-level: 8
 *  c-basic-offset: 8
 * End:
 */
