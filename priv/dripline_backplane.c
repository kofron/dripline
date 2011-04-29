/* dripline_backplane.c
 * written by Jared Kofron <jared.kofron@gmail.com>
 * NIF interface to the I/O server backplane.  Allows for scanning
 * of the bus for cards and initialization routines for the I/O carrier.
 */

#include "erl_nif.h"
#include "stdio.h"

struct backplane {
  int handle;  
};

static ERL_NIF_TERM start(ErlNifEnv* env, 
			  int argc, 
			  const ERL_NIF_TERM argv[])
{
  struct backplane* bp_handle = (struct backplane*)erl_nif_alloc(struct backplane, 
						   sizeof(backplane));
  ERL_NIF_TERM ret = enif_make_resource(env,bp_handle);
  enif_release_resource(bp_handle);
  return enif_make_tuple2(env,enif_make_atom(env,"ok"),ret);
}

static ErlNifFunc backplane_funcs[] = {
  {"start",0,start}
};

ERL_NIF_INIT(dripline_backplane,backplane_funcs,NULL,NULL,NULL,NULL)
