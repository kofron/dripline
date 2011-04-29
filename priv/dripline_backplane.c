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
typedef struct backplane backplane;
static ErlNifResourceType* backplane_rsrc;

static ERL_NIF_TERM start(ErlNifEnv* env, 
			  int argc, 
			  const ERL_NIF_TERM argv[])
{
  backplane* bp_handle = enif_alloc_resource(backplane_rsrc,
					     sizeof(backplane));
  ERL_NIF_TERM ret = enif_make_resource(env,bp_handle);
  enif_release_resource(bp_handle);
  return enif_make_tuple2(env,enif_make_atom(env,"ok"),ret);
}

static void unload_cleanup(ErlNifEnv* env, void* arg)
{
}

static int load_init(ErlNifEnv* env, 
		     void** priv_data, 
		     ERL_NIF_TERM load_info)
{
  backplane_rsrc = enif_open_resource_type(env,
					   "dripline_backplane",
					   "backplane_rsrc",
					   &unload_cleanup,
					   ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
					   0);
  return 0;
}

static ErlNifFunc backplane_funcs[] = {
  {"start",0,start}
};

ERL_NIF_INIT(dripline_backplane,backplane_funcs,&load_init,
	     NULL,NULL,NULL)
