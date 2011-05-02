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

/* scan/3
 * scan the backplane for installed cards.  returns a proplist to erlang
 * in the form of [{slot::atom(), model::atom()}]
 */
static ERL_NIF_TERM scan(ErlNifEnv* env, 
			 int argc, 
			 const ERL_NIF_TERM argv[])
{
  // There are 4 card slots.  Iterate over all of them and read the PROM
  ERL_NIF_TERM res[4];

  // Obviously this does absolutely nothing right now but return a dummy
 res[0] = enif_make_tuple2(env,
			   enif_make_atom(env,"cardA"),
			   enif_make_atom(env,"ios320"));
 res[1] = enif_make_tuple2(env,
			   enif_make_atom(env,"cardB"),
			   enif_make_atom(env,"ios330"));
 res[2] = enif_make_tuple2(env,
			   enif_make_atom(env,"cardC"),
			   enif_make_atom(env,"ios408"));
 res[3] = enif_make_tuple2(env,
			   enif_make_atom(env,"cardD"),
			   enif_make_atom(env,"nocard"));

  return enif_make_list_from_array(env,res,4);
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
  {"scan",0,scan}
};

ERL_NIF_INIT(dripline_backplane,backplane_funcs,&load_init,
	     NULL,NULL,NULL)
