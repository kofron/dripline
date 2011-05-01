# the basic necessities to build the project
C_SRC=$(wildcard priv/*.c)
C_OBJ=$(C_SRC:.c=.so)
ERL_ROOT=$(shell escript get_root.erl)
NIF_INC=$(ERL_ROOT)/usr/include
CFLAGS=-std=c99
LDFLAGS=-shared -fPIC

# we need to add these flags for LD if we're on OSX to
# satisfy the linker.
ifeq ($(shell uname), Darwin)
	LDFLAGS+=-flat_namespace -undefined suppress
endif

# we don't use rebar to build the shared libraries, we
# have a custom build.  so build the C objects, and then
# use rebar to build the erlang code.
all: $(C_OBJ) erl_src

erl_src:
	@./rebar compile

%.so: %.c
	@echo building shared library $@...
	@gcc $(CFLAGS) $(LDFLAGS) -I$(NIF_INC) $< -o $@

# clean up
clean: clean_emacs clean_priv
	@echo cleaning up after erl -make...
	@./rebar clean

# remove shared objects from priv dir
clean_priv:
	@echo cleaning up shared objects...
	@rm priv/*.so

# delete temporaries and tilde files
clean_emacs:
	@echo cleaning up after emacs...
	@find . -name '*~' | xargs -I{} rm {}