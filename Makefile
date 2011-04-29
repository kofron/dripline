C_SRC=$(wildcard priv/*.c)
C_OBJ=$(C_SRC:.c=.so)
ERL_ROOT=/usr/lib/erlang
NIF_INC=$(ERL_ROOT)/usr/include
LDFLAGS=-shared -fPIC
ifeq ($(shell uname), Darwin)
	ERL_ROOT=/usr/local/Cellar/erlang/R14B02/lib/erlang/
	LDFLAGS+=-flat_namespace -undefined suppress
endif

all: $(C_OBJ) erl_src

erl_src:
	@./rebar compile

%.so: %.c
	@echo building shared library $@...
	@gcc $(LDFLAGS) -I$(NIF_INC) $< -o $@

clean: clean_emacs clean_priv
	@echo cleaning up after erl -make...
	@./rebar clean

clean_priv:
	@echo cleaning up shared objects...
	@rm priv/*.so

clean_emacs:
	@echo cleaning up after emacs...
	@find . -name '*~' | xargs -I{} rm {}