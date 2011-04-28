all: compile clean_emacs

compile:
	@./rebar compile

clean: clean_emacs
	@./rebar clean

clean_emacs:
	@echo cleaning up after emacs...
	@find . -name '*~' | xargs -I{} rm {}