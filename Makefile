all: compile clean_emacs

compile:
	@./rebar compile

clean_emacs:
	@echo cleaning up after emacs...
	@find . -name '*~' | xargs -I{} rm {}