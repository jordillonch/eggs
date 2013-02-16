APP:=example_game
ERL:=erl
EPATH:=ebin deps/*/ebin
APPS:=erts stdlib kernel crypto sasl compiler syntax_tools
PLT:=$(HOME)/.eggs_dialyzer_plt

all: get-deps compile example_game.boot

compile:
	./rebar compile

get-deps:
	./rebar get-deps

clean:
	./rebar clean

$(APP).boot: $(APP).rel Makefile
	$(ERL) -pa $(EPATH) -noshell +B -eval \
	'case systools:make_script("$(basename $@)",[local]) of ok -> halt(0); _ -> halt(1) end.'

$(APP).rel: ebin/$(APP).app Makefile
	$(ERL) -pa $(EPATH) -noshell +B -eval \
	"ok = application:load($(basename $@)), \
	 {ok, Apps} = application:get_key($(basename $@), applications), \
	 {ok, F} = file:open("$@", [write]), \
	 io:format(F, \"~p.~n\", [{release, {\"$(basename $@)\", \"$(SRCREV)\"}, \
	 {erts, erlang:system_info(version)}, \
	 lists:map(fun (App) -> application:load(App), \
	    {ok, Vsn} = application:get_key(App, vsn), \
        {App, Vsn} end, Apps ++ [$(basename $@)])}]), \
	 file:close(F), halt(0)."

build_plt: compile
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS) deps/*/ebin

check_plt: compile
	dialyzer --check_plt --plt $(PLT) --apps $(APPS) deps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	dialyzer --plt $(PLT) -Wno_undefined_callbacks ebin


