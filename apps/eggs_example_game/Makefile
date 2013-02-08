APP:=example_game
ERL:=erl
EPATH:=ebin deps/*/ebin

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