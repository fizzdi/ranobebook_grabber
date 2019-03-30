all:
	./rebar get-deps
	./rebar compile

clean:
	./rebar clean
	

plugins:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH;escript do-plugins.escript)



copy-static:
	@(cp -r lib/nitrogen_core/www/* priv/static//nitrogen/)

