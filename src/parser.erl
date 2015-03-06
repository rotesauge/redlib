%%%-------------------------------------------------------------------
%%% @author sev_alex
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. июн 2014 8:24
%%%-------------------------------------------------------------------
-module(parser).
-author("sev_alex").

%% API
-export([parse/1]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Parse expression string and exec it 
%%
%% @spec parse(Str) -> term()
%% @end
%%--------------------------------------------------------------------
-spec(parse(Str :: string()) ->
  term() | {error, Reason :: term()}).
parse(Str)->
  Code=generate_code(Str),
  file:write_file("tmplt.erl",Code),
  compile:file("tmplt.erl"),
  Ans=tmplt:pfunc(),
  code:purge(tmplt),
  code:delete(tmplt),
  file:delete("tmplt.erl"),
  file:delete("tmplt.beam"),
  Ans.

generate_code(Str)->
  Header = "-module(tmplt).
           -export([pfunc/0]).
           pfunc()->",
  list_to_binary(lists:append([Header,Str,"."])).
