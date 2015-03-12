module(alg_generator).
-author("sev_alex").

%% API
-export([generate/1,generate/2,drop_elements/2]).
-define(DEFNAME, "alg").
-define(SAY(Format),?SAY(Format, [])).
-define(SAY(Format, Data),io:format(lists:append(["Module ~w Say: ",Format]),[{?MODULE, self()}|Data])).
-define(SAYVAL(Data),io:format("Module ~w Say VALUE: ~w ~n",[{?MODULE, self()},Data])).
%% List,Current,Header,Export,Functions
generate(List) when is_list(List) -> generate(List,?DEFNAME).
generate(List,Fname) when is_list(List)-> generate(List,Fname,[],[]).
generate([{_,[{type,start},{next,Next}]}|Rest],Header,[],[])->generate(Rest,Header,["init/1"],[lists:flatten(["\ninit(ParamList) -> ",atom_to_list(Next),"(ParamList).\n\n"])]);

generate([{Name,[{type,calc}|Props]}|Rest],Fname,Export,Functions)->
  {inputs,Inputs}=proplists:lookup(inputs,Props),
  {expressions,Expressions}=proplists:lookup(expressions,Props),
  {outputs,Outputs}=proplists:lookup(outputs,Props),
  {next,Next}=proplists:lookup(next,Props),
  NewFunc = lists:flatten([atom_to_list(Name),"(ParamList)->\n",get_inputs(Inputs),
    [lists:flatten(["  ",Exp,",\n"]) ||Exp<-string:tokens(Expressions,",;")],
   "  NewPlist=[",get_outputs(Outputs),"],\n"
    ,atom_to_list(Next),"(lists:append(ParamList,NewPlist)).\n\n"]),
  generate(Rest,Fname,[lists:flatten([atom_to_list(Name),"/1"])|Export],[NewFunc|Functions]);
generate([{Name,[{type,ets_data}|Props]}|Rest],Fname,Export,Functions)->
  {inputs,Inputs}=proplists:lookup(inputs,Props),
  {etsname,Etsname}=proplists:lookup(etsname,Props),
  {outputs,Outputs}=proplists:lookup(outputs,Props),
  {next,Next}=proplists:lookup(next,Props),
  NewFunc = lists:flatten([atom_to_list(Name), "(ParamList)->\n",
    get_inputs(Inputs),
    "[{_,LookupRet}]=ets:lookup(",
    Etsname,
    ",{",
    get_FirstUpList(Inputs),
    "}),\n{",
    get_FirstUpList(Outputs),
    "}=LookupRet,\nNewPlist=[",
    get_outputs(Outputs),
    "],\n",
    atom_to_list(Next),
    "(lists:append(ParamList,NewPlist)).\n\n"]),
  generate(Rest,Fname,[lists:flatten([atom_to_list(Name),"/1"])|Export],[NewFunc|Functions]);
generate([{Name,[{type,ets_data}|Props]}|Rest],Fname,Export,Functions)->
  {inputs,Inputs}=proplists:lookup(inputs,Props),
  {etsname,Etsname}=proplists:lookup(etsname,Props),
  {outputs,Outputs}=proplists:lookup(outputs,Props),
  {next,Next}=proplists:lookup(next,Props),
  NewFunc = lists:flatten([atom_to_list(Name),"(ParamList)->\n",get_inputs(Inputs),
    "[{_,LookupRet}]=ets:lookup(",Etsname,",{",get_FirstUpList(Inputs),"})\n",
    "{",get_FirstUpList(Outputs),"}=LookupRet,\n"
    "NewPlist=[",get_outputs(Outputs),"],\n",atom_to_list(Next),"(lists:append(ParamList,NewPlist)).\n\n" ]),
  generate(Rest,Fname,[lists:flatten([atom_to_list(Name),"/1"])|Export],[NewFunc|Functions]);
generate([{Name,[{type,cases}|Props]}|Rest],Fname,Export,Functions)->
  {inputs,Inputs}=proplists:lookup(inputs,Props),
  {condition,Condition}=proplists:lookup(condition,Props),
  Caselist = string:join([lists:flatten(["\n   ",CaseEl," -> ",atom_to_list(NextFunc),"(ParamList)"])||{CaseEl,NextFunc}<-proplists:delete(inputs, proplists:delete(condition,Props))],";"),
  NewFunc = lists:flatten([atom_to_list(Name), "(ParamList)->\n",get_inputs(Inputs),"case ",Condition," of  ",Caselist,"\nend.\n\n" ]),
  generate(Rest,Fname,[lists:flatten([atom_to_list(Name),"/1"])|Export],[NewFunc|Functions]);
generate([{_,[{type,stop}|_]}|_Rest],Fname,Export,Functions)->write_generated(Fname,Export,["stop(ParamList)->ParamList.\n\n"|Functions]);
generate(A,B,C,D) when (not is_list(A)) and (not is_list(B)) and (not is_list(C)) and (not is_list(D))->{error,badalg}.

write_generated(Fname,Export,Functions)->
  ListCode = lists:flatten([lists:flatten([" %",Fname,"% \n-module(",Fname,").\n"]),"-export([",string:join(lists:reverse(Export),","),"]).\n",lists:reverse(Functions)]),
  ?SAYVAL(ListCode),
  Code = list_to_binary(ListCode),
  ?SAYVAL(Code),
  FullName = lists:flatten([Fname,".erl"]),
  file:write_file(FullName,Code),
  compile:file(FullName),
%%   dbg:tracer(),
%%   dbg:tpl(Fname,[]),
%%   dbg:p(all,c),
  ok.



drop_elements(List,Droplist)->lists:filter(fun(Elem)->not lists:member(Elem,Droplist)end,List).

get_inputs(Inputs)-> [lists:flatten(["{_,",Par,"}=proplists:lookup(",string:to_lower( hd(Par)),tl(Par),",ParamList),\n"])||Par<-string:tokens(Inputs,",")].

get_FirstUpList(Inputs)-> string:join([lists:flatten([string:to_upper( hd(Par)),tl(Par)])||Par<-string:tokens(Inputs,",")],",").
get_FirstLowList(Inputs)-> string:join([lists:flatten([string:to_lower( hd(Par)),tl(Par)])||Par<-string:tokens(Inputs,",")],",").

get_outputs(Outputs)->string:join([  lists:flatten(["{",string:to_lower( hd(Par)),tl(Par),",",Par,"}"]) ||Par<-string:tokens(Outputs,",")],",").
