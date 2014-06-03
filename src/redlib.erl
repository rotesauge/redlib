-module(redlib).
-compile(export_all).
%===============================================================================================================================												
%===============================================================================================================================												
split_list_for(List,N)->split10(List,[],N).
split_list_for([],Lists,N)->Lists;
split_list_for(List,Lists,N) when erlang:length(List)  < N -> [List|Lists];
split_list_for(List,Lists,N) when erlang:length(List) == N -> [List|Lists];
split_list_for(List,Lists,N) when erlang:length(List)  > N -> {H,T} = lists:split(N,List),
                                                   split_list_for(T,[H|Lists],N).
%===============================================================================================================================												
list_element(List,Elem)->element(Elem,erlang:list_to_tuple(List)).												
%===============================================================================================================================												
list_element2(List,Elem)->erlang:hd(lists:sublist(List, Elem, 1).												 