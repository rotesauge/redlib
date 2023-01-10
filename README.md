redlib
======
redlib contains:

  split_list_for(List,N) -> [[any()]] when is_subtype(List,[any()]), is_subtype(N,integer()).

  list_element(List,Elem) -> term() when is_subtype(List,[any()]), is_subtype(Elem,integer()).

  sort_list_of_tuple(List) -> [tuple()] when is_subtype(List,[tuple()]).

  deduplicate(List) -> [any()] when is_subtype(List,[any()]).

  list2binary_ex([any()]) -> binary().

  is_simple_list(maybe_improper_list()) -> boolean().

  is_string(maybe_improper_list()) -> boolean().

  data_to_html(Data) -> binary() when is_subtype(Data,term()).

 flatten([any()]) -> list().
 
 parser__________________________________________________________
 
 parse(Str :: string()) -> term() | {error, Reason :: term()}).
 
 alg_generator____________________________________________________
 
 generate(list()) -> ok| {error, Reason :: term()}).  generates
