%% @copyright 2011 Bob Ippolito
%% @author Bob Ippolito <bob@redivi.com>

%% @doc Implementation of Key Value Coding style "queries" for commonly
%% used Erlang data structures.
-module(kvc).
-export([path/2, value/3]).

%% @type kvc_key() = binary() | atom() | string().
%% @type kvc_obj_node() = proplist() | {struct, proplist()} | dict() | gb_tree() | term().
%% @type kvc_obj() = kvc_obj_node() | [kvc_obj_node()] | [].
%% @type elem_type() = atom | binary | string | undefined.
-type elem_type() :: atom | binary | string | list | undefined.
-type kvc_obj() :: kvc_obj_node() | [kvc_obj_node()] | list().
-type kvc_key() :: binary() | atom() | string().
-type proplist() :: [{kvc_key(), kvc_obj()}].
-type kvc_obj_node() :: proplist() | {struct, proplist()} | dict() | gb_tree() | term().
-type typed_proplist() :: {proplist() | {gb_tree, gb_tree()}, elem_type()}.

%% @spec path(kvc_key() | [kvc_key()], kvc_obj()) -> term() | []
%% @doc Return the result of the query Path on P.
path(Path, P) when is_binary(Path) ->
    path(binary:split(Path, <<".">>, [global]), P);
path(Path, P) when is_atom(Path) ->
    path(atom_to_binary(Path, utf8), P);
path(Path=[N | _], P) when is_integer(N) ->
    path(iolist_to_binary(Path), P);
path([], P) ->
    P;
path([K | Rest], P) ->
    path(Rest, value(K, P, [])).

%% @spec value(kvc_key(), kvc_obj(), term()) -> term()
%% @doc Return the immediate result of the query for key K in P.
value(K, P, Default) ->
    case proplist_type(P) of
        {Nested, list} ->
            R = make_ref(),
            case get_nested_values(K, Nested, R) of
                R ->
                    Default;
                V ->
                    V
            end;
        {{gb_tree, Tree}, Type} ->
            case gb_trees:lookup(normalize(K, Type), Tree) of
                none ->
                    Default;
                {value, V} ->
                    V
            end;
        {Proplist, Type} ->
            case lists:keyfind(normalize(K, Type), 1, Proplist) of
                false ->
                    Default;
                {_, V} ->
                    V
            end
    end.

get_nested_values(<<"@max">>, L, _R) ->
    lists:max(L);
get_nested_values(<<"@min">>, L, _R) ->
    lists:min(L);
get_nested_values(<<"@sum">>, L, _R) ->
    lists:sum(L);
get_nested_values(<<"@count">>, L, _R) ->
    length(L);
get_nested_values(<<"@avg">>, [], R) ->
    R;
get_nested_values(<<"@avg">>, L, _R) ->
    {Count, Sum} = lists:foldl(
                     fun (N, {C, S}) -> {1 + C, N + S} end,
                     {0, 0},
                     L),
    Sum / Count;
get_nested_values(<<"@distinctUnionOfArrays">>, L, _R) ->
    lists:usort(lists:append(L));
get_nested_values(<<"@distinctUnionOfObjects">>, L, _R) ->
    lists:usort(L);
get_nested_values(<<"@unionOfArrays">>, L, _R) ->
    lists:append(L);
get_nested_values(<<"@unionOfObjects">>, L, _R) ->
    L;
get_nested_values(A, L, R) when is_atom(A) andalso A > '@' andalso A < 'A' ->
    get_nested_values(atom_to_binary(A, utf8), L, R);
get_nested_values(K="@" ++ _, L, R) ->
    get_nested_values(iolist_to_binary(K), L, R);
get_nested_values(K, [L | Rest], R) ->
    case value(K, L, R) of
        R ->
            get_nested_values(K, Rest, R);
        V ->
            [V | get_nested_values(K, Rest, R)]
    end;
get_nested_values(_K, [], _R) ->
    [].

-spec proplist_type(term()) -> typed_proplist().
proplist_type(P=[{K, _} | _]) ->
    {P, typeof_elem(K)};
proplist_type({struct, P=[{K, _} | _]}) ->
    {P, typeof_elem(K)};
proplist_type(L) when is_list(L) ->
    {L, list};
proplist_type(D) ->
    first_of([fun () ->
                      proplist_type(dict:to_list(D))
              end,
              fun () ->
                      {K, _V} = gb_trees:smallest(D),
                      {{gb_tree, D}, typeof_elem(K)}
              end,
              fun () ->
                      {[], undefined}
              end]).

first_of([F | Rest]) ->
    try F()
    catch error:_ ->
            first_of(Rest)
    end.


%% @spec typeof_elem(term()) -> typeof_elem()
typeof_elem(A) when is_atom(A) ->
    atom;
typeof_elem(B) when is_binary(B) ->
    binary;
typeof_elem([N | _]) when is_integer(N) andalso N > 0 ->
    string;
typeof_elem(_) ->
    undefined.

%% @spec normalize(term(), elem_type()) -> term()
normalize(K, atom) when is_atom(K) ->
    K;
normalize(K, atom) when is_binary(K) ->
    try binary_to_existing_atom(K, utf8)
    catch error:badarg ->
            K
    end;
normalize(K, atom) when is_list(K) ->
    try list_to_existing_atom(K)
    catch error:badarg ->
            K
    end;
normalize(K, binary) when is_binary(K) ->
    K;
normalize(K, binary) when is_atom(K) ->
    atom_to_binary(K, utf8);
normalize(K, binary) when is_list(K) ->
    iolist_to_binary(K);
normalize(K, string) when is_list(K) ->
    K;
normalize(K, string) when is_binary(K) ->
    binary_to_list(K);
normalize(K, string) when is_atom(K) ->
    atom_to_list(K);
normalize(K, undefined) ->
    K.
