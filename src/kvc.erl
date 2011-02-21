%% @copyright 2011 Bob Ippolito
%% @author Bob Ippolito <bob@redivi.com>

%% @doc Implementation of Key Value Coding style "queries" for commonly
%% used Erlang data structures.
-module(kvc).
-export([path/2, value/3]).
-compile([export_all]).

%% @type kvc_key() = binary() | atom() | string().
%% @type kvc_obj_node() = proplist() | {struct, proplist()} | dict() | gb_tree().
%% @type kvc_obj() = kvc_obj_node() | [kvc_obj_node()] | [].

%% @spec path(kvc_key() | [kvc_key()], kvc_obj()) -> term() | []
path(B, P) when is_binary(B) ->
    path(binary:split(B, <<".">>, [global]), P);
path(A, P) when is_atom(A) ->
    path(atom_to_binary(A, utf8), P);
path(L=[N | _], P) when is_integer(N) ->
    path(iolist_to_binary(L), P);
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
        {{gb_trees, Tree}, Type} ->
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
            end;
        undefined ->
            Default
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

proplist_type(P=[{K, _} | _]) ->
    {P, typeof_elem(K)};
proplist_type({struct, P=[{K, _} | _]}) ->
    {P, typeof_elem(K)};
proplist_type(L) when is_list(L) ->
    {L, list};
proplist_type(D) when element(1, D) =:= dict ->
    proplist_type(dict:to_list(D));
proplist_type(T={N, {_, _, _, _}}) when is_integer(N) andalso N > 0 ->
    {K, _V} = gb_trees:smallest(T),
    {{gb_trees, T}, typeof_elem(K)};
proplist_type(_) ->
    undefined.

typeof_elem(A) when is_atom(A) ->
    atom;
typeof_elem(B) when is_binary(B) ->
    binary;
typeof_elem([N | _]) when is_integer(N) andalso N > 0 ->
    string;
typeof_elem(_) ->
    undefined.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
path_aggregate_test() ->
    ?assertEqual(
       [taco, taco, grande],
       kvc:path(foo, [{foo, [taco, taco, grande]}])),
    ?assertEqual(
       3,
       kvc:path(<<"foo.@count">>, [{foo, [taco, taco, grande]}])),
    ?assertEqual(
       6,
       kvc:path('foo.@sum', [{foo, [1, 2, 3]}])),
    ?assertEqual(
       2.0,
       kvc:path("foo.@avg", [{foo, [1, 2, 3]}])),
    ?assertEqual(
       1,
       kvc:path([foo, '@min'], [{foo, [1, 2, 3]}])),
    ?assertEqual(
       3,
       kvc:path("foo.@max", [{foo, [1, 2, 3]}])),
    ?assertEqual(
       [taco, taco, grande],
       kvc:path(<<"foo.@unionOfObjects">>,
                [{foo, [taco, taco, grande]}])),
    ?assertEqual(
       [taco, taco, grande],
       kvc:path(<<"foo.@unionOfArrays">>,
                [{foo, [[taco], [taco], [grande]]}])),
    ?assertEqual(
       lists:sort([taco, grande]),
       lists:sort(kvc:path(<<"foo.@distinctUnionOfObjects">>,
                           [{foo, [taco, taco, grande]}]))),
    ?assertEqual(
       lists:sort([taco, grande]),
       lists:sort(kvc:path(<<"foo.@distinctUnionOfArrays">>,
                           [{foo, [[taco], [taco], [grande]]}]))),
    ok.

pairwise_combinations(L) ->
    pairwise_combinations(L, []).

pairwise_combinations([], Acc) ->
    lists:append(lists:reverse(Acc));
pairwise_combinations(L=[H | T], Acc) ->
    pairwise_combinations(T, [[{H, E} || E <- L] | Acc]).

value_coercion_test() ->
    lists:foreach(fun ({K0, K1}) ->
                          ?assertEqual(bar, kvc:value(K0, [{K1, bar}], []))
                  end,
                  pairwise_combinations([a, <<"a">>, "a"])).

value_aggregate_test() ->
    ?assertEqual(
       6,
       kvc:value('@sum', [1, 2, 3], [])),
    ?assertEqual(
       6,
       kvc:value("@sum", [1, 2, 3], [])),
    ?assertEqual(
       6,
       kvc:value(<<"@sum">>, [1, 2, 3], [])),
    ?assertEqual(
       2.0,
       kvc:value(<<"@avg">>, [1, 2, 3], [])),
    ?assertEqual(
       [],
       kvc:value(<<"@avg">>, [], [])),
    ok.

path_edge_test() ->
    ?assertEqual(
       [bar],
       kvc:path(foo, [[{foo, bar}], [{bar, baz}]])),
    ?assertEqual(
       [bar],
       kvc:path(foo, [[{foo, bar}], [{bar, baz}]])),
    ok.

value_edge_test() ->
    ?assertEqual(
       [],
       kvc:value(foo, [{1, 2}], [])),
    ?assertEqual(
       [],
       kvc:value(<<255>>, [{foo, ok}], [])),
    ?assertEqual(
       [],
       kvc:value([256], [{foo, ok}], [])),
    ok.

path_plist_test() ->
    lists:foreach(
      fun (F) ->
              ?assertEqual(
                 baz,
                 kvc:path(foo.bar, F([{foo, [{bar, baz}]}]))),
              ?assertEqual(
                 [],
                 kvc:path(foo.bar, F([{foo, [{baz, baz}]}]))),
              ?assertEqual(
                 [],
                 kvc:path(foo.bar, F([{not_foo, ok}]))),
              ?assertEqual(
                 [],
                 kvc:path(foo.bar, F([])))
      end,
      [fun gb_trees:from_orddict/1, fun dict:from_list/1]),
    ?assertEqual(
       wibble,
       kvc:path(foo.bar.baz, [{foo, [{bar, [{baz, wibble}]}]}])),
    ?assertEqual(
       [],
       kvc:path(foo.bar.baz.invalid_proplist, [{foo, [{bar, [{baz, wibble}]}]}])),
    ?assertEqual(
       [],
       kvc:path(foo.bar.baz, [{foo, [{bar, [{bar, wibble}]}]}])),
    ?assertEqual(
       <<"wibble">>,
       kvc:path(foo.bar.baz,
                {struct,
                 [{<<"foo">>,
                   {struct,
                    [{<<"bar">>,
                      {struct, [{<<"baz">>, <<"wibble">>}]}}]}}]})),
    ?assertEqual(
       "wibble",
       kvc:path(foo.bar.baz,
                {struct,
                 [{"foo",
                   {struct,
                    [{"bar",
                      {struct, [{"baz", "wibble"}]}}]}}]})),
    ?assertEqual(
       ok,
       kvc:value("foo", [{foo, ok}], [])),
    ?assertEqual(
       ok,
       kvc:value("foo", [{<<"foo">>, ok}], [])),
    ok.

-endif.
