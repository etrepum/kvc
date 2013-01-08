%% @copyright 2011 Bob Ippolito
%% @author Bob Ippolito <bob@redivi.com>

%% @doc Property based tests for KVC.
-module(kvc_prop_tests).

-type strict_key() :: kvc:kvc_key().
-type strict_proplist() :: [{strict_key(), strict_value()}].
-type strict_value() :: strict_key() | strict_proplist() | [strict_value()].
-type container_type() :: gb_trees | dict | struct | proplist | eep0018.

-ifdef(USE_PROPER).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([check_container/1, check_container/2]).

proper_test_() ->
    [{atom_to_list(F),
      fun () -> ?assert(proper:quickcheck(?MODULE:F(), [long_result])) end}
     || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`'].

prop_to_proplist_identity() ->
    ?FORALL(P, union([strict_proplist(), list(integer()),
                      integer(), tuple([integer(), integer()])]),
            kvc:to_proplist(P) =:= P).


-spec make_container(container_type(), strict_proplist()) -> kvc:kvc_obj_node().
make_container(proplist, P) ->
    P;
make_container(struct, P) ->
    {struct, P};
make_container(eep0018, P) ->
    {P};
make_container(eep0018_empty, []) ->
    {};
make_container(eep0018_empty, P) ->
    {P};
make_container(dict, P) ->
    dict:from_list(P);
make_container(gb_trees, P) ->
    gb_trees:from_orddict(orddict:from_list(P)).

key() ->
    union([1.0, 2.0, 3.0, 4.0, 5.0]).

value(S) ->
    ?LAZY(union([key(), container(S), resize(S, list(value(S div 2)))])).

pair(S) ->
    {key(), value(S)}.

container(S) ->
    ?LET({Type, Pairs},
         {union([proplist, struct, dict, gb_trees, eep0018, eep0018_empty]),
          resize(S, list(pair(S div 2)))},
         make_container(Type, Pairs)).

prop_value_coercion() ->
    Keys = union([a, <<"a">>, "a"]),
    ?FORALL({K0, K1, CtrType}, {Keys, Keys, container_type()},
            bar =:= kvc:value(K0, make_container(CtrType, [{K1, bar}]), [])).

prop_to_proplist_shallow() ->
    ?FORALL({P, CtrType}, {strict_proplist(), container_type()},
            begin
                P1 = orddict:from_list(P),
                Ctr = make_container(CtrType, P1),
                P1 =:= orddict:from_list(kvc:to_proplist(Ctr))
            end).

check_container(Container) ->
    check_container(
      uniq(kvc:to_proplist(Container)),
      Container).

uniq(L) ->
    orddict:from_list(lists:reverse(L)).

check_container([{K, V} | T], Container) when is_float(V) ->
    kvc:value(K, Container, undefined) =:= V
        andalso check_container(T, Container);
check_container([{K, V=[{K1, _} | _]} | T], Container) when is_float(K1) ->
    check_container(uniq(V), kvc:value(K, Container, []))
        andalso check_container(T, Container);
check_container([{K, []} | T], Container) ->
    [] =:= kvc:to_proplist(kvc:value(K, Container, undefined))
        andalso check_container(T, Container);
check_container([{K, V} | T], Container) when is_list(V) ->
    check_list(V, kvc:value(K, Container, undefined))
        andalso check_container(T, Container);
check_container([], _Container) ->
    true.

check_list([H | T0], [H | T1]) ->
    check_list(T0, T1);
check_list([L=[{_, _}|_] | T0], [C | T1]) ->
    check_container(uniq(L), C) andalso check_list(T0, T1);
check_list([[] | T0], [C | T1]) ->
    [] =:= kvc:to_proplist(C) andalso check_list(T0, T1);
check_list([L | T0], [C | T1]) when is_list(L) ->
    check_list(L, C) andalso check_list(T0, T1);
check_list([], []) ->
    true;
check_list(_, _) ->
    false.

prop_to_proplist_deep() ->
    ?FORALL(Container, container(8),
            check_container(Container)).
-endif.
