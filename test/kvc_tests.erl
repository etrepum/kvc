%% @copyright 2011 Bob Ippolito
%% @author Bob Ippolito <bob@redivi.com>

%% @doc Tests for KVC.
-module(kvc_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type strict_key() :: kvc:kvc_key().
-type strict_proplist() :: [{strict_key(), strict_value()}].
-type strict_value() :: strict_key() | strict_proplist() | [strict_value()].
-type container_type() :: gb_trees | dict | struct | proplist.

proper_test_() ->
    [{atom_to_list(F),
      fun () -> ?assert(proper:quickcheck(?MODULE:F(), [long_result])) end}
     || {F, 0} <- ?MODULE:module_info(exports), F > 'prop_', F < 'prop`'].

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

to_proplist_readme_test() ->
    ?assertEqual(
       [{<<"foo">>, [{<<"bar">>, <<"baz">>}]}],
       kvc:to_proplist({struct,
                        [{<<"foo">>,
                          {struct,
                           [{<<"bar">>, <<"baz">>}]}}]})).

prop_to_proplist_identity() ->
    ?FORALL(P, union([strict_proplist(), list(integer()),
                      integer(), tuple([integer(), integer()])]),
            kvc:to_proplist(P) =:= P).


-spec make_container(container_type(), strict_proplist()) -> kvc:kvc_obj_node().
make_container(proplist, P) ->
    P;
make_container(struct, P) ->
    {struct, P};
make_container(dict, P) ->
    dict:from_list(P);
make_container(gb_trees, P) ->
    gb_trees:from_orddict(orddict:from_list(P)).

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
