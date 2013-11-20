KVC - Key Value Coding for Erlang data structures
=================================================

<bob@redivi.com>

Overview:
---------

kvc supports Key Value Coding-like queries on common Erlang data structures.
A common use case for kvc is to quickly access one or more deep values in
decoded JSON, or some other nested data structure. It can also help with some
aggregate operations. It solves similar problems that you might want to
use a tool like XPath or jQuery for, but it is far simpler and strictly less
powerful. It's inspired by Apple's NSKeyValueCoding protocol from Objective-C.

The following common Erlang data structures are supported:

* `list()`
* `dict()`
* `gb_trees()`
* `proplist()`
* `{struct, proplist()}` (commonly used in mochijson2)
* `{proplist()}` ([EEP 18](http://www.erlang.org/eeps/eep-0018.html))

Only the following data types are permitted for keys, and they must be UTF-8
if any type coercion takes place:

* `atom()`
* `binary()`
* `string()`

Another limitation is that it is assumed that the given data structure has a
homogeneous key type. For example, if any key is `binary()`, all keys should
be `binary()`.

Collection Operators
====================

The following collection operators are supported. Note that the
numerics have straightforward implementations and no special care is
taken reduce floating point error.

Their native representation is binary. Atom and string are also
supported but require an additional type coercion for the look-up.

* `<<"@sum">>`
* `<<"@min">>`
* `<<"@max">>`
* `<<"@count">>`
* `<<"@avg">>`
* `<<"@distinctUnionOfArrays">>`
* `<<"@distinctUnionOfObjects">>`
* `<<"@unionOfArrays">>`
* `<<"@unionOfObjects">>`

Status:
-------

Not used in production, but it has a test suite that passes.

If you decide to use this in your production app, you should use lists
for paths and try to use the same type as the keys in your data
structure.

If you'd like to contribute to kvc, a good implementation for setters
is the biggest missing piece.

Usage:
------

Two styles of queries are supported, the more performant native
interface uses a list of keys for the path. If a string, binary, or
atom are given then it will be split on '.' peroids to form this key list.

Simple `proplist()` example:

    %% Native key list of atoms that match the data type (fastest)
    wibble =:= kvc:path([foo, bar, baz], [{foo, [{bar, [{baz, wibble}]}]}]).

    %% Native key list of binaries, does not match key data type (slower)
    wibble =:= kvc:path([<<"foo">>, <<"bar">>, <<"baz">>],
                        [{foo, [{bar, [{baz, wibble}]}]}]).

    %% These bare keys must be parsed first (slowest)
    wibble =:= kvc:path('foo.bar.baz', [{foo, [{bar, [{baz, wibble}]}]}]).
    wibble =:= kvc:path(<<"foo.bar.baz">>, [{foo, [{bar, [{baz, wibble}]}]}]).
    wibble =:= kvc:path("foo.bar.baz", [{foo, [{bar, [{baz, wibble}]}]}]).


mochijson2 `{struct, proplist()}` example:

    <<"wibble">> =:= kvc:path([<<"foo">>, <<"bar">>, <<"baz">>],
                         {struct,
                          [{<<"foo">>,
                            {struct,
                             [{<<"bar">>,
                               {struct, [{<<"baz">>, <<"wibble">>}]}}]}}]}).

    <<"wibble">> =:= kvc:path('foo.bar.baz',
                         {struct,
                          [{<<"foo">>,
                            {struct,
                             [{<<"bar">>,
                               {struct, [{<<"baz">>, <<"wibble">>}]}}]}}]}).

Collection operator example:

    2.0 =:= kvc:path([<<"foo">>,<<"bar">>,<<"baz">>,<<"@avg">>],
                     {struct,
                      [{<<"foo">>,
                       {struct,
                        [{<<"bar">>,
                         {struct, [{<<"baz">>, [1, 2, 3]}]}}]}}]}).

    2.0 =:= kvc:path('foo.bar.baz.@avg',
                     {struct,
                      [{<<"foo">>,
                       {struct,
                        [{<<"bar">>,
                         {struct, [{<<"baz">>, [1, 2, 3]}]}}]}}]}).

to_proplist normalization:

    [{<<"foo">>, [{<<"bar">>, <<"baz">>}]}] =:=
        kvc:to_proplist({struct,
                         [{<<"foo">>,
                           {struct,
                            [{<<"bar">>, <<"baz">>}]}}]}).
