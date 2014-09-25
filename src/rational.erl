%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@bestmx.ru>
%%% @doc
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(rational).

%% Types
-export_type([rational/0]).

%% API
-export([new/1, new/2]).

%% API
-export([eq/2, ge/2, gt/2, ne/2, le/2, lt/2]).

%% API
-export([diff/2, inv/1, neg/1, prod/2, quot/2, sum/2]).

%% API
-export([format/1, parse/1]).

%% API
-export([from_float/1, to_float/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type rational() :: {rational, integer(), integer()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(integer()) -> rational().

new(Num) when is_integer(Num) ->
    {rational, Num, 1};

new(_Num) ->
    error(badarg).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(integer(), integer()) -> rational().

new(Num, Denom)
  when is_integer(Num),
       is_integer(Denom), Denom =/= 0 ->
    normalize(reduce({rational, Num, Denom}));

new(_Num, _Denom) ->
    error(badarg).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec eq(integer() | rational(),
         integer() | rational()) -> boolean().

eq(Q, Z) when is_integer(Z) ->
    eq(Q, new(Z));

eq(Z, Q) when is_integer(Z) ->
    eq(new(Z), Q);

eq({rational, A, B}, {rational, C, D})
  when is_integer(A), is_integer(B),
       is_integer(C), is_integer(D) ->
    (A * D) =:= (B * C);

eq(_Q1, _Q2) ->
    error(badarith).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec ge(integer() | rational(),
         integer() | rational()) -> boolean().

ge(Q1, Q2) ->
    not(lt(Q1, Q2)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec gt(integer() | rational(),
         integer() | rational()) -> boolean().

gt(Q1, Q2) ->
    lt(Q2, Q1).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec ne(integer() | rational(),
         integer() | rational()) -> boolean().

ne(Q1, Q2) ->
    not(eq(Q1, Q2)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec le(integer() | rational(),
         integer() | rational()) -> boolean().

le(Q1, Q2) ->
    not(lt(Q2, Q1)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec lt(integer() | rational(),
         integer() | rational()) -> boolean().

lt(Z, Q) when is_integer(Z) ->
    lt(new(Z), Q);

lt(Q, Z) when is_integer(Z) ->
    lt(Q, new(Z));

lt(Q1 = {rational, _A, B}, Q2) when B < 0 ->
    lt(normalize(Q1), Q2);

lt(Q1, Q2 = {rational, _C, D}) when D < 0 ->
    lt(Q1, normalize(Q2));

lt({rational, A, B}, {rational, C, D})
  when is_integer(A), is_integer(B),
       is_integer(C), is_integer(D) ->
    (A * D) < (B * C);

lt(_Q1, _Q2) ->
    error(badarith).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec diff(integer() | rational(),
           integer() | rational()) -> rational().

diff(Q1, Q2) ->
    sum(Q1, neg(Q2)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec inv(integer() | rational()) -> rational().

inv(Z) when is_integer(Z), Z =/= 0 ->
    inv(new(Z));

inv({rational, A, B}) ->
    {rational, B, A};

inv(_Q) ->
    error(badarith).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec neg(integer() | rational()) -> rational().

neg(Z) when is_integer(Z) ->
    neg(new(Z));

neg({rational, A, B}) ->
    {rational, -A, B}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prod(integer() | rational(),
           integer() | rational()) -> rational().

prod(Q, Z) when is_integer(Z) ->
    prod(Q, new(Z));

prod(Z, Q) when is_integer(Z) ->
    prod(new(Z), Q);

prod({rational, A, B}, {rational, C, D})
  when is_integer(A), is_integer(B),
       is_integer(C), is_integer(D) ->
    reduce(normalize({rational, (A * C), (B * D)}));

prod(_Q1, _Q2) ->
    error(badarith).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec quot(integer() | rational(),
           integer() | rational()) -> rational().

quot(Q1, Q2) ->
    prod(Q1, inv(Q2)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec sum(integer() | rational(),
          integer() | rational()) -> rational().

sum(Q, Z) when is_integer(Z) ->
    sum(Q, new(Z));

sum(Z, Q) when is_integer(Z) ->
    sum(new(Z), Q);

sum({rational, A, B}, {rational, C, D})
  when is_integer(A), is_integer(B),
       is_integer(C), is_integer(D) ->
    reduce(normalize({rational, (A * D) + (C * B), (B * D)}));

sum(_Q1, _Q2) ->
    error(badarith).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format(integer() | rational()) -> binary().

format(Z) when is_integer(Z) ->
    integer_to_binary(Z);

format(Q) ->
    case normalize(reduce(Q)) of
        {rational, A, 1} ->
            integer_to_binary(A);
        {rational, A, B} when B =/= 1 ->
            <<(integer_to_binary(A))/bytes, $/,
              (integer_to_binary(B))/bytes>>
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse(binary()) -> {ok, rational()} | {error, badarg}.

parse(Bytes) ->
    parse_integer(
      Bytes,
      fun(<<>>, Num) ->
              {ok, new(Num)};
         (<<$/, Bytes1/bytes>>, Num) ->
              parse_integer(
                Bytes1,
                fun(<<>>, Denom) when Denom =/= 0 ->
                        {ok, new(Num, Denom)};
                   (_Bytes2, _Denom) ->
                        {error, badarg}
                end
               );
         (_Bytes1, _Num) ->
              {error, badarg}
      end
     ).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_float(float()) -> rational().

from_float(X) ->
    from_float(X, 21).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_float(rational()) -> float().

to_float({rational, A, B})
  when is_integer(A), is_integer(B) ->
    A / B;

to_float(_Q) ->
    error(badarg).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec gcd(integer(), integer()) -> integer().

gcd(A, 0) -> A;

gcd(A, B) -> gcd(B, A rem B).

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_float(float(), non_neg_integer()) -> rational().

from_float(X, 0) ->
    new(trunc(X));

from_float(X, K) ->
    N = trunc(X),
    case X - N of
        F when F =:= 0; F =:= 0.0 -> new(N);
        F -> sum(new(N), inv(from_float(1 / F, K - 1)))
    end.

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize(rational()) -> rational().

normalize({rational, A, B}) when B < 0 ->
    {rational, -A, -B};

normalize(Q) ->
    Q.

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse_integer(binary(), fun()) -> {error, badarg}.

parse_integer(<<$-, C, Other/bytes>>, Fun)
  when C >= $0, C =< $9 ->
    parse_positive(Other, C - $0, fun(Bytes1, Pos) -> Fun(Bytes1, -Pos) end);

parse_integer(<<$+, C, Other/bytes>>, Fun)
  when C >= $0, C =< $9 ->
    parse_positive(Other, C - $0, Fun);

parse_integer(<<C, Other/bytes>>, Fun)
  when C >= $0, C =< $9 ->
    parse_positive(Other, C - $0, Fun);

parse_integer(_Bytes, _Fun) ->
    {error, badarg}.

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse_positive(binary(), non_neg_integer(), fun()) -> no_return().

parse_positive(<<C, Other/bytes>>, Ans, Fun)
  when C >= $0, C =< $9 ->
    parse_positive(Other, (Ans * 10) + (C - $0), Fun);

parse_positive(Bytes, Ans, Fun) ->
    Fun(Bytes, Ans).

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reduce(rational()) -> rational().

reduce({rational, A, B}) ->
    K = gcd(A, B),
    {rational, A div K, B div K}.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

new_1_test_() ->
    [ ?_assertEqual({rational, 33, 1}, new(33)),
      ?_assertEqual({rational, 1, 1}, new(1)),
      ?_assertEqual({rational, 0, 1}, new(0)),
      ?_assertEqual({rational, -1, 1}, new(-1)),
      ?_assertEqual({rational, -33, 1}, new(-33)),
      ?_assertError(badarg, new([])),
      ?_assertError(badarg, new(<<"a">>)),
      ?_assertError(badarg, new(1.234)),
      ?_assertError(badarg, new(self())),
      ?_assertError(badarg, new(ok)),
      ?_assertError(badarg, new(make_ref())) ].

new_2_test_() ->
    [ ?_assertEqual({rational, 1, 2}, new(1, 2)),
      ?_assertEqual({rational, 1, 2}, new(2, 4)),
      ?_assertEqual({rational, 0, 1}, new(0, 2)),
      ?_assertEqual({rational, 0, 1}, new(0, -2)),
      ?_assertEqual({rational, -1, 2}, new(-1, 2)),
      ?_assertEqual({rational, -1, 2}, new(1, -2)),
      ?_assertEqual({rational, 1, 2}, new(-1, -2)),
      ?_assertError(badarg, new(1.23, 4.56)),
      ?_assertError(badarg, new(ok, 2)),
      ?_assertError(badarg, new(1, ok)),
      ?_assertError(badarg, new(2, 0)) ].

eq_2_test_() ->
    [ ?_assertError(badarith, eq(1, 2.3)),
      ?_assertError(badarith, eq(1.2, 3)),
      ?_assert(eq(2, 2)),
      ?_assert(eq(new(2), 2)),
      ?_assert(eq(2, new(2))),
      ?_assertNot(eq(2, 3)),
      ?_assert(eq(new(2), new(2))),
      ?_assertNot(eq(new(2), new(-2))),
      ?_assert(eq(new(1, 2), new(2, 4))) ].

inv_1_test_() ->
    [ ?_assertError(badarith, inv(0)),
      ?_assertEqual({rational, 1, 2}, inv(2)),
      ?_assertEqual({rational, 1, 2}, inv(new(2))),
      ?_assertEqual({rational, 1, 2}, inv(new(2, 1))),
      ?_assertEqual({rational, 2, -1}, inv(new(-1, 2))),
      ?_assertEqual({rational, 2, -1}, inv(new(1, -2))) ].

format_1_test_() ->
    [ ?_assertEqual(<<"1">>, format(1)),
      ?_assertEqual(<<"-1">>, format(-1)),
      ?_assertEqual(<<"1/2">>, format(new(1, 2))),
      ?_assertEqual(<<"-1/2">>, format(new(-1, 2))) ].

parse_1_test_() ->
    [ ?_assertEqual({ok, {rational, 1, 2}}, parse(<<"1/2">>)),
      ?_assertEqual({ok, {rational, 0, 1}}, parse(<<"0">>)),
      ?_assertEqual({ok, {rational, -1, 2}}, parse(<<"-1/2">>)),
      ?_assertEqual({ok, {rational, -1, 2}}, parse(<<"1/-2">>)),
      ?_assertEqual({ok, {rational, 1, 2}}, parse(<<"-1/-2">>)),
      ?_assertEqual({ok, {rational, 1, 2}}, parse(<<"+1/+2">>)),
      ?_assertEqual({ok, {rational, 355, 113}}, parse(<<"355/113">>)),
      ?_assertEqual({ok, {rational, 355, 113}}, parse(<<"355/+113">>)),
      ?_assertEqual({ok, {rational, 355, 113}}, parse(<<"+355/113">>)),
      ?_assertEqual({error, badarg}, parse(<<>>)),
      ?_assertEqual({error, badarg}, parse(<<".23">>)),
      ?_assertEqual({error, badarg}, parse(<<"1.23">>)),
      ?_assertEqual({error, badarg}, parse(<<" 1/2 ">>)),
      ?_assertEqual({error, badarg}, parse(<<"1/2 z">>)),
      ?_assertEqual({error, badarg}, parse(<<"1/0">>)),
      ?_assertEqual({ok, {rational, 0, 1}}, parse(<<"0/1">>)),
      ?_assertEqual({ok, {rational, 0, 1}}, parse(<<"0/-1">>)),
      ?_assertEqual({ok, {rational, 0, 1}}, parse(<<"-0/1">>)) ].

to_float_1_test_() ->
    [ ?_assertEqual(2.0, to_float(new(2))),
      ?_assertEqual(3.5, to_float(new(7, 2))),
      ?_assertEqual(-3.5, to_float(new(-7, 2))),
      ?_assertEqual(3.1415929203539825, to_float(new(355, 113))) ].

from_float_1_test_() ->
    [ ?_assertEqual(new(7, 2), from_float(3.5)),
      ?_assertEqual(normalize(new(-7, 2)), normalize(from_float(-3.5))),
      ?_assertEqual(math:pi(), to_float(from_float(math:pi()))) ].

%% TODO

-endif.
