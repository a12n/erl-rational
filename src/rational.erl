%%%-------------------------------------------------------------------
%%% @author Anton Yabchinskiy <arn@bestmx.ru>
%%% @doc
%%% @end
%%% For copyright notice see LICENSE.
%%%-------------------------------------------------------------------
-module(rational).

%% Types
-export_type([rational/0, t/0]).

%% API
-export([denom/1, new/1, new/2, num/1, zero/0, one/0]).

%% API
-export([eq/2, ge/2, gt/2, ne/2, le/2, lt/2]).

%% API
-export([sub/2, pow/2, inv/1, neg/1, abs/1, mul/2, 'div'/2, add/2]).

%% API
-export([min/2, max/2]).

%% API
-export([format/1, parse/1]).

%% API
-export([from_float/1, to_float/1, to_integer/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-opaque rational() :: {rational, integer(), pos_integer()}.
-type t() :: rational().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns denominator `B' of the rational `Q = A/B'. Raises `badarg'
%% error on invalid input.
%% @end
%%--------------------------------------------------------------------
-spec denom(rational()) -> integer().

denom({rational, _A, B}) -> B;

denom(_Q) -> error(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Constructs new rational number `Q = A/1' out of integer `A'. Raises
%% `badarg' error on invalid input.
%% @end
%%--------------------------------------------------------------------
-spec new(integer()) -> rational().

new(Num) when is_integer(Num) -> {rational, Num, 1};

new(_Num) -> error(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Constructs new rational number `Q = A/B' out of numerator `A' and
%% non-zero denominator `B'. Raises `badarg' error on invalid input
%% (non-integer arguments, zero denominator).
%% @end
%%--------------------------------------------------------------------
-spec new(integer(), integer()) -> rational().

new(Num, Denom)
  when is_integer(Num),
       is_integer(Denom), Denom =/= 0 ->
    normalize(reduce({rational, Num, Denom}));

new(_Num, _Denom) -> error(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Returns numerator `A' of the rational `Q = A/B'. Raises `badarg' on
%% invalid input.
%% @end
%%--------------------------------------------------------------------
-spec num(integer() | rational()) -> integer().

num({rational, A, _B}) -> A;

num(Z) when is_integer(Z) -> Z;

num(_Q) -> error(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Rational number 0.
%% @end
%%--------------------------------------------------------------------
-spec zero() -> rational().

zero() -> new(0).

%%--------------------------------------------------------------------
%% @doc
%% Rational number 1.
%% @end
%%--------------------------------------------------------------------
-spec one() -> rational().

one() -> new(1).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% For rational numbers `Q1' and `Q2' returns `true' if `Q1' equals
%% `Q2'. Accepts plain integers as `Q1' and/or `Q2'. Generates
%% `badarith' error on invalid arguments.
%% @end
%%--------------------------------------------------------------------
-spec eq(integer() | rational(),
         integer() | rational()) -> boolean().

eq(Q, Z) when is_integer(Z) -> eq(Q, new(Z));

eq(Z, Q) when is_integer(Z) -> eq(new(Z), Q);

eq({rational, A, B}, {rational, C, D}) -> (A * D) =:= (B * C);

eq(_Q1, _Q2) -> error(badarith).

%%--------------------------------------------------------------------
%% @doc
%% For rational numbers `Q1' and `Q2' returns `true' if `Q1' is
%% greater than or equal to `Q2'. Accepts plain integers as `Q1'
%% and/or `Q2'. Generates `badarith' error on invalid arguments.
%% @end
%%--------------------------------------------------------------------
-spec ge(integer() | rational(),
         integer() | rational()) -> boolean().

ge(Q1, Q2) -> not lt(Q1, Q2).

%%--------------------------------------------------------------------
%% @doc
%% For rational numbers `Q1' and `Q2' returns `true' if `Q1' is
%% greater than `Q2'. Accepts plain integers as `Q1' and/or
%% `Q2'. Generates `badarith' error on invalid arguments.
%% @end
%%--------------------------------------------------------------------
-spec gt(integer() | rational(),
         integer() | rational()) -> boolean().

gt(Q1, Q2) -> lt(Q2, Q1).

%%--------------------------------------------------------------------
%% @doc
%% For rational numbers `Q1' and `Q2' returns `true' if `Q1' isn't
%% equal to `Q2'. Accepts plain integers as `Q1' and/or
%% `Q2'. Generates `badarith' error on invalid arguments.
%% @end
%%--------------------------------------------------------------------
-spec ne(integer() | rational(),
         integer() | rational()) -> boolean().

ne(Q1, Q2) -> not eq(Q1, Q2).

%%--------------------------------------------------------------------
%% @doc
%% For rational numbers `Q1' and `Q2' returns `true' if `Q1' is less
%% than or equal to `Q2'. Accepts plain integers as `Q1' and/or
%% `Q2'. Generates `badarith' error on invalid arguments.
%% @end
%%--------------------------------------------------------------------
-spec le(integer() | rational(),
         integer() | rational()) -> boolean().

le(Q1, Q2) -> not lt(Q2, Q1).

%%--------------------------------------------------------------------
%% @doc
%% For rational numbers `Q1' and `Q2' returns `true' if `Q1' is less
%% than `Q2'. Accepts plain integers as `Q1' and/or `Q2'. Generates
%% `badarith' error on invalid arguments.
%% @end
%%--------------------------------------------------------------------
-spec lt(integer() | rational(),
         integer() | rational()) -> boolean().

lt(Z, Q) when is_integer(Z) -> lt(new(Z), Q);

lt(Q, Z) when is_integer(Z) -> lt(Q, new(Z));

lt(Q1 = {rational, _A, B}, Q2) when B < 0 -> lt(normalize(Q1), Q2);

lt(Q1, Q2 = {rational, _C, D}) when D < 0 -> lt(Q1, normalize(Q2));

lt({rational, A, B}, {rational, C, D}) -> (A * D) < (B * C);

lt(_Q1, _Q2) -> error(badarith).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% For rational numbers `Q1' and `Q2' returns difference `Q1 -
%% Q2'. Accepts plain integers as `Q1' and/or `Q2'. Raises `badarith'
%% error on invalid input.
%% @equiv add(Q1, neg(Q2)).
%% @end
%%--------------------------------------------------------------------
-spec sub(integer() | rational(),
          integer() | rational()) -> rational().

sub(Q1, Q2) -> add(Q1, neg(Q2)).

%%--------------------------------------------------------------------
%% @doc
%% Exponentiation of rational number `Q' to the integer power
%% `N'. Accepts plain integer as `Q' argument. Raises `badarith' on
%% invalid input.
%% @end
%%--------------------------------------------------------------------
-spec pow(integer() | rational(), integer()) -> rational().

pow(Z, N) when is_integer(Z) -> pow(new(Z), N);

pow(Q, N) when is_integer(N), N < 0 -> pow(inv(Q), -N);

pow(_Q, 0) -> new(1);

pow(Q = {rational, _A, _B}, 1) -> Q;

pow(Q, 2) -> mul(Q, Q);

pow(Q, N) when is_integer(N), (N rem 2) =:= 0 -> pow(mul(Q, Q), N div 2);

pow(Q, N) when is_integer(N) -> mul(Q, pow(Q, N - 1));

pow(_Q, _N) -> error(badarith).

%%--------------------------------------------------------------------
%% @doc
%% For rational number `Q = a/b' returns the reciprocal `b/a'. Accepts
%% plain integer as the argument. The argument must not be zero, or
%% `badarith' error is raised.
%% @end
%%--------------------------------------------------------------------
-spec inv(integer() | rational()) -> rational().

inv(Z) when is_integer(Z), Z =/= 0 -> inv(new(Z));

inv({rational, A, B}) when A =/= 0 -> {rational, B, A};

inv(_Q) -> error(badarith).

%%--------------------------------------------------------------------
%% @doc
%% For rational number `Q = a/b' returns the negation `-a/b'. Accepts
%% plain integer as the argument. Raises `badarith' error on invalid
%% input.
%% @end
%%--------------------------------------------------------------------
-spec neg(integer() | rational()) -> rational().

neg(Z) when is_integer(Z) -> neg(new(Z));

neg({rational, A, B}) -> {rational, -A, B};

neg(_Q) -> error(badarith).

%%--------------------------------------------------------------------
%% @doc
%% For rational number `Q = a/b' returns it's absolute value
%% `abs(a)/b'. Accepts plain integer as the argument. Raises
%% `badarith' error on invalid input.
%% @end
%%--------------------------------------------------------------------
-spec abs(integer() | rational()) -> rational().

abs(Z) when is_integer(Z) -> ?MODULE:abs(new(Z));

abs({rational, A, B}) -> {rational, erlang:abs(A), B};

abs(_Q) -> error(badarith).

%%--------------------------------------------------------------------
%% @doc
%% Product of the rational numbers `Q1' and `Q2'. Accepts plain
%% integers as `Q1' and/or `Q2'. Raises `badarith' error on invalid
%% input.
%% @end
%%--------------------------------------------------------------------
-spec mul(integer() | rational(),
           integer() | rational()) -> rational().

mul(Q, Z) when is_integer(Z) -> mul(Q, new(Z));

mul(Z, Q) when is_integer(Z) -> mul(new(Z), Q);

mul({rational, A, B}, {rational, C, D}) ->
    normalize(reduce({rational, (A * C), (B * D)}));

mul(_Q1, _Q2) -> error(badarith).

%%--------------------------------------------------------------------
%% @doc
%% Quotient of division of two rational numbers `Q1' and `Q2'. Accepts
%% plain integers as `Q1' and/or `Q2' arguments. Raises `badarith'
%% error on invalid input or then `Q2' is zero.
%% @equiv mul(Q1, inv(Q2)).
%% @end
%%--------------------------------------------------------------------
-spec 'div'(integer() | rational(),
           integer() | rational()) -> rational().

'div'(Q1, Q2) -> mul(Q1, inv(Q2)).

%%--------------------------------------------------------------------
%% @doc
%% For rational numbers `Q1' and `Q2' returns sum `Q1 + Q2'. Accepts
%% plain integers as `Q1' and/or `Q2'. Raises `badarith' error on
%% invalid input.
%% @end
%%--------------------------------------------------------------------
-spec add(integer() | rational(),
          integer() | rational()) -> rational().

add(Q, Z) when is_integer(Z) -> add(Q, new(Z));

add(Z, Q) when is_integer(Z) -> add(new(Z), Q);

add({rational, A, B}, {rational, C, D}) ->
    normalize(reduce({rational, (A * D) + (C * B), (B * D)}));

add(_Q1, _Q2) -> error(badarith).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns smaller of two rational numbers.
%% @end
%%--------------------------------------------------------------------
-spec min(integer() | rational(), integer() | rational()) -> rational().

min(Z, Q) when is_integer(Z) -> ?MODULE:min(new(Z), Q);

min(Q, Z) when is_integer(Z) -> ?MODULE:min(Q, new(Z));

min(Q1, Q2) ->
    case le(Q1, Q2) of
        true -> Q1;
        false -> Q2
    end.

%%--------------------------------------------------------------------
%% @doc
%% Returns bigger of two rational numbers.
%% @end
%%--------------------------------------------------------------------
-spec max(integer() | rational(), integer() | rational()) -> rational().

max(Z, Q) when is_integer(Z) -> ?MODULE:max(new(Z), Q);

max(Q, Z) when is_integer(Z) -> ?MODULE:max(Q, new(Z));

max(Q1, Q2) ->
    case ge(Q1, Q2) of
        true -> Q1;
        false -> Q2
    end.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Formats given rational number `Q' as a binary string. Accepts plain
%% integer as an argument. Raises `badarg' error on invalid input.
%% @end
%%--------------------------------------------------------------------
-spec format(integer() | rational()) -> binary().

format(Z) when is_integer(Z) -> integer_to_binary(Z);

format(Q = {rational, _A, _B}) ->
    case normalize(reduce(Q)) of
        {rational, C, 1} ->
            integer_to_binary(C);
        {rational, C, D} ->
            <<(integer_to_binary(C))/bytes, $/,
              (integer_to_binary(D))/bytes>>
    end;

format(_Q) -> error(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Parses rational number from `Bytes' of binary string. Returns
%% rational number `Q' on success or throws `badarg' atom if string
%% doesn't represent any rational number.
%% @todo from_string/1
%% @end
%%--------------------------------------------------------------------
-spec parse(iodata()) -> rational().

parse(Bytes) when is_binary(Bytes) ->
    case parse_integer_part(Bytes) of
        {Num, Num10, <<>>} when Num10 > 1 -> new(Num);
        {Num, Num10, <<$/, Bytes1/bytes>>} when Num10 > 1 ->
            case parse_integer_part(Bytes1) of
                {Denom, Denom10, <<>>} when Denom =/= 0, Denom10 > 1 -> new(Num, Denom);
                {_Denom, _Denom10, _Bytes2} -> throw(badarg)
            end;
        {Int, Int10, <<$., Bytes1/bytes>>} ->
            case parse_non_neg_integer_part(Bytes1, 0, 1) of
                {_Frac, 1, <<>>} when Int10 > 1 -> new(Int);
                {Frac, Frac10, <<>>} when Int < 0 -> new(Int * Frac10 - Frac, Frac10);
                {Frac, Frac10, <<>>} when Frac10 > 1 -> new(Int * Frac10 + Frac, Frac10);
                {_Frac, _Frac10, _Bytes2} -> throw(badarg)
            end;
        {_Num, _Num10, _Bytes1} -> throw(badarg)
    end;

parse(Bytes) when is_list(Bytes) -> parse(iolist_to_binary(Bytes)).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns rational number approximation to the given real number `X'.
%% @end
%%--------------------------------------------------------------------
-spec from_float(float()) -> rational().

from_float(X) -> from_float(X, 21).

%%--------------------------------------------------------------------
%% @doc
%% Converts rational number `Q' to real number. Raises `badarg' error
%% on invalid input.
%% @end
%%--------------------------------------------------------------------
-spec to_float(integer() | rational()) -> float().

to_float(Z) when is_integer(Z) -> float(Z);

to_float({rational, A, B}) -> A / B;

to_float(_Q) -> error(badarg).

%%--------------------------------------------------------------------
%% @doc
%% Converts rational number `Q' to integer, truncating the fractional
%% part. Raises `badarg' error on invalid input.
%% @end
%%--------------------------------------------------------------------
-spec to_integer(integer() | rational()) -> integer().

to_integer(Z) when is_integer(Z) -> Z;

to_integer({rational, A, B}) -> A div B;

to_integer(_Q) -> error(badarg).

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

from_float(X, 0) -> new(trunc(X));

from_float(X, K) ->
    N = trunc(X),
    case X - N of
        F when F == 0 -> new(N);
        F -> add(new(N), inv(from_float(1 / F, K - 1)))
    end.

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize(rational()) -> rational().

normalize({rational, A, B}) when B < 0 -> {rational, -A, -B};

normalize(Q) -> Q.

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse_integer_part(binary()) -> {integer(), pos_integer(), binary()}.

parse_integer_part(<<$-, Other/bytes>>) ->
    {N, N10, Bytes1} = parse_non_neg_integer_part(Other, 0, 1),
    {-N, N10, Bytes1};

parse_integer_part(<<$+, Other/bytes>>) -> parse_non_neg_integer_part(Other, 0, 1);

parse_integer_part(Bytes) -> parse_non_neg_integer_part(Bytes, 0, 1).

%%--------------------------------------------------------------------
%% @priv
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse_non_neg_integer_part(binary(), non_neg_integer(), pos_integer()) ->
                                        {non_neg_integer(), pos_integer(), binary()}.

parse_non_neg_integer_part(<<C, Other/bytes>>, N, N10)
  when C >= $0, C =< $9 ->
    parse_non_neg_integer_part(Other, (N * 10) + (C - $0), N10 * 10);

parse_non_neg_integer_part(Bytes, N, N10) -> {N, N10, Bytes}.

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

denom_1_test_() ->
    [ ?_assertEqual(1, denom(new(1))),
      ?_assertEqual(13, denom(new(2, 13))),
      ?_assertEqual(1, denom(new(-11))),
      ?_assertEqual(2, denom(new(11, -2))),
      ?_assertError(badarg, denom(ok)) ].

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

num_1_test_() ->
    [ ?_assertEqual(1, num(new(1))),
      ?_assertEqual(2, num(new(2, 13))),
      ?_assertEqual(-11, num(new(-11))),
      ?_assertEqual(-11, num(new(11, -2))),
      ?_assertError(badarg, num(ok)) ].

gt_2_test_() ->
    [ ?_assert(gt(1, new(1, 2))),
      ?_assert(gt(1, new(1, 4))),
      ?_assert(gt(0, new(-1, 10))),
      ?_assertNot(gt(0, new(0, 1))),
      ?_assert(gt(3, -1)),
      ?_assertNot(gt(-1, 3)),
      ?_assertNot(gt(3, 3)),
      ?_assertError(badarith, gt(ok, 1)) ].

ge_2_test_() ->
    [ ?_assert(ge(new(1, 2), new(1, 4))),
      ?_assert(ge(new(1, 2), new(1, 2))),
      ?_assertNot(ge(new(1, 2), new(1, 1))),
      ?_assertError(badarith, ge(2, ok)) ].

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

le_2_test_() ->
    [ ?_assert(le(new(-1, 2), new(1, 2))),
      ?_assert(le(new(-1, 2), 0)),
      ?_assert(le(new(-1, 2), new(-1, 2))),
      ?_assert(le(new(-1, 2), new(-1, 4))),
      ?_assertError(badarith, le(ok, 2)) ].

lt_2_test_() ->
    [ ?_assert(lt({rational, 1, -2}, {rational, 1, -4})),
      ?_assert(lt(new(-1, 2), new(-1, 4))),
      ?_assert(lt(new(-1, 2), 0)),
      ?_assertNot(lt(new(-1, 2), new(-1, 2))),
      ?_assertError(badarith, lt(1, ok)) ].

ne_2_test_() ->
    [ ?_assert(ne(new(1), new(2))),
      ?_assertNot(ne(new(1, -12), new(1, -12))),
      ?_assertError(badarith, ne(1.23, 1.23)),
      ?_assertError(badarith, ne(ok, ok)) ].

sub_2_test_() ->
    [ ?_assertEqual(new(0), sub(1, 1)),
      ?_assertEqual(new(1), sub(new(12), new(11))),
      ?_assertEqual(new(-1), sub(11, new(12, 1))),
      ?_assertError(badarith, sub(1.23, 1)) ].

pow_2_test_() ->
    [ ?_assertError(badarith, pow(0, -1)),
      ?_assertError(badarith, pow(ok, 12)),
      ?_assertError(badarith, pow(new(12), false)),
      ?_assertError(badarith, pow(ok, 1)),
      ?_assertEqual(new(1), pow(0, 0)),
      ?_assertEqual(new(1), pow(new(-12), 0)),
      ?_assertEqual(new(1), pow(new(12), 0)),
      ?_assertEqual(new(1, 4), pow(new(1, 2), 2)),
      ?_assertEqual(new(4), pow(new(2), 2)),
      ?_assertEqual(new(1, 4), pow(2, -2)),
      ?_assertEqual(new(1, 65536), pow(2, -16)),
      ?_assertEqual(new(65536), pow(2, 16)),
      ?_assertEqual(new(32), pow(2, 5)),
      ?_assertEqual(new(1, 32), pow(2, -5)) ].

inv_1_test_() ->
    [ ?_assertError(badarith, inv(0)),
      ?_assertEqual({rational, 1, 2}, inv(2)),
      ?_assertEqual({rational, 1, 2}, inv(new(2))),
      ?_assertEqual({rational, 1, 2}, inv(new(2, 1))),
      ?_assertEqual({rational, 2, -1}, inv(new(-1, 2))),
      ?_assertEqual({rational, 2, -1}, inv(new(1, -2))) ].

abs_1_test_() ->
    [ ?_assertEqual(new(1, 2), ?MODULE:abs(new(-1, 2))),
      ?_assertEqual(new(1, 2), ?MODULE:abs(new(1, -2))),
      ?_assertEqual(new(1, 2), ?MODULE:abs(new(1, 2))) ].

mul_2_test_() ->
    [ ?_assertEqual(new(1), mul(new(1, 2), 2)),
      ?_assertEqual(new(1), mul(2, new(1, 2))),
      ?_assertEqual(new(1), mul(new(3, 43), inv(new(3, 43)))),
      ?_assertEqual(new(1), mul(new(-3, 43), inv(new(-3, 43)))),
      ?_assertError(badarith, mul(1.23, 4)),
      ?_assertError(badarith, mul(new(1), ok)) ].

div_2_test_() ->
    [ ?_assertEqual(new(1, 2), 'div'(1, 2)),
      ?_assertEqual(new(-1, 4), 'div'(-1, new(4, 1))),
      ?_assertError(badarith, 'div'(new(2), new(0))),
      ?_assertError(badarith, 'div'(new(2), ok)),
      ?_assertError(badarith, 'div'(new(2), 1.2)) ].

add_2_test_() ->
    [ ?_assertEqual(new(1), add(new(1, 2), new(1, 2))),
      ?_assertEqual(new(0), add(1, -1)),
      ?_assertEqual(new(1, 2), add(new(1, 4), new(1, 4))),
      ?_assertError(badarith, add(1, 2.34)),
      ?_assertError(badarith, add(ok, 1.23)) ].

min_2_test_() ->
    [ ?_assertEqual(new(1), ?MODULE:min(1, 2)),
      ?_assertEqual(new(-1, 2), ?MODULE:min(new(-1, 2), new(1, 2))),
      ?_assertEqual(new(1, 2), ?MODULE:min(new(1, 2), new(3, 4)))
    ].

max_2_test_() ->
    [ ?_assertEqual(new(2), ?MODULE:max(1, 2)),
      ?_assertEqual(new(1, 2), ?MODULE:max(new(-1, 2), new(1, 2))),
      ?_assertEqual(new(3, 4), ?MODULE:max(new(1, 2), new(3, 4)))
    ].

format_1_test_() ->
    [ ?_assertEqual(<<"1">>, format(1)),
      ?_assertEqual(<<"-1">>, format(-1)),
      ?_assertEqual(<<"1/2">>, format(new(1, 2))),
      ?_assertEqual(<<"-1/2">>, format(new(-1, 2))),
      ?_assertEqual(<<"355/113">>, format(new(355, 113))),
      ?_assertEqual(<<"2">>, format(new(4, 2))),
      ?_assertEqual(<<"-2">>, format(new(-4, 2))),
      ?_assertError(badarg, format(ok)),
      ?_assertError(badarg, format({a, b, c})) ].

parse_1_test_() ->
    [ ?_assertEqual({rational, 1, 2}, parse(<<"1/2">>)),
      ?_assertEqual({rational, 0, 1}, parse(<<"0">>)),
      ?_assertEqual({rational, -1, 2}, parse(<<"-1/2">>)),
      ?_assertEqual({rational, -1, 2}, parse(<<"1/-2">>)),
      ?_assertEqual({rational, 1, 2}, parse(<<"-1/-2">>)),
      ?_assertEqual({rational, 1, 2}, parse(<<"+1/+2">>)),
      ?_assertEqual({rational, 355, 113}, parse(<<"355/113">>)),
      ?_assertEqual({rational, 355, 113}, parse(<<"355/+113">>)),
      ?_assertEqual({rational, 355, 113}, parse(<<"+355/113">>)),
      ?_assertEqual({rational, 1, 10}, parse(<<"0.1">>)),
      ?_assertEqual({rational, 0, 1}, parse(<<"0.0">>)),
      ?_assertEqual({rational, 0, 1}, parse(<<"0.">>)),
      ?_assertEqual(new(123), parse(<<"123.">>)),
      ?_assertEqual(new(-123), parse(<<"-123.">>)),
      ?_assertEqual(new(-1234, 10), parse(<<"-123.4">>)),
      ?_assertEqual(new(-12345, 100), parse(<<"-123.45">>)),
      ?_assertThrow(badarg, parse(<<"1.-1">>)),
      ?_assertThrow(badarg, parse(<<"-1.-1">>)),
      ?_assertThrow(badarg, parse(<<>>)),
      ?_assertEqual(new(23, 100), parse(<<".23">>)),
      ?_assertEqual(new(123, 100), parse(<<"1.23">>)),
      ?_assertThrow(badarg, parse(<<".">>)),
      ?_assertThrow(badarg, parse(<<" 1/2 ">>)),
      ?_assertThrow(badarg, parse(<<"1/2 z">>)),
      ?_assertThrow(badarg, parse(<<"1/0">>)),
      ?_assertEqual({rational, 0, 1}, parse(<<"0/1">>)),
      ?_assertEqual({rational, 0, 1}, parse(<<"0/-1">>)),
      ?_assertEqual({rational, 0, 1}, parse(<<"-0/1">>)),
      ?_assertEqual(new(33, 3), parse("33/3")),
      ?_assertEqual(new(33, 3), parse(["33", $/, "3"]))
    ].

to_float_1_test_() ->
    [ ?_assertEqual(2.0, to_float(new(2))),
      ?_assertEqual(3.5, to_float(new(7, 2))),
      ?_assertEqual(-3.5, to_float(new(-7, 2))),
      ?_assertEqual(3.1415929203539825, to_float(new(355, 113))),
      ?_assertError(badarg, to_float(1.234)),
      ?_assertError(badarg, to_float(ok)) ].

from_float_1_test_() ->
    [ ?_assertEqual(new(7, 2), from_float(3.5)),
      ?_assertEqual(new(3), from_float(3)),
      ?_assertEqual(new(-3), from_float(-3)),
      ?_assertEqual(new(-7, 2), from_float(-3.5)),
      ?_assertEqual(math:pi(), to_float(from_float(math:pi()))),
      ?_assertError(badarg, from_float(ok)) ].

-endif.
