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
-spec inv(rational()) -> rational().

inv({rational, A, B}) ->
    {rational, B, A}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec neg(rational()) -> rational().

neg({rational, A, B}) ->
    {rational, -A, B}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prod(rational(), rational()) -> rational().

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
-spec sum(rational(), rational()) -> rational().

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
-spec format(rational()) -> binary().

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

%% TODO

-endif.
