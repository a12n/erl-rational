-module(prop_rational).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

prop_num() ->
    ?FORALL(
       Num,
       integer(),
       Num =:= rational:num(rational:new(Num, 1))
      ).

prop_denom() ->
    ?FORALL(
       Denom,
       pos_integer(),
       Denom =:= rational:denom(rational:new(1, Denom))
      ).

prop_one() ->
    ?FORALL(
       N,
       pos_integer(),
       rational:eq(1, rational:new(N, N))
      ).

prop_add_assoc() ->
    ?FORALL(
       {A, B, C},
       {rational(), rational(), rational()},
       rational:eq(
         rational:add(A, rational:add(B, C)),
         rational:add(rational:add(A, B), C)
        )
      ).

prop_mul_assoc() ->
    ?FORALL(
       {A, B, C},
       {rational(), rational(), rational()},
       rational:eq(
         rational:mul(A, rational:mul(B, C)),
         rational:mul(rational:mul(A, B), C)
        )
      ).

prop_add_comm() ->
    ?FORALL(
       {A, B},
       {rational(), rational()},
       rational:eq(
         rational:add(A, B),
         rational:add(B, A)
        )
      ).

prop_mul_comm() ->
    ?FORALL(
       {A, B},
       {rational(), rational()},
       rational:eq(
         rational:mul(A, B),
         rational:mul(B, A)
        )
      ).

prop_add_id() ->
    ?FORALL(
       A,
       rational(),
       rational:eq(
         rational:new(0),
         rational:add(A, rational:neg(A))
        )
      ).

prop_mul_id() ->
    ?FORALL(
       A,
       rational(),
       ?IMPLIES(rational:ne(A, 0),
                rational:eq(
                  rational:new(1),
                  rational:mul(A, rational:inv(A))
                 ))
      ).

prop_mul_zero() ->
    ?FORALL(
       A,
       rational(),
       rational:eq(
         rational:new(0),
         rational:mul(A, rational:new(0))
        )
      ).

prop_distrib() ->
    ?FORALL(
       {A, B, C},
       {rational(), rational(), rational()},
       rational:eq(
         rational:mul(A, rational:add(B, C)),
         rational:add(rational:mul(A, B), rational:mul(A, C))
        )
      ).

prop_pow_zero() ->
    ?FORALL(
       A,
       rational(),
       rational:eq(
         rational:new(1),
         rational:pow(A, 0)
        )
      ).

prop_pow_one() ->
    ?FORALL(
       A,
       rational(),
       rational:eq(
         A,
         rational:pow(A, 1)
        )
      ).

prop_pow_two() ->
    ?FORALL(
       A,
       rational(),
       rational:eq(
         rational:mul(A, A),
         rational:pow(A, 2)
        )
      ).

rational() ->
    oneof([ integer(),
            ?LET({N, D}, {integer(), pos_integer()}, rational:new(N, D)) ]).
