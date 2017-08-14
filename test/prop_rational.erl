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

prop_sum_assoc() ->
    ?FORALL(
       {A, B, C},
       {rational(), rational(), rational()},
       rational:eq(
         rational:sum(A, rational:sum(B, C)),
         rational:sum(rational:sum(A, B), C)
        )
      ).

prop_prod_assoc() ->
    ?FORALL(
       {A, B, C},
       {rational(), rational(), rational()},
       rational:eq(
         rational:prod(A, rational:prod(B, C)),
         rational:prod(rational:prod(A, B), C)
        )
      ).

prop_sum_comm() ->
    ?FORALL(
       {A, B},
       {rational(), rational()},
       rational:eq(
         rational:sum(A, B),
         rational:sum(B, A)
        )
      ).

prop_prod_comm() ->
    ?FORALL(
       {A, B},
       {rational(), rational()},
       rational:eq(
         rational:prod(A, B),
         rational:prod(B, A)
        )
      ).

prop_sum_id() ->
    ?FORALL(
       A,
       rational(),
       rational:eq(
         rational:new(0),
         rational:sum(A, rational:neg(A))
        )
      ).

prop_prod_id() ->
    ?FORALL(
       A,
       rational(),
       ?IMPLIES(rational:ne(A, 0),
                rational:eq(
                  rational:new(1),
                  rational:prod(A, rational:inv(A))
                 ))
      ).

prop_prod_zero() ->
    ?FORALL(
       A,
       rational(),
       rational:eq(
         rational:new(0),
         rational:prod(A, rational:new(0))
        )
      ).

prop_distrib() ->
    ?FORALL(
       {A, B, C},
       {rational(), rational(), rational()},
       rational:eq(
         rational:prod(A, rational:sum(B, C)),
         rational:sum(rational:prod(A, B), rational:prod(A, C))
        )
      ).

rational() ->
    oneof([ integer(),
            ?LET({N, D}, {integer(), pos_integer()}, rational:new(N, D)) ]).
