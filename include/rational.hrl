%% For use in guards
-define(IS_RATIONAL(Q),
        is_record(Q, rational, 3),
        is_integer(element(2, Q)),
        is_integer(element(3, Q))
       ).
