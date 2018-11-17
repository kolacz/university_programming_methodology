:- module(andrzej_kolacz_tests, [tests/5]).

:- op(200, fx, ~).
:- op(500, xfy, v).

tests(excluded_middle, validity, [p v ~p], 500, solution([(p,t)])).
tests(contains_empty_clause, validity, [p v ~q, [], r], 500, count(0)).
tests(pqrs, validity, [p v q v r v s], 500, solution([(p,f),(q,f),(r,f),(s,t)])).
tests(unsatisfiable, validity, [p v q, p v ~q, ~p v q, ~p v ~q], 500, count(0)).
tests(empty, validity, [], 500, count(0)).
tests(implication, validity, [~p v q], 500, solution([(p,f),(q,t)])).
tests(unsatisfiable_excluded_middle, validity, [p v ~p, p, ~p], 500, count(0)).
tests(q_any, validity, [p v q, p v ~q], 500, solution([(p,t),(q,f)])).
tests(redundancy, validity, [p v ~q, ~q v p, ~q v ~q v p, p v p v ~q, ~q v p v ~q v p v ~q v p], 1000, count(3)).