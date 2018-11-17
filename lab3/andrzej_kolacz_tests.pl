:- module(andrzej_kolacz_tests, [tests/3]).

tests(empty_program, input(""), program([])).
tests(invalid, input("def main()"), no).
tests(srcpos, input("def main(_) = 1"),
program([def(main, wildcard(file(test, 1, 10, 9, 1)), num(no, 1))])).

tests(comment, input("def fun(_) = 7 (*def main()*)"), yes).
tests(paren, input("def fun((((_)))) = 1"), yes).
tests(bit_sel1, input("def fun(A) = A[[[]]]")).
tests(bit_sel2, input("def fun(_) = A[if A = 1 then A|~A else A&#A..let N=1 in -A+N]"), yes).
tests(pos_works_omg, input("def fun(A, B) = A & B"), 
program([def(fun, pair(file(test,1,9,8,4), var(file(test,1,9,8,1),'A'), var(file(test,1,12,11,1),'B')), op(file(test,1,17,16,5),'&', var(file(test,1,17,16,1),'A'), var(file(test,1,21,20,1),'B')))])).

tests(num_as_arg, input("def main(123) = 1"), no).
tests(wrong_id_digit, input("def 9_abc(_) = 1"), no).
tests(wrong_id_apostrophe, file('apostrophe.hdml'), no).
tests(keyword, input("def def(_) = 1"), no).
tests(eqeq, input("def fun(_) == 2"), no).
tests(wrong_comment, file('comment.hdml'), no).
tests(wrong_let_in, file('adder_in.hdml'), no).
tests(wrong_op, input("def fun(A) = A++"), no).
tests(wrong_if_then_else, input("def fun(A,B) = if A = 1 A & B else A | B"), no).
tests(local_function, file('local_fun.hdml'), no).
tests(without_comma, input("def fun(A,B,C,D) = A & B C | D"), no).
tests(wrong_args, input("def fun(A,if,B) = 1"), no).
tests(float, input("def fun(_) = 3.1415"), no).