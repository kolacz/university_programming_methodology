:- module(andrzej_kolacz, [parse/3]).

lexer(Tokens) -->
   white_space,
   ( 
      (  "(",       !, { Token = tokLParen }
      ;  ")",       !, { Token = tokRParen } 
      ;  "[",       !, { Token = tokLBrack }
      ;  "]",       !, { Token = tokRBrack }
      ;  "..",      !, { Token = tokDots }
      ;  ",",       !, { Token = tokComma }
      ;  "=",       !, { Token = tokEq }
      ;  "<>",      !, { Token = tokNeq }
      ;  "<=",      !, { Token = tokLeq }
      ;  "<",       !, { Token = tokLt }
      ;  ">=",      !, { Token = tokGeq }
      ;  ">",       !, { Token = tokGt }
      ;  "^",       !, { Token = tokCaret }
      ;  "|",       !, { Token = tokBar }
      ;  "+",       !, { Token = tokPlus }
      ;  "-",       !, { Token = tokMinus }
      ;  "&",       !, { Token = tokAnd }
      ;  "*",       !, { Token = tokTimes }
      ;  "/",       !, { Token = tokDiv }
      ;  "\u0025",  !, { Token = tokPercent }
      ;  "@",       !, { Token = tokAt }
      ;  "#",       !, { Token = tokHash }
      ;  "~",       !, { Token = tokTilde }
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (def, tokDef),
                                     (else, tokElse),
                                     (if, tokIf),
                                     (in, tokIn), 
                                     (let, tokLet),
                                     (then, tokThen),
                                     ('_', tokUndscr) ]),
               !
            ;  Token = tokVar(Id)
            }
      ;  [_],
            { Token = tokUnknown }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).

commentEnd -->
      ( 
            "*)", !
      ; 
            [_], commentEnd
      ).

white_space -->
      ( 
            "(*", !, commentEnd, white_space
      ; 
            [Char], { code_type(Char, space) }, !, white_space
      ; 
            []
      ).
   
digit(D) -->
      [D],
            { code_type(D, digit) }.

digits([D|T]) -->
      digit(D),
      !,
      digits(T).
digits([]) -->
      [].

number(D, N) -->
      digits(Ds),
            { number_chars(N, [D|Ds]) }.

letter(L) -->
      [L], { code_type(L, csym) }.

alphanum([A|T]) -->
      [A], { code_type(A, csym) ; char_code('''', A) } , !, alphanum(T).
alphanum([]) -->
      [].

identifier(L, Id) -->
      alphanum(As),
            { atom_codes(Id, [L|As]) }.

:- op(700, xfx, <=).
:- op(700, xfx, <>).
:- op(700, xfx, >=).
:- op(650, xfy, '@').
:- op(500, yfx, '||').
:- op(500, yfx, '^').
:- op(500, yfx, '+').
:- op(500, yfx, '-').
:- op(450, yfx, '&').
:- op(450, yfx, '*').
:- op(450, yfx, '/').
:- op(450, yfx, '\u0025').

program(Ast) --> 
      definitions(Ast), !.
   
definitions(Ast) -->
      (
            definition(Def),
            (  definitions(Rest), !,
                  { Ast = [Def|Rest] }
            ;  [],
                  { Ast = [Def] }
            )
      ;
            [], { Ast = [] }
      ).

definition(Def) -->
      [tokDef], !, [tokVar(Name)], 
      [tokLParen], pattern1(Patt), [tokRParen], [tokEq],
      expression(Expr),
      { Def = def(Name, Patt, Expr) }.

pattern1(Patt) --> 
      (
            pattern(P1), [tokComma],!, pattern1(P2),
            { Patt = pair(no, P1, P2) }
      ;
            pattern(Patt)
      ).

pattern(Patt) --> 
      (
            [tokUndscr],!, { Patt = wildcard(no) }
      ;
            [tokLParen],!, pattern(Patt), [tokRParen]
      ;
            [tokVar(Id)], { Patt = var(no, Id) }
      ).


expression(Expr) -->
      (
            [tokIf], !, expression(E1), 
            [tokThen], expression(E2), [tokElse], expression(E3),
            { Expr = if(no, E1, E2, E3) }
      ;
            [tokLet], !, pattern1(P), [tokEq], 
            expression(E1), [tokIn], expression(E2),
            { Expr = let(no, P, E1, E2) }
      ;
            op_expr(Expr)
      ).

op_expr(Expr) -->
      (
            op2_expr(E1), [tokComma], !, op_expr(E2),
            { Expr = pair(no, E1, E2) }
      ;
            op2_expr(Expr)
      ).

op2_expr(Expr) -->
      ( 
            op3_expr(E1), op2(Op),!, op3_expr(E2),
            { Expr = op(no, Op, E1, E2) }
      ;
            op3_expr(Expr)
      ).

op3_expr(Expr) -->
      (
            op4_expr(E1), [tokAt], !, op3_expr(E2),
            { Expr = op(no, @, E1, E2) }
      ;
            op4_expr(Expr)
      ).

op4_expr(Expr) -->
      op5_expr(E1), op4_expr(E1, Expr).

op4_expr(Acc, Expr) -->
      op4(Op), !, op5_expr(E2),
      { Acc1 = op(no, Op, Acc, E2) },
      op4_expr(Acc1, Expr).
op4_expr(Acc, Acc) -->
      [].
      
op5_expr(Expr) -->
      unary_op_expr(E1), op5_expr(E1, Expr).

op5_expr(Acc, Expr) -->
      op5(Op),!, unary_op_expr(E2),
      { Acc1 = op(no, Op, Acc, E2) },
      op5_expr(Acc1, Expr).
op5_expr(Acc, Acc) -->
      [].

unary_op_expr(Expr) -->
      (
            unary_op(Op), !, op_expr(E),
            { Expr = op(no, Op, E) }
      ;
            simple_expr(Expr)
      ).


simple_expr(Expr) -->
      (
            [tokLParen], !, expression(Expr), [tokRParen]
      ;
            atomic_expr(Expr), !
      ;
            simple_expr1(E1),[tokLBrack], expression(E2), 
            [tokDots],!, expression(E3), [tokRBrack],
            { Expr = bitsel(no, E1, E2, E3) }
      ;
            simple_expr1(E1), [tokLBrack], expression(E2), [tokRBrack],
            { Expr = bitsel(no, E1, E2) }
      
      ).

%simple_expr1(Expr) --> %pozbywamy się lewostronnej rekursji
%      (
%            [], !
%      ;
%            [tokLParen], !, expression(Expr), [tokRParen]
%      ;
%            atomic_expr(Expr), !
%      ;
%            simple_expr1(E1),[tokLBrack], expression(E2), 
%            [tokDots],!, expression(E3), [tokRBrack],
%            { Expr = bitsel(no, E1, E2, E3) }
%      ;
%            simple_expr1(E1), [tokLBrack], expression(E2), [tokRBrack],
%            { Expr = bitsel(no, E1, E2) }     
%      ).

simple_expr1(Expr) --> %pozbywamy się lewostronnej rekursji
      (
            [], !
      ;
            atomic_expr(Expr), !
      ;
            [tokLParen], !, expression(Expr), [tokRParen]
      ;
            bit_sels(Expr), !
      ;
            bit_sel(Expr)   
      ).

% problem z selekcją bitów

bit_sel(Expr) -->
      simple_expr1(E1), [tokLBrack], expression(E2), [tokRBrack],
      { Expr = bitsel(no, E1, E2) }.

bit_sels(Expr) -->
      simple_expr1(E1), [tokLBrack], expression(E2), 
      [tokDots], expression(E3), [tokRBrack],
      { Expr = bitsel(no, E1, E2, E3) }.

atomic_expr(Expr) -->
      (
            [tokVar(Name)], [tokLParen],!, expression(E), [tokRParen],
            { Expr = call(no, Name, E) }
      ;
            [tokVar(Id)], !, { Expr = var(no, Id) }
      ;
            [tokNumber(N)], !, { Expr = num(no, N) } 
      ;
            [tokLBrack], expression(E), !, [tokRBrack],
            { Expr = bit(no, E) }
      ;
            [tokLBrack], [tokRBrack], { Expr = empty(no) }
      ).


op2(=) -->
      [tokEq], !.
op2(<>) -->
      [tokNeq], !.
op2(<) -->
      [tokLt], !.
op2(<=) -->
      [tokLeq], !.
op2(>) -->
      [tokGt], !.
op2(>=) -->
      [tokGeq].

op4('||') --> % prolog zabrania inaczej
      [tokBar], !.
op4(^) -->
      [tokCaret].
op4(+) -->
      [tokPlus], !.
op4(-) -->
      [tokMinus].

op5('&') -->
      [tokAnd], !.
op5(*) -->
      [tokTimes], !.
op5(/) -->
      [tokDiv], !.
op5('\u0025') --> %
      [tokPercent].

unary_op(-) -->
      [tokMinus], !.
unary_op(#) -->
      [tokHash], !.
unary_op(~) -->
      [tokTilde].



parse(_Path,CharListCodes,Ast) :-
    phrase(lexer(Tokens),CharListCodes),
    phrase(program(Ast),Tokens).

%parse(_Path, Codes, Program) :-
%  Codes = [], Program = [].


