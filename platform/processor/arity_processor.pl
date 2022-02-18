:- module(arity_processor, [
  (>@<)/2,
  op(500, xfy, (>@<))
]).

'>@<'(A, B) :-
  has_overlap(A, B), !.

has_overlap(arity(_, variable), arity(_, variable)).
has_overlap(arity(Arity, static), arity(Arity, static)).
has_overlap(arity(A, static), arity(B, variable)) :-
  A >= B.
has_overlap(arity(A, variable), arity(B, static)) :-
  B >= A.
has_overlap(A, arity(Arity, Type)) :-
  has_overlap(arity(A, static), arity(Arity, Type)).
has_overlap(arity(Arity, Type), B) :-
  has_overlap(arity(Arity, Type), arity(B, static)).
has_overlap(A, A).

user:portray(arity(Arity, static), Arity).
user:portray(arity(Arity, variable), Portrayal) :-
  atom_concat(Arity, '..n', Portrayal).
