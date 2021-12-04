:- module(constants_processor, [
  get_constant_signature/2
]).

:- use_module('../structure/header').

get_constant_signature(Constant, constant(Name, Arity)) :-
  header(Constant, Name, Arguments),
  length(Arguments, Arity).
