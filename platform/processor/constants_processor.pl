:- module(constants_processor, [
  get_constant_signature/2
]).

get_constant_signature(Constant, constant(Name, Arity)) :-
  compound_name_arguments(Constant, Name, Arguments),
  length(Arguments, Arity).
