:- module(header, [
  header/3
]).

header(Module:Header, Module:Name, []) :-
  nonvar(Header),
  functor(Header, Name, 0, atom).
header(Module:Header, Module:Name, Arguments) :-
  nonvar(Header),
  functor(Header, Name, _, compound),
  compound_name_arguments(Header, Name, Arguments).
header(Header, Name, []) :-
  nonvar(Header),
  functor(Header, Name, 0, atom).
header(Header, Name, Arguments) :-
  nonvar(Header),
  functor(Header, Name, _, compound),
  compound_name_arguments(Header, Name, Arguments).
header(Module:Header, Module:Name, []) :-
  nonvar(Name),
  compound_name_arguments(Header, Name, []).
header(Module:Header, Module:Name, Arguments) :-
  nonvar(Name),
  nonvar(Arguments),
  compound_name_arguments(Header, Name, Arguments).
header(Header, Name, []) :-
  nonvar(Name),
  compound_name_arguments(Header, Name, []).
header(Header, Name, Arguments) :-
  nonvar(Name),
  nonvar(Arguments),
  compound_name_arguments(Header, Name, Arguments).
