:- use_module(platform).

:- classdef world_greeter(greeting, name) methodsdef [(
  greet :-
    greeting(Greeting),
    name(Name),
    formulate_sentence(Greeting, Name, Sentence),
    writeln(Sentence)
), (
  formulate_sentence(Greeting, Name, Sentence) :-
    format(string(Sentence), "~w, ~w!\n", [Greeting, Name])
), (
  formulate(Sentence) :-
    greeting(Greeting),
    name(Name),
    formulate_sentence(Greeting, Name, Sentence)
)].

:- classdef person(name, age) methodsdef [(
  kill :-
    name(Name),
    format("~w was killed\n", [Name])
)].

start :-
  Person := person(name="Gabriel", age=12),
  Person::kill,
  WorldGreeter := world_greeter(greeting="Good morning", name="Gabriel"),
  WorldGreeter::formulate(Sentence),
  writeln(Sentence).
