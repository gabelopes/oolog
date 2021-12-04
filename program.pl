:- use_module('platform/orchestrator/class_orchestrator').
:- use_module('platform/orchestrator/instance_orchestrator').
:- use_module('platform/orchestrator/method_orchestrator').

:- class rectangle(protected width, protected height) implements [
  public constructor(Width, Height) => (
    width(Width),
    height(Height)
  ),

  public area(Area) => (
    width(Width),
    height(Height),
    Area is Width * Height
  )
].

:- class square(protected size) extends rectangle implements [
  public constructor(Size) => (
    width(Size),
    height(Size)
  )
].

start :-
  Square := square(30),
  Square::area(Area),
  writeln(Area).
