:- module(exception, [
  raise_exception/1,
  raise_exception/2,
  raise_exception/3
]).

:- use_module('../io/console').

raise_exception(Message) :-
  raise_exception(Message, []).

raise_exception(MessageFormat, Arguments) :-
  raise_exception(MessageFormat, Arguments, 1).

raise_exception(MessageFormat, Arguments, ExitCode) :-
  format(string(Exception), MessageFormat, Arguments),
  write_error(Exception),
  halt(ExitCode).
