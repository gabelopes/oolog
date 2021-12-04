:- module(console, [
  write_success/1,
  write_success/2,
  write_warning/1,
  write_warning/2,
  write_error/1,
  write_error/2
]).

write_success(Message) :-
  write_success(Message, []).
write_success(Message, Arguments) :-
  ansi_format([fg(green)], Message, Arguments).

write_warning(Message) :-
  write_warning(Message, []).
write_warning(Message, Arguments) :-
  ansi_format([fg(yellow)], Message, Arguments).

write_error(Message) :-
  write_error(Message, []).
write_error(Message, Arguments) :-
  with_output_to(user_error, ansi_format([fg(red)], Message, Arguments)).
