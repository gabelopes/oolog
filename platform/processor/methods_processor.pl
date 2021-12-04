:- module(methods_processor, [
  get_method_signature/2,
  validate_duplicate_methods/2,
  validate_overlapping_methods/2,
  find_methods_by_scope/3,
  find_methods_by_visibility/3
]).

:- use_module(library(unison)).
:- use_module('../structure/set').
:- use_module('../lifecycle/exception').

get_method_signature(Method, method(Name, Arity, Scope)) :-
  _{ name: Name, arity: Arity, modifiers: _{ scope: Scope }} :<< Method.

validate_duplicate_methods(Methods, DifferentialMethods) :-
  subtract_set(set(methods_processor:get_method_signature, Methods), DifferentialMethods, Repetitions),
  validate_repetitions(Repetitions).

validate_repetitions([]).
validate_repetitions(Repetitions) :-
  format_repetitions(Repetitions, Result),
  raise_exception("Duplicate methods found in class: ~w", [Result]).

format_repetitions(Repetitions, Result) :-
  format_repetitions(Repetitions, "", Result).

format_repetitions([], Result, Result).
format_repetitions([Repetition|Repetitions], CurrentFormatting, Result) :-
  get_method_signature(Repetition, method(Name, Arity, Scope)),
  format(string(RepetitionFormatting), "~n - ~w/~w (~w)", [Name, Arity, Scope]),
  string_concat(CurrentFormatting, RepetitionFormatting, Formatting),
  format_repetitions(Repetitions, Formatting, Result).

get_overlapping_methods(Methods, Attributes, OverlappingMethods) :-
  findall(Method, (
    member(Method, Methods),
    member(Attribute, Attributes),
    _{ name: Name, arity: 1, modifiers: _{ scope: Scope }} :<< Method,
    _{ name: Name, modifiers: _{ scope: Scope }} :<< Attribute
  ), OverlappingMethods).

validate_overlapping_methods(Methods, Attributes) :-
  get_overlapping_methods(Methods, Attributes, OverlappingMethods),
  validate_overlapping_methods(OverlappingMethods).

validate_overlapping_methods([]).
validate_overlapping_methods(OverlappingMethods) :-
  format_overlapping_methods(OverlappingMethods, Result),
  raise_exception("Methods clashing with attributes found in class: ~w", [Result]).

format_overlapping_methods(OverlappingMethods, Result) :-
  format_overlapping_methods(OverlappingMethods, "", Result).

format_overlapping_methods([], Result, Result).
format_overlapping_methods([OverlappingMethod|OverlappingMethods], CurrentFormatting, Result) :-
  get_method_signature(OverlappingMethod, method(Name, Arity, Scope)),
  format(string(OverlappingMethodFormatting), "~n - ~w/~w, ~w (~w)", [Name, Arity, Name, Scope]),
  string_concat(CurrentFormatting, OverlappingMethodFormatting, Formatting),
  format_overlapping_methods(OverlappingMethods, Formatting, Result).

find_methods_by_scope(Methods, Scopes, FoundMethods) :-
  findall(Method, (
    member(Method, Methods),
    method{
      modifiers: modifiers{
        scope: Scope
      }
    } :<< Method,
    member(Scope, Scopes)
  ), FoundMethods).

find_methods_by_visibility(Methods, Visibilities, FoundMethods) :-
  findall(Method, (
    member(Method, Methods),
    method{
      modifiers: modifiers{
        visibility: Visibility
      }
    } :<< Method,
    member(Visibility, Visibilities)
  ), FoundMethods).
