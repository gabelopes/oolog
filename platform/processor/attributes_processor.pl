:- module(attributes_processor, [
  get_attribute_signature/2,
  validate_duplicate_attributes/2,
  find_attributes/3
]).

:- use_module(library(unison)).
:- use_module('../structure/set').
:- use_module('../lifecycle/exception').

get_attribute_signature(Attribute, attribute(Name, Scope)) :-
  _{ name: Name, modifiers: _{ scope: Scope }} :<< Attribute.

validate_duplicate_attributes(Attributes, DifferentialAttributes) :-
  subtract_set(set(attributes_processor:get_attribute_signature, Attributes), DifferentialAttributes, Repetitions),
  validate_repetitions(Repetitions).

validate_repetitions([]).
validate_repetitions(Repetitions) :-
  format_repetitions(Repetitions, Result),
  raise_exception("Clashing attributes found in class: ~w", [Result]).

format_repetitions(Repetitions, Result) :-
  format_repetitions(Repetitions, "", Result).

format_repetitions([], Result, Result).
format_repetitions([Repetition|Repetitions], CurrentFormatting, Result) :-
  get_attribute_signature(Repetition, attribute(Name, Scope)),
  format(string(RepetitionFormatting), "~n - ~w (~w)", [Name, Scope]),
  string_concat(CurrentFormatting, RepetitionFormatting, Formatting),
  format_repetitions(Repetitions, Formatting, Result).

find_attributes(Attributes, Scopes, FoundAttributes) :-
  findall(Attribute, (
    member(Attribute, Attributes),
    attribute{
      modifiers: modifiers{
        scope: Scope
      }
    } :<< Attribute,
    member(Scope, Scopes)
  ), FoundAttributes).
