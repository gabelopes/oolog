:- module(attributes_processor, [
  get_attribute_signature/2,
  validate_duplicate_attributes/2,
  validate_overlapping_attributes/2,
  find_attributes_by_scope/3,
  find_attribute_by_name/3,
  initialize_attributes/2,
  allocate_attributes/3
]).

:- use_module(library(unison)).
:- use_module('../structure/set').
:- use_module('../lifecycle/exception').
:- use_module('../processor/arity_processor').

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

get_overlapping_attributes(Attributes, SuperClassMethods, OverlappingAttributes) :-
  findall(Attribute, (
    member(Attribute, Attributes),
    member(SuperClassMethod, SuperClassMethods),
    _{ name: Name, modifiers: _{ scope: Scope }} :<< Attribute,
    _{ name: Name, arity: Arity, modifiers: _{ scope: Scope }} :<< SuperClassMethod,
    Arity >@< 1
  ), OverlappingAttributes).

validate_overlapping_attributes(Attributes, SuperClassMethods) :-
  get_overlapping_attributes(Attributes, SuperClassMethods, OverlappingAttributes),
  validate_overlapping_attributes(OverlappingAttributes).

validate_overlapping_attributes([]).
validate_overlapping_attributes(OverlappingAttributes) :-
  format_overlapping_attributes(OverlappingAttributes, Result),
  raise_exception("Error declaring class. One or more attributes would overwrite parent methods: ~w", [Result]).

format_overlapping_attributes(OverlappingAttributes, Result) :-
  format_overlapping_attributes(OverlappingAttributes, "", Result).

format_overlapping_attributes([], Result, Result).
format_overlapping_attributes([OverlappingAttribute|OverlappingAttributes], CurrentFormatting, Result) :-
  get_attribute_signature(OverlappingAttribute, attribute(Name, Scope)),
  format(string(OverlappingAttributeFormatting), "~n - ~w overwrites ~w/1 from parent class (at ~w level)", [Name, Name, Scope]),
  string_concat(CurrentFormatting, OverlappingAttributeFormatting, Formatting),
  format_overlapping_attributes(OverlappingAttributes, Formatting, Result).

find_attributes_by_scope(Attributes, Scopes, FoundAttributes) :-
  findall(Attribute, (
    member(Attribute, Attributes),
    attribute{
      modifiers: modifiers{
        scope: Scope
      }
    } :<< Attribute,
    member(Scope, Scopes)
  ), FoundAttributes).

find_attribute_by_name(Attributes, Name, Attribute) :-
  findall(Attribute, (
    member(Attribute, Attributes),
    attribute{ name: Name } :< Attribute
  ), [Attribute]).

initialize_attributes(AttributeDefinitions, Attributes) :-
  initialize_attributes(AttributeDefinitions, map{}, Attributes).

initialize_attributes([], Attributes, Attributes).
initialize_attributes([AttributeDefinition|AttributeDefinitions], CurrentAttributes, Attributes) :-
  attribute{ name: Name, initial_value: InitialValue } :< AttributeDefinition,
  PartialAttributes = CurrentAttributes.put([Name=InitialValue]),
  initialize_attributes(AttributeDefinitions, PartialAttributes, Attributes).

allocate_attributes(Reference, AttributeDefinitions, Attributes) :-
  allocate_attributes(Reference, AttributeDefinitions, map{}, Attributes).

allocate_attributes(_, [], Attributes, Attributes).
allocate_attributes(Reference, [AttributeDefinition|AttributeDefinitions], CurrentAttributes, Attributes) :-
  attribute{ name: Name } :< AttributeDefinition,
  PartialAttributes = CurrentAttributes.put([Name=allocate(Reference)]),
  allocate_attributes(Reference, AttributeDefinitions, PartialAttributes, Attributes).
