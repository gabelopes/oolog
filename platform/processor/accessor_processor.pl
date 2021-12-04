:- module(accessor_processor, [
  access_attribute/3
]).

:- use_module(library(unison)).
:- use_module('../manager/reference_manager').
:- use_module('../lifecycle/exception').

access_attribute(Reference, Name, Value) :-
  reference_manager:get_object(Reference, Object),
  handle_attribute(Reference, Object, Name, Value).
access_attribute(_, Name, _) :-
  raise_exception("Attribute ~w does not exist in reference.", [Name]).

handle_attribute(Reference, Object, Name, Value) :-
  _{ data: data{ attributes: Attributes }} :<< Object,
  get_dict(Name, Attributes, CurrentValue),
  handle_value(Reference, Object, Name, Value, CurrentValue).

handle_value(Reference, Object, Name, Value, allocate(Reference)) :-
  nonvar(Value),
  upadate_attribute(Object, Name, Value).
handle_value(_, _, Name, _, allocate(_)) :-
  raise_exception("Fatal error! Forbidden access to attribute '~w' from requested context.", [Name]).
handle_value(_, _, _, Value, Value).

upadate_attribute(Object, Name, Value) :-
  _{ data: Data } :< Object,
  data{ attributes: Attributes } :< Data,
  UpdatedAttributes = Attributes.put([Name=Value]),
  UpdatedData = Data.put([attributes=UpdatedAttributes]),
  UpdatedObject = Object.put([data=UpdatedData]),
  reference_manager:update_object(UpdatedObject).
