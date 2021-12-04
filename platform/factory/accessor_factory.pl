:- module(accessor_factory, [
  create_accessors/3
]).

:- use_module('../processor/accessor_processor').

create_accessors(_, [], []).
create_accessors(Reference, [AttributeDefinition|AttributeDefinitions], [method{
  modifiers: Modifiers,
  name: Name,
  arity: 1,
  arguments: [Value],
  body: (
    accessor_processor:access_attribute(Reference, Name, Value)
  )
}|Accessors]) :-
  attribute{ name: Name, modifiers: Modifiers } :< AttributeDefinition,
  create_accessors(Reference, AttributeDefinitions, Accessors).
