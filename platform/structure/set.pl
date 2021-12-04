:- module(set, [
  create_set/3,
  create_set/4,
  merge_set/3,
  merge_set/4,
  subtract_set/3,

  % Mappers
  identity/2
]).

create_set(Elements, Mapper, Set) :-
  create_set(Elements, Mapper, Set, _).

create_set(Elements, Mapper, Set, DifferentialSet) :-
  merge_set(set(Mapper, []), Elements, Set, DifferentialSet).

merge_set(Set, Elements, MergedSet) :-
  merge_set(Set, Elements, MergedSet, _).

merge_set(set(Mapper, Set), Elements, set(Mapper, MergedSet), DifferentialSet) :-
  map_set(set(Mapper, Set), MappedSet),
  differentiate_set(Mapper, MappedSet, Elements, DifferentialSet),
  append([Set, DifferentialSet], MergedSet).

differentiate_set(_, _, [], []).
differentiate_set(Mapper, MappedSet, [Element|Elements], [Element|DifferentialSet]) :-
  map_element(Mapper, Element, MappedElement),
  \+ member(MappedElement, MappedSet),
  differentiate_set(Mapper, [MappedElement|MappedSet], Elements, DifferentialSet).
differentiate_set(Mapper, MappedSet, [_|Elements], DifferentialSet) :-
  differentiate_set(Mapper, MappedSet, Elements, DifferentialSet).

map_set(set(_, []), []).
map_set(set(Mapper, [Element|Elements]), [MappedElement|MappedElements]) :-
  map_element(Mapper, Element, MappedElement),
  map_set(set(Mapper, Elements), MappedElements).

map_element(Mapper, Element, MappedElement) :-
  create_mapper_goal(Mapper, [Element, MappedElement], MapperGoal),
  call(MapperGoal), !.

create_mapper_goal(Module:GoalName, Arguments, MapperGoal) :-
  Goal =.. [GoalName|Arguments],
  MapperGoal =.. [':', Module, Goal].
create_mapper_goal(GoalName, Arguments, MapperGoal) :-
  MapperGoal =.. [GoalName|Arguments].

%% Subtracts elements from a given Set, following its mapping policy.
%  If given Set is not proper, duplicated elements are kept in the Subtraction.
subtract_set(set(_, Subtraction), [], Subtraction).
subtract_set(set(Mapper, Set), [Element|Elements], Subtraction) :-
  delete_element(set(Mapper, Set), Element, ResultingSet),
  subtract_set(set(Mapper, ResultingSet), Elements, Subtraction).

delete_element(Set, Element, ResultingSet) :-
  delete_element([], Set, Element, ResultingSet).

delete_element(Head, set(_, []), _, Head).
delete_element(Head, set(Mapper, [CurrentElement|Tail]), Element, ResultingSet) :-
  map_element(Mapper, Element, MappedElement),
  map_element(Mapper, CurrentElement, MappedCurrentElement),
  MappedElement = MappedCurrentElement,
  append(Head, Tail, ResultingSet).
delete_element(Head, set(Mapper, [CurrentElement|Tail]), Element, ResultingSet) :-
  append(Head, [CurrentElement], CurrentHead),
  delete_element(CurrentHead, set(Mapper, Tail), Element, ResultingSet).

% Mappers
identity(Element, Element).
