:- module(platform, [
  declare_class/3,
  instantiate/3,
  invoke/3,
% Operators 
  (:=)/2,
  (::)/2,
  classdef/1,
% Operators Definition
  op(500, xfy, (:=)),
  op(500, xfy, (::)),
  op(500, fx, classdef),
  op(400, xfy, methodsdef)
]).

:- use_module(executor).

:- dynamic class/3.

%% 
%  Classes
%%
'classdef'(Declaration) :-
  Declaration = Header methodsdef Methods,
  Header =.. [Name|Attributes],
  declare_class(Name, Attributes, Methods).

declare_class(Name, _, _) :-
  class(Name, _, _),
  format("Class '~w' is already declared!").
declare_class(Name, Attributes, Methods) :-
  assertz(class(Name, Attributes, Methods)).

%% 
%  Instances
%%
':='(Identifier, ConstructorCall) :-
  ConstructorCall =.. [ClassName|Arguments],
  instantiate(ClassName, Arguments, Identifier).

instantiate(ClassName, InstanceAttributes, Instance) :-
  class(ClassName, ClassAttributes, _),
  uuid(UUID),
  filter_attributes(ClassAttributes, InstanceAttributes, FilteredAttributes),
  Instance =.. [ClassName, UUID, FilteredAttributes].

filter_attributes([], _, []).
filter_attributes([ClassAttribute|ClassAttributes], InstanceAttributes, [InstanceAttribute|FilteredAttributes]) :-
  get_attribute(InstanceAttributes, ClassAttribute, InstanceAttribute),
  filter_attributes(ClassAttributes, InstanceAttributes, FilteredAttributes).

get_attribute([], Name, _) :-
  format("Could not find attribute '~w' in provided class.", [Name]),
  fail.
get_attribute([Name=Value|_], Name, Attribute) :-
  Attribute =.. [Name, Value].
get_attribute([Attribute|_], Name, Attribute) :-
  Attribute =.. [Name|_].
get_attribute([_|Attributes], Name, Attribute) :-
  get_attribute(Attributes, Name, Attribute).

%%
%  Invocation
%%
'::'(Instance, MethodCall) :-
  MethodCall =.. [MethodName|MethodArguments],
  invoke(Instance, MethodName, MethodArguments).

invoke(Instance, MethodName, MethodArguments) :-
  Instance =.. [ClassName|_],
  class(ClassName, _, ClassMethods),
  execute_method(ClassMethods, Instance, MethodName, MethodArguments).
