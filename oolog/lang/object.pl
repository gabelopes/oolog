:- module(object, []).

:- use_module('../../platform/operators/reference_operators').
:- use_module('../../platform/orchestrator/class_orchestrator').
:- use_module('../../platform/manager/context_manager').
:- use_module('../../platform/manager/instance_manager').
:- use_module('../../platform/manager/class_manager').

:- class object implements [
  public get_class(Class) => (
    get_instance(Instance),
    instance{ class: ClassReference } :< Instance,
    class_manager:get_class(ClassReference, ClassDefinition),
    Class := class(ClassDefinition)
  ),

  public to_string(String) => (
    get_class(Class),
    get_instance(Instance),
    class{ name: ClassName } :< Class,
    instance{ reference: &InstanceReference } :< Instance,
    format(string(String), "~w@~w", [ClassName, InstanceReference])
  ),

  private get_instance(Instance) => (
    context_module(ContextReference),
    context_manager:get_context(&ContextReference, Context),
    context{ binding: InstanceReference } :< Context,
    instance_manager:get_instance(InstanceReference, Instance)
  ),

  public static class(Class) => (
    context_module(ContextReference),
    context_manager:get_context(&ContextReference, Context),
    context{ binding: ClassReference } :< Context,
    class_manager:get_class(ClassReference, ClassDefinition),
    Class := class(ClassDefinition)
  )
].
