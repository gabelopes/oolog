interface class {
  reference?: reference;
  package: atom;
  name: string;
  super_class: reference;
  attributes: attribute[];
  constructors: method[];
  methods: method[];
  data: data;
}

interface attribute {
  modifiers: modifiers;
  name: string;
  initial_value: any;
}

interface method {
  modifiers: modifiers;
  name: string;
  arity: number;
  arguments: var[];
  body: body;
}

interface instance {
  reference?: reference;
  class: reference;
  data: data;
}

interface modifiers {
  visibility: public | protected | private;
  scope: prototype | static;
}

interface data {
  attributes: Map<string, any>;
}

interface context {
  binding: reference;
  predicates: method[];
  facts: fact[];
  exports: atom[];
  imports: atom[];
}
