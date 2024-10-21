
structure JSONSchema =
struct

datatype schema_type =
   NullType
 | BooleanType
 | ObjectType
 | ArrayType
 | NumberType
 | StringType

type regex = string

datatype schema_index = SchemaIndex of int

datatype applicator = Applicator of {
      (* Logic *)
      oneOf: schema_index list,
      allOf: schema_index list,
      anyOf: schema_index list,
      if_: schema_index option,
      then_: schema_index option,
      else_: schema_index option,
      not: schema_index option,
      (* Object *)
      properties: schema_index AtomMap.map,
      patternProperties: (regex * schema_index) list,
      propertyNames: schema_index option,
      additionalProperties: schema_index option,
      dependentSchemas: schema_index AtomMap.map,
      (* Array *)
      prefixItems: schema_index option,
      contains: schema_index option,
      items: schema_index option
   }

datatype validation = Validation of {
      (* Any *)
      type_: schema_type list,
      enum: JSON.value list option,
      const: JSON.value option,
      (* String *)
      minLength: int option,
      maxLength: int option,
      pattern: string option, (* Regex *)
      (* Number *)
      minimum: int option,
      maximum: int option,
      exclusiveMinimum: int option,
      exclusiveMaximum: int option,
      multipleOf: int option,
      (* Object *)
      dependentRequired: string list AtomMap.map,
      minProperties: int option,
      maxProperties: int option,
      required: string list,
      (* Array *)
      minItems: int option,
      maxItems: int option,
      minContains: int option,
      maxContains: int option,
      uniqueItems: bool
   }

datatype meta_data = MetaData of {
      title: string option,
      description: string option,
      default: JSON.value option
   }

datatype schema = Schema of {
      boolean: bool option,
      ref: schema_index option,
      rec_rec: schema_index option,
      rec_anchor: bool,
      dyn_ref: (schema_index * string option) option,
      dyn_anchor: string option,
      applicator: applicator,
      validation: validation,
      meta_data: meta_data
   }

end

(* vim: set tw=0 ts=3 sw=3: *)
