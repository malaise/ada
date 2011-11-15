-- Definition of a property: (name, value)
with As.U, Any_Def;
package Property_Def is

  -- A property
  type Property is record
    Name : As.U.Asu_Us;
    Value : Any_Def.Any;
  end record;

  -- Vector of Property names
  type Property_Names is array (Positive range <>) of As.U.Asu_Us;

  -- Vector of Properties
  type Properties is array (Positive range <>) of Property;

end Property_Def;

