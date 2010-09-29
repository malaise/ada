with As.U; use As.U;
with Any_Def;
package Property_Def is


  type Property is record
    Name : Asu_Us;
    Value : Any_Def.Any;
  end record;

  type Property_Names is array (Positive range <>) of Asu_Us;
  type Properties is array (Positive range <>) of Property;

end Property_Def;

