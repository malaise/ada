package Names is

  -- Separator for parsing
  Sep : constant Character := '.';
  Sep_Str : constant String := Sep & "";
  function Is_Sep (C : Character) return Boolean;


  -- Syntax of name for Get/Set
  --   <ident> [ { .<ident> } ]
  function Is_Valid_Name (Name : String) return Boolean;


  -- Syntax of name for (Un) Notify is
  --   <ident_or_anyid_or_anything> [ { .<ident_or_anyid> } ] [ . <anything> ]
  --   anyid is "*" and matches any identifier
  --   anything is "**" and matches any sequence of identifiers (even empty)
  -- No pattern matching between notify and un-notify names,
  --   they have to be the same.
  -- May raise Invalid_Name or Name_Too_Long
  function Is_Valid_Notify (Criteria : String) return Boolean;


  -- Do item match criteria
  function Match (Name, Criteria : String) return Boolean;

end Names;

