-- List of ada source files parsed
with Ada.Strings.Unbounded;
with Argument_Parser, Dynamic_List;
package Sourcer is

  -- Ada unbounded strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;

  -- Kind of ada source
  type Src_Kind_List is (Unit_Spec, Unit_Body, Child_Spec, Child_Body, Subunit);

  -- Code of an ada source US/UB/CS/CB/SU
  subtype Src_Code is String (1 .. 2);
  Src_Codes : constant array (Src_Kind_List) of Src_Code
           := ("US", "UB", "CS", "CB", "SU");

  -- A parsed source descriptor
  type Src_Dscr is record
    -- Kind of unit in file
    Kind : Src_Kind_List;
    -- Full unit name - MixedStr
    Unit : Asu_Us;
    -- Full file path name
    File : Asu_Us;
    -- Standalone indicator: spec without body or body without spec
    Standalone : Boolean;
    -- Unit name of parent (if Child or subunit)
    Parent : Asu_Us;
    -- List of withed units - @unit@unit...@unit@"
    Witheds : Asu_Us;
  end record;

  -- List of parsed source descriptors
  package Src_Dyn_List_Mng is new Dynamic_List (Src_Dscr);
  package Src_List_Mng renames Src_Dyn_List_Mng.Dyn_List;
  List : Src_List_Mng.List_Type;

  -- Parse sources and build list
  -- Reports errors on stderr and raises Error
  Error : exception;
  procedure Build_List (Args : in Argument_Parser.Parsed_Dscr);

end Sourcer;

