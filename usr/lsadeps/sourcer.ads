-- List of ada source files parsed
with Ada.Strings.Unbounded;
with Argument_Parser, Unique_List;
package Sourcer is

  -- Ada unbounded strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;

  -- Kind of ada source
  type Src_Kind_List is (Unit_Spec, Unit_Body, Subunit);

  -- Separator of units
  Separator : constant Character := '@';

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
    -- List of withed units - @unit@unit...@unit@
    Witheds : Asu_Us;
    -- List of subunits (if Body or subunit)  - @unit@unit...@unit@
    Subunits : Asu_Us;
  end record;

  -- Unique list of parsed source descriptors
  type Src_Dscr_Access is access all Src_Dscr;
  procedure Set (To : out Src_Dscr; Val : in Src_Dscr);
  function "=" (Current : Src_Dscr; Criteria : Src_Dscr) return Boolean;
  function Image (Element : Src_Dscr) return String;
  package Src_List_Mng is new Unique_List (Src_Dscr, Src_Dscr_Access,
                                           Set, "=" , Image);
  List : Src_List_Mng.List_Type;

  -- Parse sources and build list
  -- Reports errors on stderr and raises Error
  Error_Raised : exception;
  procedure Build_List (Args : in Argument_Parser.Parsed_Dscr);

  -- Some utilities
  -- Does a unit name contain a '.'
  function Has_Dot (Unit : in Asu_Us) return Boolean;

end Sourcer;

