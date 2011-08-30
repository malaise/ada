-- List of ada source files parsed
with As.U, Hashed_List.Unique;
package Sourcer is

  -- Kind of ada source
  type Src_Kind_List is (Unit_Spec, Unit_Body, Subunit);

  -- Separator of units
  Separator : constant Character := '@';

  -- A parsed source descriptor
  type Src_Dscr is record
    -- Kind of unit in file
    Kind : Src_Kind_List := Unit_Spec;
    -- Full unit name - MixedStr
    Unit : As.U.Asu_Us;
    -- Full path of the unit (with trailing /)
    Path : As.U.Asu_Us;
    -- Full file path name
    File : As.U.Asu_Us;
    -- Standalone indicator: spec without body or body without spec
    Standalone : Boolean := False;
    -- Unit name of parent (if Child or subunit)
    Parent : As.U.Asu_Us;
    -- List of withed units - @unit@unit...@unit@
    Witheds : As.U.Asu_Us;
    -- List of ancestors of withed units (if any) - @unit@unit...@unit@
    Witheds_Parents : As.U.Asu_Us;
    -- List of used units - @unit@unit...@unit@
    Useds : As.U.Asu_Us;
    -- List of subunits (if Body or subunit) - @unit@unit...@unit@
    Subunits : As.U.Asu_Us;
  end record;

  -- Unique list of parsed source descriptors (Kind, Unit and Path)
  type Src_Dscr_Access is access all Src_Dscr;
  procedure Set (To : out Src_Dscr; Val : in Src_Dscr);
  function "=" (Current : Src_Dscr; Criteria : Src_Dscr) return Boolean;
  function Image (Element : Src_Dscr) return String;
  package H_Src_List_Mng is new Hashed_List (Src_Dscr, Src_Dscr_Access,
                                           Set, "=" , Image);
  package Src_List_Mng is new H_Src_List_Mng.Unique;
  List : Src_List_Mng.Unique_List_Type;


  -- A name descriptor
  type Name_Dscr is record
    -- Full unit name - MixedStr
    Unit : As.U.Asu_Us;
    -- List of paths where it existst - @path@path...@path@
    Paths : As.U.Asu_Us;
  end record;
  -- Unique list of parsed unit names (Unit)
  type Name_Dscr_Access is access all Name_Dscr;
  procedure Set (To : out Name_Dscr; Val : in Name_Dscr);
  function "=" (Current : Name_Dscr; Criteria : Name_Dscr) return Boolean;
  function Image (Element : Name_Dscr) return String;
  package H_Name_List_Mng is new Hashed_List (Name_Dscr, Name_Dscr_Access,
                                           Set, "=" , Image);
  package Name_List_Mng is new H_Name_List_Mng.Unique;
  Name_List : Name_List_Mng.Unique_List_Type;


  -- A withing descriptor
  type Withing_Dscr is record
    -- Full unit name of the withed unit
    Unit : As.U.Asu_Us;
    -- Withings - @path/unit@path/unit...@path/unit@
    Withings : As.U.Asu_Us;
  end record;
  type Withing_Dscr_Access is access all Withing_Dscr;
  procedure Set (To : out Withing_Dscr; Val : in Withing_Dscr);
  function "=" (Current : Withing_Dscr; Criteria : Withing_Dscr) return Boolean;
  function Image (Element : Withing_Dscr) return String;
  package H_Withing_List_Mng is new Hashed_List (Withing_Dscr,
                                           Withing_Dscr_Access,
                                           Set, "=" , Image);
  package Withing_List_Mng is new H_Withing_List_Mng.Unique;
  Withing_List : Withing_List_Mng.Unique_List_Type;



  -- Parse sources and build lists
  -- Reports errors on stderr and raises Error
  Error_Raised : exception;
  procedure Build_Lists;

  -- Some utilities
  -- Does a unit name contain a '.'
  function Has_Dot (Unit : in As.U.Asu_Us) return Boolean;

  -- Get parent of Dscr (body or subunit)
  -- Return Dscr itself if it is a spec or a standalone body
  function Get_Parent (Dscr : in Src_Dscr) return Src_Dscr;

  -- Get root Unit of a path/unit
  -- Return a spec or a standalone body or subunit
  -- Return a Dscr with empty Unit if not found
  function Get_Unit (Path, Unit : in As.U.Asu_Us) return Src_Dscr;
  function Get_Unit (Path_Unit : in As.U.Asu_Us) return Src_Dscr;

  -- Get Unit_Body of a subunit
  function Get_Body (Sub : in Src_Dscr) return Src_Dscr;

end Sourcer;

