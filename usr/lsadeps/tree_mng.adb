with As.U, Dynamic_List, Hashed_List.Unique, Str_Util, Basic_Proc, Parser,
     Images;
with Debug;
package body Tree_Mng is

  use type Sourcer.Src_Kind_List;

  -- Error
  Error_Raised : exception renames Sourcer.Error_Raised;
  procedure Error (Msg : in String) is
  begin
    Debug.Logger.Log_Fatal ("INTERNAL ERROR: " & Msg & ".");
    raise Error_Raised;
  end Error;

  -- Do we build full tree or do we optimize (each entry only once)
  --  or do we do only first level
  type Tree_Kind_List is (Full_Tree, Shortest_Way, Optimized, First_Level);
  Tree_Kind : Tree_Kind_List;

  -- Do we display files or units
  File_Mode : Boolean;

  -- Do we handle restricted with (limited / private with)
  Restrict_Mode : Boolean;

  -- Optimization: unique list of (<Path><File>, Level)
  type Cell is record
    Name : As.U.Asu_Us;
    Level : Positive;
  end record;
  type Cell_Access is access all Cell;
  procedure Set (To : out Cell; Val : in Cell) is
  begin
    To := Val;
  end Set;
  function "=" (Current : Cell; Criteria : Cell) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";
  function Key_Image (A_Cell : Cell) return String is
  begin
    return A_Cell.Name.Image;
  end Key_Image;
  package Cell_Hashed_List_Mng is
      new Hashed_List (Cell, Cell_Access, Set, "=", Key_Image);
  package Cell_Unique_List_Mng is new Cell_Hashed_List_Mng.Unique;
  Got_List : Cell_Unique_List_Mng.Unique_List_Type;

  -- List of direct With from first Origin to current
  Rope : Sourcer.Src_List_Mng.Unique_List_Type;

  -- For parsing list of With / subunits
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Sourcer.Separator;
  end Is_Sep;

  -- Build a node
  procedure Build_Node (Level : in Positive;
                        Origin : in Sourcer.Src_Dscr;
                        Specs_Mode, Revert_Mode,
                        Bodies_Mode : in Boolean);

  -- Is Name in the restricted with
  function Is_Restricted_With (Name, Restr_Witheds : String) return Boolean is
  begin
    -- True if #Name@ appears in Restr_Witheds
    return Str_Util.Locate (Restr_Witheds,
          Sourcer.Restr_Separator & Name & Sourcer.Separator) /= 0;
  end Is_Restricted_With;

  -- Build the nodes with any unit withed in List,
  -- For each unit: select unit in Path if it exists,
  --  otherwise look everywhere and add all the units found
  procedure Build_Witheds (Level : in Positive;
                           Path, Witheds, Restr_Witheds : in As.U.Asu_Us;
                           Specs_Mode : in Boolean) is
    Iter1 : Parser.Iterator;
    Name : Sourcer.Name_Dscr;

    -- Find and insert units with name = Name.Unit
    procedure Find_Units is
      Found : Boolean;
      Iter2 : Parser.Iterator;
      Src : Sourcer.Src_Dscr;
    begin
      -- See if a local unit exists
      Src := Sourcer.Get_Unit (Path, Name.Unit);
      if not Src.Unit.Is_Null then
        -- Yes, local unit hides remote ones
        Build_Node (Level, Src, Specs_Mode, False, False);
        return;
      end if;
      -- List all (remote) units with this withed name
      Sourcer.Name_List.Search (Name, Found);
      if not Found then
        -- This unit name is not known at all => skip it
        return;
      end if;
      Sourcer.Name_List.Read (Name);
      -- Parse list of paths with Parser and find each Path
      Iter2.Set (Name.Paths.Image, Is_Sep'Access);
      loop
        Src.Path := As.U.Tus (Iter2.Next_Word);
        Src.Kind := Sourcer.Unit_Spec;
        Src.Unit := Name.Unit;
        Sourcer.List.Search (Src, Found);
        if Found then
          -- Got a spec
          Sourcer.List.Read (Src);
        else
          Src.Kind := Sourcer.Unit_Body;
          Sourcer.List.Search (Src, Found);
          if not Found then
            -- Likely a standalone spec
            return;
          end if;
          -- Found a body, read it to raise ERROR if not standalone
          Sourcer.List.Read (Src);
          if not Src.Standalone then
            -- Found a body without spec and not standalone
            raise Sourcer.Src_List_Mng.Not_In_List;
          end if;
        end if;
        Build_Node (Level, Src, Specs_Mode, False, False);
      end loop;
    end Find_Units;

  begin
    if Witheds.Is_Null then
      return;
    end if;
    -- Parse list with Parser and find each name
    Iter1.Set (Witheds.Image, Is_Sep'Access);
    loop
      Name.Unit := As.U.Tus (Iter1.Next_Word);
      exit when Name.Unit.Is_Null;
      if not Restrict_Mode
      or else not Is_Restricted_With (Name.Unit.Image, Restr_Witheds.Image) then
        -- Not handling restricted with, or this unit is not restricted
        Find_Units;
      end if;
    end loop;
  end Build_Witheds;

  -- Build the subunits of Units (with same path)
  -- Else any unit withed in Units, whatever Path
  procedure Build_Subunits (Level : in Positive;
                            Units : in As.U.Asu_Us;
                            Path : in As.U.Asu_Us;
                            Specs_Mode, Revert_Mode,
                            Bodies_Mode : in Boolean) is
    Iter : Parser.Iterator;
    Crit : Sourcer.Src_Dscr;
  begin
    if Units.Is_Null then
      return;
    end if;
    -- Parse Units list with Parser and insert nodes
    Iter.Set (Units.Image, Is_Sep'Access);
    loop
      Crit.Unit := As.U.Tus (Iter.Next_Word);
      exit when Crit.Unit.Is_Null;
      Crit.Kind := Sourcer.Subunit;
      Crit.Path := Path;
      Sourcer.List.Read (Crit);
      Build_Node (Level, Crit, Specs_Mode, Revert_Mode, Bodies_Mode);
    end loop;
  end Build_Subunits;

  -- Get a spec or else a body of unit Name withing Withed
  function Get_Wither (Name, Withing : in String) return Sourcer.Src_Dscr is
    Crit : constant String := Sourcer.Separator & Name & Sourcer.Separator;
    Dscr : Sourcer.Src_Dscr;
    function Withes  return Boolean is
    begin
      return Str_Util.Locate (Dscr.Witheds.Image, Crit) /= 0
      or else Str_Util.Locate (Dscr.Witheds_Parents.Image, Crit) /= 0;
    end Withes;

  begin
    Dscr := Sourcer.Get_Unit (As.U.Tus (Withing));
    -- This unit shall be found
    if Dscr.Unit.Is_Null then
      Basic_Proc.Put_Line_Error ("INTERNAL ERROR: with " & Withing & ".");
      raise Sourcer.Src_List_Mng.Not_In_List;
    end if;
    if Withes then
      -- Spec withese us
      return Dscr;
    end if;
    -- Read body (it must exist)
    Dscr.Kind := Sourcer.Unit_Body;
    Sourcer.List.Read (Dscr);
    if not Withes then
      -- One of them shall with us
      Basic_Proc.Put_Line_Error ("INTERNAL ERROR: with " & Withing & ".");
      raise Sourcer.Src_List_Mng.Not_In_List;
    end if;
    return Dscr;
  end Get_Wither;

  -- Build wihing units (in revert mode)
  -- The temporary local dynamic list is necessary because
  --  here we call Build_Node, which itself calls us recursively
  package Src_List_Mng is new Dynamic_List (Sourcer.Src_Dscr);
  package Src_Dyn_List_Mng renames Src_List_Mng.Dyn_List;

  procedure Build_Withings (Level : in Positive;
                            Path, Name : in String;
                            Bodies_Mode : in Boolean) is
    Withing : Sourcer.Withing_Dscr;
    Found : Boolean;
    Iter : Parser.Iterator;

    Dscr : Sourcer.Src_Dscr;
    Hidding : Sourcer.Src_Dscr;

    Src_List : Src_Dyn_List_Mng.List_Type;
    Moved : Boolean;
    use type As.U.Asu_Us;

  begin
    -- Search ourself in list of withings cross references
    Withing.Unit := As.U.Tus (Name);
    Sourcer.Withing_List.Search (Withing, Found);
    if not Found then
      -- Nobody is withing us :-(
      return;
    end if;
    Sourcer.Withing_List.Read (Withing);

    -- Scan all withing units
    Iter.Set (Withing.Withings.Image, Is_Sep'Access);
    loop
      declare
        -- Path/Name of the withing unit
        Str : constant String := Iter.Next_Word;
      begin
        exit when Str = "";
        -- Get spec or body withing us
        Dscr := Get_Wither (Name, Str);
        -- Skip restricted with if requested
        if not Restrict_Mode
        or else not Is_Restricted_With (Name, Dscr.Restr_Witheds.Image) then
          -- See if it is local
          if Dscr.Path = Path then
            -- Yesss, add it to our list
            Src_List.Insert (Dscr);
          else
            -- We are withed by a remote unit, see if we are hidden
            -- Search a unit (spec or standalone body) in Dscr Path
            --  with our name
            Hidding := Sourcer.Get_Unit (Dscr.Path, As.U.Tus (Name));
            if Hidding.Unit.Is_Null then
              -- We are not hidden, add it to our list
              Src_List.Insert (Dscr);
            end if;
          end if;
        end if;
      end;
    end loop;

    -- Insert in the tree each node of the list
    if not Src_List.Is_Empty then
      Src_List.Rewind;
      loop
        Src_List.Read (Dscr, Moved => Moved);
        Build_Node (Level, Dscr, True, True, Bodies_Mode);
        exit when not Moved;
      end loop;
    end if;
  end Build_Withings;

  -- Build children from list of children
  procedure Build_Children (Level : in Positive;
                            Path, Children : in As.U.Asu_Us;
                            Bodies_Mode : in Boolean) is
    Iter : Parser.Iterator;
    Dscr : Sourcer.Src_Dscr;
    Child_List : Src_Dyn_List_Mng.List_Type;
    Moved : Boolean;
    use type As.U.Asu_Us;

  begin
    -- Scan all children units
    Iter.Set (Children.Image, Is_Sep'Access);
    loop
      declare
        -- Path/Name of the child unit
        Str : constant String := Iter.Next_Word;
      begin
        exit when Str = "";
        -- Get spec of child
        Dscr := Sourcer.Get_Unit (Path, As.U.Tus (Str));
        if Dscr.Unit.Is_Null then
          Basic_Proc.Put_Line_Error ("INTERNAL ERROR: with " & Str & ".");
          raise Sourcer.Src_List_Mng.Not_In_List;
        end if;
        Child_List.Insert (Dscr);
      end;
    end loop;

    -- Insert in the tree each node of the list
    if not Child_List.Is_Empty then
      Child_List.Rewind;
      loop
        Child_List.Read (Dscr, Moved => Moved);
        Build_Node (Level, Dscr, True, True, Bodies_Mode);
        exit when not Moved;
      end loop;
    end if;
  end Build_Children;

  -- Build a node
  procedure Build_Node (Level : in Positive;
                        Origin : in Sourcer.Src_Dscr;
                        Specs_Mode,  Revert_Mode,
                        Bodies_Mode : in Boolean) is
    -- Clean rope and move up after processing node
    procedure Done is
    begin
      if Tree.Has_Father then
        Tree.Move_Father;
        Rope.Delete (Origin);
      end if;
    end Done;

    Found : Boolean;
    Child : Sourcer.Src_Dscr;
    Kind : As.U.Asu_Us;
    Crit : Cell;
    -- Don't increase level of body and subunits in units mode
    Sub_Level : constant Positive
              := (if File_Mode then Level + 1 else Level);
    use type As.U.Asu_Us;
  begin
    -- Insert ourself
    if Tree.Is_Empty then
      -- First node
      Tree.Insert_Father ((Origin, Looping => False));
      Rope.Insert (Origin);
    else
      if Tree_Kind = Optimized
      or else Tree_Kind = Shortest_Way then
        -- Optim: locate and store (<path><file>, Level)
        Crit.Name := Origin.Path & Origin.File;
        Crit.Level := 1;
        Got_List.Search (Crit, Found);
        if Found then
          if Tree_Kind = Optimized then
            -- This unit already exists
            Debug.Logger.Log_Debug ("Optim skip " & Crit.Name.Image);
            return;
          elsif Got_List.Get_Access_Current.Level < Level then
            -- This unit already exists at lower level
            if Debug.Logger.Debug_On then
              Debug.Logger.Log_Debug (
                  "Optim skip " & Crit.Name.Image
                & " " & Images.Integer_Image (Level)
                & " cause found "
                & Images.Integer_Image (Got_List.Get_Access_Current.Level));
            end if;
            return;
          end if;
        end if;
        Debug.Logger.Log_Debug ("Optim insert " & Crit.Name.Image
                              & " " & Images.Integer_Image (Level));
        Got_List.Insert ( (Crit.Name, Level) );
      end if;
      Rope.Search (Origin, Found);
      if Found then
        -- Current Origin already exists => Looping
        Tree.Insert_Child ((Origin, Looping => True));
        Tree.Move_Father;
        return;
      else
        -- Normal insertion of a new node
        Tree.Insert_Child ((Origin, Looping => False));
        Rope.Insert (Origin);
      end if;
      -- Done if not root and only first level requested
      if Tree_Kind = First_Level then
        Done;
        return;
      end if;
    end if;

    -- Any unit: Insert withed
    -- In revert: Insert units withing us if we are a spec or standalone body
    --            Otherwise they are not dependant on us
    --            except in Bodies_Mode
    if not Revert_Mode then
      Kind :=  As.U.Tus ("withed");
      Build_Witheds (Level + 1, Origin.Path, Origin.Witheds,
                     Origin.Restr_Witheds, Specs_Mode);
    elsif Bodies_Mode
          or else Origin.Kind = Sourcer.Unit_Spec
          or else (Origin.Kind = Sourcer.Unit_Body
                   and then Origin.Standalone) then
      Kind :=  As.U.Tus ("withing");
      Build_Withings (Level + 1, Origin.Path.Image, Origin.Unit.Image,
                      Bodies_Mode);
      -- Also our children are dependent from us
      Build_Children (Level + 1, Origin.Path, Origin.Children,
                      Bodies_Mode);
    end if;

    -- A Child (spec or standalone body): Insert parent spec if not revert
    if not Revert_Mode
    and then (Origin.Kind = Sourcer.Unit_Spec
       or else (Origin.Kind = Sourcer.Unit_Body and then Origin.Standalone) )
    and then Sourcer.Has_Dot (Origin.Unit) then
      Kind := As.U.Tus ("parent");
      Child.Unit := Origin.Parent;
      Child.Kind := Sourcer.Unit_Spec;
      Child.Path := Origin.Path;
      Sourcer.List.Read (Child);
      Build_Node (Level + 1, Child, Specs_Mode, Revert_Mode, Bodies_Mode);
    end if;

    -- Insert body of spec
    if Revert_Mode or else not Specs_Mode then
      Kind := As.U.Tus ("body");
      -- A spec with body: Insert body
      if Origin.Kind = Sourcer.Unit_Spec
      and then not Origin.Standalone then
        Child.Unit := Origin.Unit;
        Child.Kind := Sourcer.Unit_Body;
        Child.Path := Origin.Path;
        Sourcer.List.Read (Child);
        Build_Node (Sub_Level, Child, Specs_Mode, Revert_Mode, Bodies_Mode);
      end if;

      -- A Body or subunit: Insert subunits
      if Origin.Kind = Sourcer.Unit_Body
        or else Origin.Kind = Sourcer.Subunit then
        Kind := As.U.Tus ("subunit");
        Build_Subunits (Sub_Level, Origin.Subunits, Origin.Path, Specs_Mode,
                        Revert_Mode, Bodies_Mode);
      end if;
    end if;

    -- Done, move up
    Done;
  exception
    when Sourcer.Src_List_Mng.Not_In_List =>
      Error ("Cannot find " & Kind.Image & " of " & Sourcer.Image (Origin));
  end Build_Node;

  -- Dump one element of the tree
  function Dump_One (Dscr : in out Src_Dscr;
                     Level : in Natural) return Trees.Iteration_Policy is
    Str : As.U.Asu_Us;
  begin
    for I in 1 .. Level loop
      Str.Append ("  ");
    end loop;
    Str.Append (Sourcer.Image (Dscr.Dscr));
    if Dscr.Looping then
      Str.Append (" -->X");
    end if;
    Debug.Logger.Log_Debug (Str.Image);
    return Trees.Go_On;
  end Dump_One;

  -- Build the tree of source dependencies of Origin
  procedure Build (Origin : in Sourcer.Src_Dscr;
                   Specs_Mode, Revert_Mode,
                   Tree_Mode, Shortest_Mode,
                   File_Mode, Direct_Mode,
                   Bodies_Mode, Restrict_Mode : in Boolean) is
  begin
    -- Global (constants) file and restrict modes
    Standard.Tree_Mng.File_Mode := File_Mode;
    Standard.Tree_Mng.Restrict_Mode := Restrict_Mode;
    -- Full tree, optimized, or first level
    case Tree_Mode is
      when True =>
        case Direct_Mode is
          when True =>
            Error ("Cannot do direct mode with full tree");
          when False =>
            case Shortest_Mode is
              when True =>
                Tree_Kind := Shortest_Way;
              when False =>
                Tree_Kind := Full_Tree;
            end case;
        end case;
      when False =>
        if Shortest_Mode then
          Error ("Cannot do shortest mode without tree");
        end if;
        case Direct_Mode is
          when True =>
            Tree_Kind := First_Level;
          when False =>
            Tree_Kind := Optimized;
        end case;
    end case;
    -- Clean tree
    if not Tree.Is_Empty then
      Tree.Move_Root;
      Tree.Delete_Tree;
   end if;
    -- Build tree
    Build_Node (1, Origin, Specs_Mode, Revert_Mode, Bodies_Mode);
    Debug.Logger.Log_Debug ("Dumping tree:");
    -- Dump debug
    if Debug.Logger.Debug_On then
      Tree.Iterate (Dump_One'Access, False);
    end if;
  end Build;

end Tree_Mng;

