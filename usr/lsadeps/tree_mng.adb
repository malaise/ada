with As.U, String_Mng, Dynamic_List;
with Debug, Basic_Proc, Parser;
package body Tree_Mng is

  use type Sourcer.Src_Kind_List;

  -- Error
  Error_Raised : exception renames Sourcer.Error_Raised;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("INTERNAL ERROR: " & Msg & ".");
    raise Error_Raised;
  end Error;

  -- List of direct With from first Origin to current
  Rope : Sourcer.Src_List_Mng.Unique_List_Type;

  -- For parsing list of With / subunits
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Sourcer.Separator;
  end Is_Sep;

  -- Build a node
  procedure Build_Node (Origin : in Sourcer.Src_Dscr;
                        Revert : in Boolean);

  -- Build the nodes with any unit withed in List, whatever Path
  procedure Build_Witheds (List : in As.U.Asu_Us) is
    Iter1 : Parser.Iterator;
    Name : Sourcer.Name_Dscr;

    -- Find and insert units with name = Name.Unit
    procedure Find_Units is
      Found : Boolean;
      Iter2 : Parser.Iterator;
      Src : Sourcer.Src_Dscr;
    begin
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
        Build_Node (Src, False);
      end loop;
    end Find_Units;

  begin
    if List.Is_Null then
      return;
    end if;
    -- Parse list with Parser and find each name
    Iter1.Set (List.Image, Is_Sep'Access);
    loop
      Name.Unit := As.U.Tus (Iter1.Next_Word);
      exit when Name.Unit.Is_Null;
      Find_Units;
    end loop;
  end Build_Witheds;

  -- Build the subunits of List (with same path)
  -- Else any unit withed in List, whatever Path
  procedure Build_Subunits (List : in As.U.Asu_Us;
                            Path : in As.U.Asu_Us;
                            Revert : in Boolean) is
    Iter : Parser.Iterator;
    Crit : Sourcer.Src_Dscr;
  begin
    if List.Is_Null then
      return;
    end if;
    -- Parse list with Parser and insert nodes
    Iter.Set (List.Image, Is_Sep'Access);
    loop
      Crit.Unit := As.U.Tus (Iter.Next_Word);
      exit when Crit.Unit.Is_Null;
      Crit.Kind := Sourcer.Subunit;
      Crit.Path := Path;
      Sourcer.List.Read (Crit);
      Build_Node (Crit, Revert);
    end loop;
  end Build_Subunits;

  -- Build wihing units (in revert mode)
  -- The temporary local dynamic lists are necessary because
  --  here we call Build_Node, which itself calls us recursively
  package Src_List_Mng is new Dynamic_List (Sourcer.Src_Dscr);
  package Src_Dyn_List_Mng renames Src_List_Mng.Dyn_List;

  procedure Build_Withings (Name : in String) is
    Dscr : Sourcer.Src_Dscr;
    Moved : Boolean;
    Crit : constant String := Sourcer.Separator & Name & Sourcer.Separator;
    List : Src_Dyn_List_Mng.List_Type;
  begin
    -- Scan all known units
    Sourcer.List.Rewind;
    loop
      Sourcer.List.Read_Next (Dscr, Moved);
      -- See if its Witheds contains us
      if String_Mng.Locate (Dscr.Witheds.Image, Crit) /= 0
      or else String_Mng.Locate (Dscr.Witheds_Parents.Image, Crit) /= 0 then
        -- Yesss, add it to our list
        List.Insert (Dscr);
      end if;
      exit when not Moved;
    end loop;
    -- Insert in the tree each node of the list
    if not List.Is_Empty then
      List.Rewind;
      loop
        List.Read (Dscr, Moved => Moved);
        Build_Node (Dscr, True);
        exit when not Moved;
      end loop;
    end if;
  end Build_Withings;

  -- Build a node
  procedure Build_Node (Origin : in Sourcer.Src_Dscr; Revert : in Boolean) is
    Found : Boolean;
    Child : Sourcer.Src_Dscr;
    Kind : As.U.Asu_Us;
  begin
    -- Insert ourself
    if Tree.Is_Empty then
      -- First node
      Tree.Insert_Father ((Origin, False));
      Rope.Insert (Origin);
    else
      Rope.Search (Origin, Found);
      if Found then
        -- Current Origin already exists => Looping
        Tree.Insert_Child ((Origin, True));
        Tree.Move_Father;
        return;
      else
        -- Normal insertion of a new node
        Tree.Insert_Child ((Origin, False));
        Rope.Insert (Origin);
      end if;
    end if;

    -- Any unit: Insert withed
    -- In revert: Insert units withing us if we are a spec or standalone body
    --            Otherwise they are not dependant on us
    if not Revert then
      Kind :=  As.U.Tus ("withed");
      Build_Witheds (Origin.Witheds);
    elsif Origin.Kind = Sourcer.Unit_Spec
          or else (Origin.Kind = Sourcer.Unit_Body
                   and then Origin.Standalone) then
      Kind :=  As.U.Tus ("withing");
      Build_Withings (Origin.Unit.Image);
    end if;

    -- A Child (spec or standalone body): Insert parent spec if not revert
    if not Revert
    and then (Origin.Kind = Sourcer.Unit_Spec
       or else (Origin.Kind = Sourcer.Unit_Body and then Origin.Standalone) )
    and then Sourcer.Has_Dot (Origin.Unit) then
      Kind := As.U.Tus ("parent");
      Child.Unit := Origin.Parent;
      Child.Kind := Sourcer.Unit_Spec;
      Child.Path := Origin.Path;
      Sourcer.List.Read (Child);
      Build_Node (Child, Revert);
    end if;

    -- A spec: Insert body
    if Origin.Kind = Sourcer.Unit_Spec then
      Kind := As.U.Tus ("body");
      if not Origin.Standalone then
        Child.Unit := Origin.Unit;
        Child.Kind := Sourcer.Unit_Body;
        Child.Path := Origin.Path;
        Sourcer.List.Read (Child);
        Build_Node (Child, Revert);
      end if;
    end if;

    -- A Body or subunit: Insert subunits
    if Origin.Kind = Sourcer.Unit_Body
    or else Origin.Kind = Sourcer.Subunit then
      Kind := As.U.Tus ("subunit");
      Build_Subunits (Origin.Subunits, Origin.Path, Revert);
    end if;

    -- Done, move up
    if Tree.Has_Father then
      Tree.Move_Father;
      Rope.Delete (Origin);
    end if;
  exception
    when Sourcer.Src_List_Mng.Not_In_List =>
      Error ("Cannot find " & Kind.Image & " of " & Sourcer.Image (Origin));
  end Build_Node;

  -- Dump one element of the tree
  function Dump_One (Dscr : in Src_Dscr;
                     Level : in Natural) return Boolean is
    Str : As.U.Asu_Us;
  begin
    for I in 1 .. Level loop
      Str.Append ("  ");
    end loop;
    Str.Append (Sourcer.Image (Dscr.Dscr));
    if Dscr.Looping then
      Str.Append (" -->X");
    end if;
    Basic_Proc.Put_Line_Output (Str.Image);
    return True;
  end Dump_One;

  -- Build the tree of source dependencies of Origin
  procedure Build (Origin : in Sourcer.Src_Dscr; Revert : in Boolean) is
  begin
    Build_Node (Origin, Revert);
    if Debug.Is_Set then
      Basic_Proc.Put_Line_Output ("Dumping tree:");
      Tree.Iterate (Dump_One'Access, False);
    end if;
  end Build;

end Tree_Mng;

