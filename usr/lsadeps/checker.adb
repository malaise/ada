with Basic_Proc, As.U, Str_Util, Parser, Trace.Loggers, Long_Long_Limited_List;
with Sourcer;
package body Checker is

  -- A logger
  Logger : Trace.Loggers.Logger;

  -- To sort the units in crescent order
  procedure Set (To : out Sourcer.Src_Dscr; Val : in Sourcer.Src_Dscr) is
  begin
    To := Val;
  end Set;
  package Src_List_Mng is new Long_Long_Limited_List (Sourcer.Src_Dscr, Set);

  -- A dyn list of Sourcer dscrs
  function "<" (Left, Right : Sourcer.Src_Dscr) return Boolean is
  begin
    return Sourcer.Short_Image (Left) < Sourcer.Short_Image (Right);
  end "<";
  procedure Sort is new Src_List_Mng.Sort ("<");

  -- Separator of Withs
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Sourcer.Separator;
  end Is_Sep;

  -- Restricted mode of unit (in @mode#Unit@...)
  function Restricted_Of (Unit : String; Within : As.U.Asu_Us)
                         return Character is
    Rwith : constant String
          := Sourcer.Restr_Separator & Unit & Sourcer.Separator;
    Index : Natural;
  begin
    -- Locate "Unit@"
    Index := Str_Util.Locate (Within.Image, Rwith);
    -- Mode is just before
    return (if Index = 0 then '-'
            else Within.Element (Index - 1));
  end Restricted_Of;

  -- A unit is "private" if it is a body or subunit, or if it is a
  -- private child spec
  function Is_Private (Unit : Sourcer.Src_Dscr) return Boolean is
    use type Sourcer.Src_Kind_List;
  begin
    return Unit.Kind /= Sourcer.Unit_Spec
        or else Unit.Private_Child;
  end Is_Private;

  -- Check that current unit withes a parent less restrictively than a child
  -- of this parent
  function Check_Curr_Restr (Parent, Child : Character) return Boolean is
  begin
    if Child = '-' or else Parent = 'B' or else Child = Parent then
      -- Child is withed not restricted or Parent is limited private withed
      --  so withing the parent is useless
      return False;
    elsif Parent = 'L' then
      -- Ok if child is not limited [ private ]
      return Child = 'P';
    elsif Parent = 'P' then
      -- Ok if child is not [ limited ] private
      return Child = 'L';
    else -- Parent = '-'
      return True;
    end if;
  end Check_Curr_Restr;

  -- Check that a unit is withed by current parent more restrictively than by a
  --  child of current parent
  -- Consider the fact that child can be a private child package
  function Check_Restr (Parent, Child : Character;
                        Private_Child : Boolean) return Boolean is
  begin
    if Parent = '-' or else Child = 'B' or else Child = Parent then
      -- Parent is unrestricted wihting, or child is limited private withing,
      -- so child is more restrictive or similar
      return False;
    elsif Parent = 'B' then
      -- Parent is limited private withing and not child => OK
      return  True;
    elsif Parent = 'L' then
      -- Ok if child is not limited [ private ] withing
      return Child /= 'L';
    else -- Parent = 'P'
      -- Ok if child is not [ limited ] private withing
      --  if it is [ private ] withing it must not be a private child
      return Child = 'L' or else not Private_Child;
    end if;
  end Check_Restr;

  -- Check that this parent doesn't already with this withed unit or a child
  --  of it, or withes it consistently with restriction (check that unit is
  --  withed by current more restrictively than the child by parent of current,
  --  considering that the child can be a private child package)
  function Check_Parent (Current : Sourcer.Src_Dscr;
                         Unit : String;
                         Unit_Restr : Character;
                         Parent : Sourcer.Src_Dscr) return Boolean is
    Index, Stop : Natural;
    Awith : constant String := Sourcer.Separator & Unit & Sourcer.Separator;
    Pwith : constant String := Sourcer.Separator & Unit & ".";
    Current_Is_Private : constant Boolean := Is_Private (Current);
    Withed : As.U.Asu_Us;
    Parent_Restr : Character;
    Result : Boolean := True;
  begin
    -- Check if parent withes Unit
    if Str_Util.Locate (Parent.Witheds.Image, Awith) /= 0 then
      -- This parent withes the same unit
      Parent_Restr := Restricted_Of (Unit, Parent.Restr_Witheds);
      Logger.Log_Debug ("    Our parent " & Sourcer.Short_Image (Parent)
                      & " withes unit " & Parent_Restr);
      if not Check_Restr (Parent_Restr, Unit_Restr, Current_Is_Private) then
        Basic_Proc.Put_Line_Output ("Unit " & Sourcer.Short_Image (Current)
            & " withes " & Unit
            & " already withed by parent " & Sourcer.Short_Image (Parent));
        Result := False;
      end if;
    -- Check if parent withese a child of Unit
    elsif Str_Util.Locate (Parent.Witheds_Parents.Image, Awith) /= 0 then
      -- Check that each with of a child of this unit does not make this useless
      Logger.Log_Debug ("      Our parent " & Sourcer.Short_Image (Parent)
                      & " withes a parent of " & Unit);
      Index := 1;
      loop
        -- Find unit as parent of with: "@Unit."
        Index := Str_Util.Locate (Parent.Witheds.Image, Pwith, Index);
        exit when Index = 0;
        -- Locate terminating separator '@' and set the Withed name
        Stop := Str_Util.Locate (Parent.Witheds.Image,
                                 Sourcer.Separator & "",
                                 From_Index => Index + Pwith'Length);
        Withed := Parent.Witheds.Uslice (Index + 1, Stop - 1);
        Parent_Restr := Restricted_Of (Withed.Image, Parent.Restr_Witheds);
        Logger.Log_Debug ("      Our parent " & Sourcer.Short_Image (Parent)
                        & " withes unit " & Withed.Image & " " & Parent_Restr);
        if not Check_Restr (Parent_Restr, Unit_Restr, Current_Is_Private) then
          Basic_Proc.Put_Line_Output ("Unit " & Sourcer.Short_Image (Current)
              & " withes " & Unit
              & " while its parent " & Sourcer.Short_Image (Parent)
              & " withes " & Withed.Image);
          Result := False;
        end if;
        Index := Stop;
      end loop;
    end if;
    return Result;
  end Check_Parent;


  -- Check redundant "with" and "use" clauses between spec/body/subunits
  --  within the whole list of units
  -- Return True if no redundance
  function Check return Boolean is
    Currents : Src_List_Mng.List_Type;
    Result : Boolean := True;
    Current : Sourcer.Src_Dscr;
    Moved, Parent_Moved : Boolean;
    Parents : Src_List_Mng.List_Type;
    Parent : Sourcer.Src_Dscr;
    Iter : Parser.Iterator;
    use type Sourcer.Src_Kind_List;
  begin
    Logger.Init ("Check");
    if Sourcer.List.Is_Empty then
      Logger.Log_Debug ("No source");
      return True;
    end if;
    -- Copy all units in a dynamic list and sort it
    Sourcer.List.Rewind;
    loop
      Sourcer.List.Read_Next (Current, Moved);
      Currents.Insert (Current);
      exit when not Moved;
    end loop;
    Sort (Currents);

    -- Scan all units. For each:
    Currents.Rewind;
    Check_All_Units: loop
      Currents.Read (Current, Moved => Moved);
      Logger.Log_Debug ("Processing Unit " & Sourcer.Short_Image (Current));
      -- For a body (not standalone) or subunit, make the list of parents

      -- Make a list of its parents
      Parents.Delete_List;
      if Current.Kind /= Sourcer.Unit_Spec
      and then not (Current.Kind = Sourcer.Unit_Body
                    and then Current.Standalone) then
        Parent := Current;
        Store_All_Parents: loop
          Parent := Sourcer.Get_Parent (Parent);
          Parents.Insert (Parent);
          Logger.Log_Debug ("  Adding parent " & Sourcer.Short_Image (Parent));
          exit Store_All_Parents when
              Parent.Kind = Sourcer.Unit_Spec
              or else (Parent.Kind = Sourcer.Unit_Body
                       and then Parent.Standalone);
        end loop Store_All_Parents;
      else
        Parent := Current;
      end if;
      -- Now parent is the spec or standalone body of current
      -- Store its parents if it is a child
      loop
        Parent := Sourcer.Get_Parent_Of_Child (Parent);
        exit when Parent.Unit.Is_Null;
        Parents.Insert (Parent);
        Logger.Log_Debug ("  Adding parent " & Sourcer.Short_Image (Parent));
      end loop;
      Sort (Parents);

      -- Extract each with of current unit
      Iter.Set (Current.Witheds.Image, Is_Sep'Access);
      Check_All_Withs: loop
        One_With: declare
          Unit : constant String := Iter.Next_Word;
          Awith : constant String
                := Sourcer.Separator & Unit & Sourcer.Separator;
          Pwith : constant String
                := Sourcer.Separator & Unit & ".";
          Occ : Positive;
          Index, Stop : Natural;
          Curr_Restr, Child_Restr : Character;
          Child : As.U.Asu_Us;
        begin
          exit Check_All_Withs when Unit = "";
          Logger.Log_Debug ("  Checking withed unit " & Unit);

          -- Check that there is no duplicated with in Witheds
          if Str_Util.Locate (Current.Witheds.Image, Awith,
                                Occurence => 2) /= 0 then
            -- Full unit appears twice in list of withed
            Basic_Proc.Put_Line_Output ("Unit " & Sourcer.Short_Image (Current)
              & " withes twice " & Unit);
            Result := False;
          end if;
          -- Check how we with children of Unit
          -- If current unit withes a unit U and a child unit of U, check
          --  that this is consistent regarding restricted with (with of U
          --   is less restrictive than with of U.Child)
          -- Get resctricted with mode of current
          Curr_Restr := Restricted_Of (Unit, Current.Restr_Witheds);
          Logger.Log_Debug ("  Checking withed children of unit " & Unit
                          &", withed " & Curr_Restr);
          Occ := 1;
          loop
            Index := Str_Util.Locate (Current.Witheds.Image, Pwith,
                                      Occurence => Occ);
            exit when Index = 0;
            -- Unit appears as part: a child is also withed
            Stop := Str_Util.Locate (Current.Witheds.Image,
                                     Sourcer.Separator & "",
                                     From_Index => Index + 1);
            Child := Current.Witheds.Uslice (Index + 1, Stop - 1);
            Child_Restr := Restricted_Of (Child.Image, Current.Restr_Witheds);
            Logger.Log_Debug ("    Child " & Child.Image
                            & " of withed unit is withed " & Child_Restr);
            -- Check that withing both is usefull, considering restrictive with
            if not Check_Curr_Restr (Curr_Restr, Child_Restr) then
              Basic_Proc.Put_Line_Output ("Unit "
                & Sourcer.Short_Image (Current)
                & " withes " & Unit & " and " & Child.Image);
              Result := False;
            end if;
            -- Next child
            Occ := Occ + 1;
          end loop;

          -- If there is no local "use", check that parents don't already with
          --  this withed unit or one of its children, or with it consistently
          --  with restriction
          if Str_Util.Locate (Current.Useds.Image, Awith) = 0
          and then not Parents.Is_Empty then
            Parents.Rewind;
            Check_All_Parents: loop
              Parents.Read (Parent, Moved => Parent_Moved);
              -- Check if this parent withes this unit or a child of it
              if not Check_Parent (Current, Unit, Curr_Restr, Parent) then
                Result := False;
              end if;
              exit Check_All_Parents when not Parent_Moved;
            end loop Check_All_Parents;
          end if;

        end One_With;
      end loop Check_All_Withs;
      exit Check_All_Units when not Moved;
    end loop Check_All_Units;
    return Result;
  end Check;

end Checker;

