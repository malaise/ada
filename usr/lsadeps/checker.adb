with Basic_Proc, String_Mng, Dynamic_List, Parser;
with Sourcer;
package body Checker is

  -- A dyn list of Sourcer dscrs
  package Src_List_Mng is new Dynamic_List (Sourcer.Src_Dscr);
  package Src_Dyn_List_Mng renames Src_List_Mng.Dyn_List;

  -- Separator of Withs
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Sourcer.Separator;
  end Is_Sep;

  function Check return Boolean is
    Result : Boolean := True;
    Current : Sourcer.Src_Dscr;
    Moved, Parent_Moved : Boolean;
    Parents : Src_Dyn_List_Mng.List_Type;
    Parent : Sourcer.Src_Dscr;
    Iter : Parser.Iterator;
    use type Sourcer.Src_Kind_List;
  begin
    if Sourcer.List.Is_Empty then
      return True;
    end if;
    -- Scan all units. For each:
    Sourcer.List.Rewind;
    Check_All_Units: loop
      Sourcer.List.Read_Next (Current, Moved);
      -- For a body (not standalone) or subunit, make the list of parents
      Parents.Delete_List;
      if Current.Kind /= Sourcer.Unit_Spec
      and then not (Current.Kind = Sourcer.Unit_Body
                    and then Current.Standalone) then
        Parent := Current;
        Store_All_Parents: loop
          Parent := Sourcer.Get_Parent (Parent);
          Parents.Insert (Parent);
          exit Store_All_Parents when
              Parent.Kind = Sourcer.Unit_Spec
              or else (Parent.Kind = Sourcer.Unit_Body
                       and then Parent.Standalone);
        end loop Store_All_Parents;
      end if;

      -- Extract each with of current unit
      Iter.Set (Current.Witheds.Image, Is_Sep'Access);
      Check_All_Withs: loop
        One_With: declare
          Word : constant String := Iter.Next_Word;
          Awith : constant String
                := Sourcer.Separator & Word & Sourcer.Separator;
          Pwith : constant String
                := Sourcer.Separator & Word & ".";
        begin
          exit Check_All_Withs when Word = "";

          -- Check that there is no duplicated with in Witheds
          if String_Mng.Locate (Current.Witheds.Image, Awith,
                                Occurence => 2) /= 0
          or else String_Mng.Locate (Current.Witheds.Image, Pwith,
                                Occurence => 1) /= 0 then
            Basic_Proc.Put_Line_Output ("Unit " & Sourcer.Image (Current)
              & " withes twice " & Word);
            Result := False;
          end if;

          -- Check that parents don't already with one of our with
          if not Parents.Is_Empty then
            Parents.Rewind;
            Check_All_Parents: loop
              Parents.Read (Parent, Moved => Parent_Moved);
              if (String_Mng.Locate (Parent.Witheds.Image, Awith) /= 0
                or else String_Mng.Locate (
                           Parent.Witheds_Parents.Image, Awith) /= 0)
              and then
                 String_Mng.Locate (Current.Useds.Image, Awith) = 0 then
                -- This local "with" appears in a parent
                --   and there is no local "use"
                Basic_Proc.Put_Line_Output ("Unit " & Sourcer.Image (Current)
                  & " withes " & Word
                  & " already withed by parent " & Sourcer.Image (Parent));
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

