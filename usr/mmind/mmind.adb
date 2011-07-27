-- Mastem mind (graphic or text mode)
with Basic_Proc, Argument;
with Common, Action, Mmind_Asc;

procedure Mmind is
   Ascii : Boolean;
begin
  -- Parse arguments
  declare
    Level : Common.Last_Level_Range;
    Default_Level : constant Common.Last_Level_Range
                  := Common.Last_Level_Range'Succ
                           (Common.Last_Level_Range'First);
  begin
    case Argument.Get_Nbre_Arg is
      when 0 =>
        -- Second level => 4
        Ascii := False;
        Level := Default_Level;
      when 1 =>
        if Argument.Get_Parameter = "-t" then
          Ascii := True;
          Level := Default_Level;
        else
          Ascii := False;
          Level := Common.Last_Level_Range'Value (Argument.Get_Parameter);
        end if;
      when 2 =>
        if Argument.Get_Parameter (Occurence => 1) /= "-t" then
          raise Constraint_Error;
        end if;
        Ascii := True;
        Level := Common.Last_Level_Range'Value
                     (Argument.Get_Parameter (Occurence => 2));
      when others =>
        raise Constraint_Error;
    end case;

    Common.Store_Level (Level);
  exception
    when Constraint_Error =>
      Basic_Proc.Put_Line_Error (
       "Syntax ERROR. Usage: " & Argument.Get_Program_Name
       & " [ -t ] [ <level> ] (level from 3 to 5, default 4).");
      return;
  end;

  if Ascii then
    -- Ascii Mode
    Mmind_Asc;
  else
    -- X Mode
    Common.Set_Level_To_Stored;
    Action.Init;

    loop
      exit when not Action.Play;
    end loop;
  end if;

exception
  when Action.No_Mouse =>
    Basic_Proc.Put_Line_Error ("Sorry, MOUSE not found.");
    return;
end Mmind;

