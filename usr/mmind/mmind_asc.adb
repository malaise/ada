with Ada.Text_Io;

with Argument, Normal, Rnd;

with Common, Response;

procedure Mmind_Asc is
begin

  declare
    Level : Common.Last_Level_Range;
  begin
    if Argument.Get_Nbre_Arg > 1 then
      raise Constraint_Error;
    end if;
    Level := Common.Last_Level_Range'Value (Argument.Get_Parameter);
    Common.Store_Level (Level);
    Common.Set_Level_To_Stored;
  exception
    when Argument.Argument_Not_Found =>
      Level := Common.Last_Level_Range'First;
      Common.Store_Level (Level);
      Common.Set_Level_To_Stored;
    when Constraint_Error =>
      Ada.Text_Io.Put_Line (
       "Syntax ERROR. Usage is ""MMIND_ASC [ <level> ]"" (level from 3 to 5).");
      return;
  end;
 
  Ada.Text_Io.Put_Line ("Find "
    & Normal(Integer(Common.Get_Stored_Level), 1) 
    & " digits from 1 to 8.");

  Rnd.Randomize;
  Response.New_Code;

  declare
    Str : String(1 .. Integer(Common.Get_Stored_Level));
    Len : Natural;
    Prop : Response.Color_Rec(Common.Get_Stored_Level);
    Rep : Response.Response_Rec;
  begin

    Play:
    for I in 1 .. 10 loop

      Get:
      loop
        begin
          Ada.Text_Io.Put(Normal(I, 2) & "? ");
          Ada.Text_Io.get_Line(Str, Len);
          if Len /= Str'Length then
            raise Constraint_Error;
          end if;
          for I in 1 .. Len loop
            Prop.Color(Common.Level_Range(I))
                 := Common.Eff_Color_Range'Value(Str(I) & "");
          end loop;
          Ada.Text_Io.Skip_Line;
          exit Get;
        exception
          when others =>
            Ada.Text_Io.Skip_Line;
        end;
      end loop Get;

      Rep := Response.Respond(Prop); 
      Str := (others => ' ');
      Ada.Text_Io.Put(" -> " & Str & " ");
      Ada.Text_Io.Put_Line(Normal(Rep.Placed_Ok,1) & Normal(Rep.Colors_Ok,1)); 

      exit Play when Rep.Placed_Ok = Len;
  
    end loop Play;

    Prop := Response.Get_Code;
    Ada.Text_Io.Put(" => ");
    for I in 1 .. Len loop
      Ada.Text_Io.Put(Normal(Integer(Prop.Color(Common.Level_Range(I))), 1));
    end loop;
    Ada.Text_Io.New_Line;

  end;

end Mmind_Asc;

