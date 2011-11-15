with Ada.Characters.Latin_1;
with Normal, Rnd, Async_Stdin;
with Common, Response;
procedure Mmind_Asc is
begin
  Common.Set_Level_To_Stored;

  Async_Stdin.Put_Line_Out ("Find "
    & Normal(Integer(Common.Get_Stored_Level), 1)
    & " digits from 1 to 8.");

  Rnd.Randomize;
  Response.New_Code;

  declare
    Len : constant Integer := Integer(Common.Get_Stored_Level);
    Prop : Response.Color_Rec(Common.Get_Stored_Level);
    Rep : Response.Response_Rec;
  begin

    Play:
    for I in 1 .. 10 loop

      Get:
      loop
        Async_Stdin.Put_Out (Normal (I, 2) & "? ");
        Async_Stdin.Overwrite;
        declare
          -- Allow up to 80 of input line
          Str : constant String := Async_Stdin.Get_Line (80, 5);
        begin
          if Str'Length /= Len  + 1
          or else Str(Str'Length) /= Ada.Characters.Latin_1.Lf then
            -- Validated input line must have 3/4/5 chars
            raise Constraint_Error;
          end if;
          for I in 1 .. Len loop
            Prop.Color(Common.Level_Range(I))
                 := Common.Eff_Color_Range'Value(Str(I) & "");
          end loop;
          exit Get;
        exception
          when others =>
            null;
        end;
      end loop Get;

      Rep := Response.Respond(Prop);
      declare
        Pad : constant String (1 .. Len) := (others => ' ');
      begin
        Async_Stdin.Put_Out (" -> " & Pad & " ");
      end;
      Async_Stdin.Put_Line_Out (Normal (Rep.Placed_Ok, 1)
                                 & Normal (Rep.Colors_Ok,1));

      exit Play when Rep.Placed_Ok = Len;

    end loop Play;

    Prop := Response.Get_Code;
    Async_Stdin.Put_Out (" => ");
    for I in 1 .. Len loop
      Async_Stdin.Put_Out (Normal (Integer(Prop.Color(
          Common.Level_Range(I))), 1));
    end loop;
    Async_Stdin.New_Line_Out;

  end;

exception
  when others =>
    Async_Stdin.Set_Async;
    raise;
end Mmind_Asc;

