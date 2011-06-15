with Basic_Proc, Normal, Rnd;

with Common, Response;

procedure Mmind_Asc is
begin
  Common.Set_Level_To_Stored;

  Basic_Proc.Put_Line_Output ("Find "
    & Normal(Integer(Common.Get_Stored_Level), 1)
    & " digits from 1 to 8.");

  Rnd.Randomize;
  Response.New_Code;

  declare
    Buffer : String(1 .. 80);
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
          Basic_Proc.Put_Output(Normal(I, 2) & "? ");
          Basic_Proc.Get_Input(Buffer, Len);
          if Len /= Str'Length then
            raise Constraint_Error;
          end if;
          Str := Buffer(1 .. Len);
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
      Str := (others => ' ');
      Basic_Proc.Put_Output(" -> " & Str & " ");
      Basic_Proc.Put_Line_Output(Normal(Rep.Placed_Ok,1) & Normal(Rep.Colors_Ok,1));

      exit Play when Rep.Placed_Ok = Len;

    end loop Play;

    Prop := Response.Get_Code;
    Basic_Proc.Put_Output(" => ");
    for I in 1 .. Len loop
      Basic_Proc.Put_Output(Normal(Integer(Prop.Color(Common.Level_Range(I))), 1));
    end loop;
    Basic_Proc.New_Line_Output;

  end;

end Mmind_Asc;

