with My_Io; use My_Io;
with Nav_Types, Nav_Format;
use  Nav_Types, Nav_Format;

procedure T_Form is
  Str_Speed : String (1 .. 6);
  Str_Angle : String (1 .. 6);
  Str_Drift : String (1 .. 7);
  Res : Format_Result;
  Pos : Positive;
  Speed : T_Speed;
  Angle : T_Angle;
  Drift : T_Drift;
  Lst : Natural;

  procedure Put (Res : in Format_Result; Pos : in Positive) is
  begin
    case Res is
      when Set   => Put_Line ("SET");
      when Unset => Put_Line ("UNSET");
      when Error => Put_Line ("ERROR at pos " & Integer'Image(Pos) );
    end case;
  end Put;

begin

  loop
    begin
      Put ("SPEED ? "); Get_Line (Str_Speed, Lst);
      Str_Speed (Lst+1 .. Str_Speed'Last) := (others => ' ');
      Value (Str_Speed, Speed, Res, Pos);
      Put (Res, Pos);
      if Res = Set then
        Put ("VALUE : "); Put_Line (Speed);
      end if;
      if Res /= Error then
        Put ("IMAGE : "); Put_Line (Imag(Speed, Res=Set));
      end if;

      Put ("ANGLE ? "); Get_Line (Str_Angle, Lst);
      Str_Angle (Lst+1 .. Str_Angle'Last) := (others => ' ');
      Value (Str_Angle, Angle, Res, Pos);
      Put (Res, Pos);
      if Res = Set then
        Put ("VALUE : "); Put (Integer(Angle.Degrees));
        Put_Line (Integer(Angle.Minutes));
      end if;
      if Res /= Error then
        Put ("IMAGE : "); Put_Line (Imag(Angle, Res=Set));
      end if;

      Put ("DRIFT ? "); Get_Line (Str_Drift, Lst);
      Str_Drift (Lst+1 .. Str_Drift'Last) := (others => ' ');
      Value (Str_Drift, Drift, Res, Pos);
      Put (Res, Pos);
      if Res = Set then
        Put ("VALUE : ");
        if not Drift.Positiv then Put ('-'); else Put ('+'); end if;
        Put (Integer(Drift.Degrees));
        Put_Line (Integer(Drift.Minutes));
      end if;
      if Res /= Error then
        Put ("IMAGE : "); Put_Line (Imag(Drift, Res=Set));
      end if;
    exception
      when others => raise;
    end;
  end loop;
end T_Form;

