with Basic_Proc, Images, Normalization;
use Basic_Proc;
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
      when Set   => Put_Line_Output ("SET");
      when Unset => Put_Line_Output ("UNSET");
      when Error => Put_Line_Output ("ERROR at pos " & Integer'Image(Pos) );
    end case;
  end Put;

begin

  loop
    begin
      Put_Output ("SPEED ? "); Get_Line (Str_Speed, Lst);
      Str_Speed (Lst+1 .. Str_Speed'Last) := (others => ' ');
      Value (Str_Speed, Speed, Res, Pos);
      Put (Res, Pos);
      if Res = Set then
        Put_Output ("VALUE : " & Normalization.Normal_Fixed (Speed, 6, 3));
      end if;
      if Res /= Error then
        Put_Output ("IMAGE : ");
        Put_Line_Output (Imag(Speed, Res=Set));
      end if;

      Put_Output ("ANGLE ? "); Get_Line (Str_Angle, Lst);
      Str_Angle (Lst+1 .. Str_Angle'Last) := (others => ' ');
      Value (Str_Angle, Angle, Res, Pos);
      Put (Res, Pos);
      if Res = Set then
        Put_Output ("VALUE : " & Images.Integer_Image (Integer(Angle.Degrees)));
      end if;
      if Res /= Error then
        Put_Output ("IMAGE : " & Imag(Angle, Res=Set));
      end if;

      Put_Output ("DRIFT ? "); Get_Line (Str_Drift, Lst);
      Str_Drift (Lst+1 .. Str_Drift'Last) := (others => ' ');
      Value (Str_Drift, Drift, Res, Pos);
      Put (Res, Pos);
      if Res = Set then
        Put_Output ("VALUE : ");
        if not Drift.Positiv then
          Put_Output ('-');
        else
          Put_Output ('+');
        end if;
        Put_Output (Images.Integer_Image (Integer(Drift.Degrees)));
        Put_Line_Output (Images.Integer_Image (Integer(Drift.Minutes)));
      end if;
      if Res /= Error then
        Put_Output ("IMAGE : ");
        Put_Line_Output (Imag(Drift, Res=Set));
      end if;
    end;
  end loop;
end T_Form;

