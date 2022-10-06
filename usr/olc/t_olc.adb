with Basic_Proc, Argument, Images;
with Olc;
procedure T_Olc is
  procedure Encode (C : in Olc.Coordinate;
                    P : in Olc.Precision_Range) is
  begin
    declare
      Code : constant String := Olc.Encode (C, P);
    begin
      Basic_Proc.Put_Line_Output (Code);
    end;
    end Encode;

    procedure Decode (C : Olc.Code_Type) is
      Se, Nw : Olc.Coordinate;
    begin
      Olc.Decode (C, Se, Nw);
      Basic_Proc.Put_Line_Output (Se.Lat'Img & " " & Se.Lon'Img
                          & " " & Nw.Lat'Img & " " & Nw.Lon'Img);
    end Decode;

    procedure Center (C : Olc.Code_Type) is
      Se, Nw, Ce: Olc.Coordinate;
    begin
      Olc.Decode (C, Se, Nw);
      Ce := Olc.Center_Of (Se, Nw);
      Basic_Proc.Put_Line_Output (Ce.Lat'Img & " " & Ce.Lon'Img);
    end Center;

    procedure Status (C : Olc.Code_Type) is
    begin
      if not Olc.Is_Valid (C) then
        Basic_Proc.Put_Line_Output ("Invalid");
        return;
      end if;
      Basic_Proc.Put_Output (Images.Integer_Image (Olc.Precision_Of (C)) & " ");
      if Olc.Is_Short (C) then
        Basic_Proc.Put_Line_Output ("Short");
        return;
      end if;
      if Olc.Is_Full (C) then
        Basic_Proc.Put_Line_Output ("Full");
      else
        Basic_Proc.Put_Line_Output ("Not_full");
      end if;
    end Status;

begin
  if Argument.Get_Parameter (Occurence => 1) = "-c" then
    Encode (
        C => (Lat => Olc.Real'Value (Argument.Get_Parameter (Occurence => 2)),
              Lon => Olc.Real'Value (Argument.Get_Parameter (Occurence => 3))),
        P => Olc.Precision_Range'Value (Argument.Get_Parameter (Occurence => 4)));
  elsif Argument.Get_Parameter (Occurence => 1) = "-d" then
    Decode (Argument.Get_Parameter (Occurence => 2));
  elsif Argument.Get_Parameter (Occurence => 1) = "-C" then
    Center (Argument.Get_Parameter (Occurence => 2));
  elsif Argument.Get_Parameter (Occurence => 1) = "-s" then
    Status (Argument.Get_Parameter (Occurence => 2));
  else
    Basic_Proc.Put_Line_Error ("Invalid argument");
    Basic_Proc.Set_Error_Exit_Code;
  end if;
end T_Olc;

