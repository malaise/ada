with Argument, Basic_Proc, As.U, My_Math, Gets, Mixed_Str,
     Normalization;
with Mapcode_Lib;
use Mapcode_Lib;
procedure Mapcode is

  function Integer_Image (I : Integer) return String is
    Str : constant String := I'Img;
  begin
    if Str(Str'First) /= ' ' then
      return Str;
    else
      return Str (Integer'Succ (Str'First) .. Str'Last);
    end if;
  end Integer_Image;

  procedure Put_Territory (Territory : in String) is
    Index : Mapcode_Lib.Territory_Range;
    Name : As.U.Asu_Us;
  begin
    Basic_Proc.Put_Output ("  " & Territory);
    Index := Get_Territory_Number (Territory);
    Name := As.U.Tus (Get_Territory_Alpha_Code (Index));
    Basic_Proc.Put_Line_Output (" -> " & Integer_Image (Index)
      & ": " & Name.Image
      & " = " & Get_Territory_Alpha_Code (Name.Image, Mapcode_Lib.Local)
      & "/" & Get_Territory_Alpha_Code (Name.Image, Mapcode_Lib.International)
      & "/" & Get_Territory_Alpha_Code (Name.Image, Mapcode_Lib.Shortest)
      & "/" & Get_Territory_Fullname (Name.Image) );
    if Is_Subdivision (Name.Image) then
      Index := Get_Parent_Of (Name.Image);
      Basic_Proc.Put_Line_Output ("Parent -> "
        & Get_Territory_Alpha_Code (Index));
    end if;
    if Has_Subdivision (Name.Image) then
      Basic_Proc.Put_Line_Output ( "Has subdivisions");
    end if;
  exception
    when Mapcode_Lib.Unknown_Territory =>
      Basic_Proc.Put_Line_Output (" not found");
  end Put_Territory;

  function Is_Command (Arg : in String) return Boolean is
    (Arg = "-h" or else Arg = "-t" or else Arg = "-c" or else Arg = "-d");

  function Image (F : My_Math.Real) return String is
    R : My_Math.Real;
    Frac_Len : constant := 9;
  begin
    R := My_Math.Round_At (F, -Frac_Len);
    return Normalization.Normal_Fixed (R, Frac_Len + 5, 4, '0');
  end Image;

  I : Positive;
  Command, Arg1, Arg2 : As.U.Asu_Us;

  Coord : Mapcode_Lib.Coordinate;
  Territory : As.U.Asu_Us;
  Shortest : Boolean;
  Precision : Precisions;
begin
    I := 1;
    loop
      Argument.Get_Parameter (Command, I);
      if Command.Image = "-h" then
        Basic_Proc.Put_Line_Output (
          "Usage: " & Argument.Get_Program_Name & " <Command>");
        Basic_Proc.Put_Line_Output (
          "  -h                            // This help");
        Basic_Proc.Put_Line_Output (
          "  -t <territory> [ <context> ]  // Territory info");
        Basic_Proc.Put_Line_Output (
          "  -c <lat> <lon> [ <options> ]  // Encode");
        Basic_Proc.Put_Line_Output (
          "    [ <territory> ] [ <shortest> ] [ <precision> ]");
        Basic_Proc.Put_Line_Output (
          "  -d <mapcode> [ <territory> ]  // Decode");
      elsif Command.Image = "-t" then
        -- Display territory info of next argument
        I := I + 1;
        Put_Territory (Argument.Get_Parameter (Occurence => I));
      elsif Command.Image = "-c" then
        I := I + 1;
        Argument.Get_Parameter (Arg1, I);
        Coord.Lat := Gets.Get_Int_Real (Arg1.Image);
        I := I + 1;
        Argument.Get_Parameter (Arg2, I);
        Coord.Lon := Gets.Get_Int_Real (Arg2.Image);
        Territory := As.U.Asu_Null;
        Shortest := True;
        Precision := 0;
        for J in I + 1 .. Argument.Get_Nbre_Arg loop
          Argument.Get_Parameter (Arg1,  J);
          exit when Is_Command (Arg1.Image);
          I := J;
          if Mixed_Str (Arg1.Image) = "True"
          or else Mixed_Str (Arg1.Image) = "False" then
            Shortest := Boolean'Value (Arg1.Image);
          elsif Arg1.Length = 1
          and then Arg1.Element (1) >= '0'
          and then Arg1.Element (1) >= '2' then
            Precision := Precisions'Value (Arg1.Image);
          else
            Territory := Arg1;
          end if;
        end loop;
        Basic_Proc.Put_Line_Output (Image (Coord.Lat)
                                  & " " & Image (Coord.Lon));
        declare
          Codes : constant Mapcode_Lib.Map_Code_Infos
                := Encode (Coord, Territory.Image, Shortest, Precision);
        begin
          for J in Codes'Range loop
            Basic_Proc.Put_Line_Output ("=> "
              &  Codes(J).Map_Code.Image
              & " " & Codes(J).Territory_Alpha_Code.Image
              & " " & Codes(J).Full_Map_Code.Image
              & " " & Integer_Image (Codes(J).Territory_Number));
          end loop;
        end;
        Basic_Proc.New_Line_Output;
      elsif Command.Image = "-d" then
        -- Decode next argument, optionally with context
        I := I + 1;
        Argument.Get_Parameter (Arg1, I);
        Arg2.Set_Null;
        if I < Argument.Get_Nbre_Arg then
          Argument.Get_Parameter (Arg2, I + 1);
          if Is_Command (Arg2.Image) then
            Arg2.Set_Null;
          else
            I := I + 1;
          end if;
        end if;
        Coord := Decode (Arg1.Image, Arg2.Image);
        Basic_Proc.Put_Line_Output ("=> " & Image (Coord.Lat)
                                  & " " & Image (Coord.Lon));
      end if;
      exit when I = Argument.Get_Nbre_Arg;
      I := I + 1;
  end loop;

end Mapcode;

