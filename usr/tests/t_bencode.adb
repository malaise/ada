with Basic_Proc, Argument, As.U, Text_Line, Bencode,
     Trilean, Hexa_Utils, Upper_Str;
procedure T_Bencode is
  Policy : Bencode.Dictio_Keys_Policy;
  Num : Positive;
  Op : Trilean.Trilean;
  Ifile, Ofile : Text_Line.File_Type;
  Ibuf, Obuf, Tmp : As.U.Asu_Us;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " [ -c | -s ] -b2x  |  -x2b  [ <file_name> ]");
    Basic_Proc.Put_Line_Error (" or:   " & Argument.Get_Program_Name & " -s2h");
    Basic_Proc.Set_Error_Exit_Code;
  end Usage;

begin

  -- Parse arguments
  -- Opt -c or -s
  Policy := Bencode.None;
  Num := 1;
  if Argument.Get_Nbre_Arg = 2 then
    if Argument.Get_Parameter (1) = "-c" then
      Policy := Bencode.Check;
      Num := 2;
    elsif Argument.Get_Parameter (1) = "-s" then
      Policy := Bencode.Sort;
      Num := 2;
    end if;
  end if;
  -- Mode
  if Argument.Get_Nbre_Arg >= Num
  and then Argument.Get_Parameter (Num) = "-b2x" then
    Op := Trilean.True;
    Num := Num + 1;
  elsif Argument.Get_Nbre_Arg >= Num
  and then Argument.Get_Parameter (Num) = "-x2b" then
    Op := Trilean.False;
    Num := Num + 1;
  elsif Argument.Get_Nbre_Arg = 1
  and then Argument.Get_Parameter (1) = "-s2h" then
    Op := Trilean.Other;
  else
    Usage;
    return;
  end if;

  -- Create flows
  Ifile.Open_All (Text_Line.In_File,
      (if Argument.Get_Nbre_Arg = Num then Argument.Get_Parameter (Num)
       else ""));
  Ofile.Open_All (Text_Line.Out_File);

  -- Read stdin
  loop
    Tmp := Ifile.Get;
    exit when Tmp.Is_Null;
    Ibuf.Append (Tmp);
  end loop;

  -- Convert
  case Op is
    when Trilean.True =>
      declare
        Bytes : Bencode.Byte_Array (1 .. Ibuf.Length);
      begin
        for I in 1 .. Ibuf.Length loop
          Bytes(I) := Character'Pos(Ibuf.Element (I));
        end loop;
        Obuf := As.U.Tus (Bencode.Bencode2Xml (Bytes, Policy));
      end;
    when Trilean.False =>
      declare
        Bytes : constant Bencode.Byte_Array
              := Bencode.Xml2Bencode (Ibuf.Image, Policy);
      begin
        for I in Bytes'Range loop
          Obuf.Append (Character'Val(Natural(Bytes(I))));
        end loop;
      end;
    when Trilean.Other =>
      for I in 1 .. Ibuf.Length loop
        Obuf.Append (Upper_Str (
            Hexa_Utils.Image (Natural'(Character'Pos(Ibuf.Element (I))),
                              2, '0')));
      end loop;
  end case;

  -- Put to stdout
  Ofile.Put (Obuf.Image);

  -- Close
  Ifile.Close_All;
  Ofile.Close_All;

end T_Bencode;

