with Utf_8, Utf_16, Unicode,
     Argument, Basic_Proc, As.U, Gets, Hexa_Utils, Upper_Str;
procedure T_Utf is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
         & " <mode> { <hexanum> }");
    Basic_Proc.Put_Line_Error (" <mode> ::= -unicode | -utf8 | -utf16 | -str");
  end Usage;

  Invalid_Argument : exception;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR : " & Msg);
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  function Get_Value (Str : String) return Natural is
    (Gets.Get_Int ("16#" & Str & "#"));

  procedure Put (Str : in String) renames Basic_Proc.Put_Output;
  procedure Put (N : in Natural; Width : in Positive) is
  begin
    Put (Upper_Str (Hexa_Utils.Image (N, Width)));
  end Put;

  procedure Put_Sep (I : Positive) is
  begin
    if I = 1 then
      Put (" ");
    else
      Put ("   ");
    end if;
  end Put_Sep;

  type Mode_List is (Uni, Utf8, Utf16, Str);
  Mode : Mode_List;
  Tmp_Code : Unicode.Unicode_Number;
  Tmp_Str : As.U.Asu_Us;
  K : Natural;
  Codes : Unicode.Unbounded_Unicode.Unbounded_Array;

begin

  -- Parse arguments
  begin
    if Argument.Get_Nbre_Arg = 0 then
      Usage;
      return;
    elsif Argument.Get_Parameter (1) = "-unicode" then
      Mode := Uni;
    elsif Argument.Get_Parameter (1) = "-utf8" then
      Mode := Utf8;
    elsif Argument.Get_Parameter (1) = "-utf16" then
      Mode := Utf16;
    elsif Argument.Get_Parameter (1) = "-str" then
      Mode := Str;
    else
      raise Constraint_Error;
    end if;
  exception
    when others =>
      raise Invalid_Argument;
  end;

  -- Parse codes and store corresponding unicodes
  for I in 2 .. Argument.Get_Nbre_Arg loop
    case Mode is
      when Uni =>
        -- Each arg is a Unicode number in hexa
        Tmp_Code := Get_Value (Argument.Get_Parameter (Occurence => I));
      when Utf8 =>
        -- Each arg is a sequence of UTF-8 bytes in hexa
        Argument.Get_Parameter (Tmp_Str, I);
        -- 2 digits for each of the 4 bytes max
        if Tmp_Str.Length rem 2 /= 0 or else Tmp_Str.Length > 8 then
          raise Invalid_Argument;
        end if;
        declare
          Len : constant Natural := Tmp_Str.Length / 2;
          Word : Utf_8.Word (1 .. Len);
        begin
          K := 1;
          for J in 1 .. Len loop
            Tmp_Code := Hexa_Utils.Value (Tmp_Str.Slice(K, K + 1));
            K := K + 2;
            Word(J) := Character'Val (Tmp_Code);
          end loop;
          Tmp_Code := Utf_8.Decode (Word);
        end;
      when Utf16 =>
        -- Each arg is a sequence of UTF-16 bytes in hexa
        Argument.Get_Parameter (Tmp_Str, I);
        -- 4 digits for each of the 2 bytes max
        if Tmp_Str.Length rem 4 /= 0 or else Tmp_Str.Length > 8 then
          raise Invalid_Argument;
        end if;
        declare
          Len : constant Natural := Tmp_Str.Length / 4;
          Word : Utf_16.Word (1 .. Len);
        begin
          K := 1;
          for J in 1 .. Len loop
            Tmp_Code := Hexa_Utils.Value (Tmp_Str.Slice(K, K + 3));
            K := K + 4;
            Word(J) := Wide_Character'Val (Tmp_Code);
          end loop;
          Tmp_Code := Utf_16.Decode (Word);
        end;
      when Str =>
        -- Processed once later
        null;
    end case;
    Codes.Append (Tmp_Code);
  end loop;

  if Mode = Str then
    -- One arg, the UTF-8 sentence
    if Argument.Get_Nbre_Arg /= 2 then
      raise Invalid_Argument;
    end if;
    Argument.Get_Parameter (Tmp_Str, 2);
    begin
      Codes.Set (Utf_8.Decode (Tmp_Str.Image));
    exception
      when Utf_8.Invalid_Sequence =>
        raise Invalid_Argument;
    end;
  end if;

  -- Consider that each UTf-8 sequence takes 4 bytes, 2 digits each
  --  + one space separating
  -- Show unicode sequence
  Put ("Unicode :");
  for I in 1 .. Codes.Length loop
    Put_Sep (I);
    Put (Codes.Element (I), 8);
  end loop;
  Basic_Proc.New_Line_Output;

  -- Show Utf-16 sequences
  Put ("Utf-16  :");
  for I in 1 .. Codes.Length loop
    Put_Sep (I);
    declare
      Seq : constant Utf_16.Sequence := Utf_16.Encode (Codes.Element (I));
    begin
      for J in reverse Utf_16.Len_Range loop
        if J > Seq'Length then
          -- Pad
          Put ("    ");
        else
          Put (Natural'(Wide_Character'Pos (Seq(Seq'Last - J + 1))), 4);
        end if;
      end loop;
    end;
  end loop;
  Basic_Proc.New_Line_Output;

  -- Show Utf-8 sequences of bytes
  Put ("Utf-8   :");
  for I in 1 .. Codes.Length loop
    Put_Sep (I);
    declare
      Seq : constant Utf_8.Sequence := Utf_8.Encode (Codes.Element (I));
    begin
      for J in reverse Utf_8.Len_Range loop
        if J > Seq'Length then
          -- Pad
          Put ("  ");
        else
          Put (Natural'(Character'Pos (Seq(Seq'Last - J + 1))), 2);
        end if;
      end loop;
    end;
  end loop;
  Basic_Proc.New_Line_Output;

  -- Show Utf-8 chars
  Put ("Utf-8   :");
  Tmp_Str.Set_Null;
  for I in 1 .. Codes.Length loop
    Put_Sep (I);
    declare
      Out_Str : String (1 .. 8) := (others => ' ');
      Str : constant Utf_8.Sequence := Utf_8.Encode (Codes.Element (I));
    begin
      Out_Str (1 .. Str'Last) := Str;
      Tmp_Str.Append (Str);
      Put (Out_Str);
    end;
  end loop;
  Basic_Proc.New_Line_Output;

  -- Show Utf-8 string
  Put ("String  :");
  Put (" " & Tmp_Str.Image);
  Basic_Proc.New_Line_Output;

exception
  when Invalid_Argument =>
    Error ("Invalid Argument");
end T_Utf;

