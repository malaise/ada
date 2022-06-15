with Utf_8, Unicode, Argument, Basic_Proc, As.U, Gets, Hexa_Utils, Upper_Str;
procedure T_Utf is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
         & " <mode> { <hexanum> }");
    Basic_Proc.Put_Line_Error (" <mode> ::= -unicode | -utf | -str");
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

  type Mode_List is (Uni, Utf, Str);
  Mode : Mode_List;
  Tmp_Code : Unicode.Unicode_Number;
  Tmp_Arg : As.U.Asu_Us;
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
    elsif Argument.Get_Parameter (1) = "-utf" then
      Mode := Utf;
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
      when Utf =>
        -- Each arg is a sequence of UTF-8 bytes in hexa
        Argument.Get_Parameter (Tmp_Arg, I);
        -- 2 digits for each oft the 4 bytes max
        if Tmp_Arg.Length rem 2 /= 0 or else Tmp_Arg.Length > 8 then
          raise Invalid_Argument;
        end if;
        declare
          Len : constant Natural := Tmp_Arg.Length / 2;
          Word : Utf_8.Word (1 .. Len);
        begin
          K := 1;
          for J in 1 .. Len loop
            Tmp_Code := Hexa_Utils.Value (Tmp_Arg.Slice(K, K + 1));
            K := K + 2;
            Word(J) := Character'Val (Tmp_Code);
          end loop;
          Tmp_Code := Utf_8.Decode (Word);
        end;
      when Str =>
        -- Each arg is a UTF8 sequence for one char
        Argument.Get_Parameter (Tmp_Arg, I);
        if Tmp_Arg.Length > Utf_8.Max_Chars then
          raise Invalid_Argument;
        end if;
        begin
          Tmp_Code := Utf_8.Decode (Utf_8.Word(Tmp_Arg.Image));
        exception
          when Utf_8.Invalid_Sequence =>
            raise Invalid_Argument;
        end;
    end case;
    Codes.Append (Tmp_Code);
  end loop;

  -- Consider that each UTf-8 sequence takes 4 bytes, 2 digits each
  --  + one space separating
  -- Show unicode sequence
  Put ("Unicode :");
  for I in 1 .. Codes.Length loop
    Put ("     ");
    Put (Codes.Element (I), 4);
  end loop;
  Basic_Proc.New_Line_Output;

  -- Show Utf-8 sequences
  Put ("Utf-8   :");
  for I in 1 .. Codes.Length loop
    declare
      Seq : constant Utf_8.Sequence := Utf_8.Encode (Codes.Element (I));
    begin
      Put (" ");
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

  -- Show Utf-8 string
  Put ("String  :");
  for I in 1 .. Codes.Length loop
    declare
      Out_Str : String (1 .. 8) := (others => ' ');
      Str : constant String := Utf_8.Encode (Codes.Element (I));
    begin
      Out_Str (1 .. Str'Last) := Str;
      Put (" " & Out_Str);
    end;
  end loop;
  Basic_Proc.New_Line_Output;

exception
  when Invalid_Argument =>
    Error ("Invalid Argument");
end T_Utf;

