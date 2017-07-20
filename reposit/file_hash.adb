with Bit_Ops, Sys_Calls, Text_Line;
package body File_Hash is

  -- Set, "=" and Key_Image for the cell
  procedure Set (To : out Line_Rec; Val : in Line_Rec) is
  begin
    To := Val;
  end Set;
  overriding function "=" (Current : Line_Rec;
                           Criteria : Line_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Txt = Criteria.Txt;
  end "=";
  function Key_Image (Element : Line_Rec) return String is (Element.Txt.Image);

  -- The goal is to store the hash of the line (0 to FFF) and a reasonable
  --  length of the line (x?) in *FFF.
  -- x could be (15) but this is too small, so it is FF (255), which is OK
  -- Longer lines are truncated
  -- Max_Str_Len : constant := 16#FF#;
  -- Hash_Max : constant Hashing.Max_Hash_Range := 16#FFFFF#;
  function Hash_Func (Key : String) return Max_Hash_Range is
    Len : constant Natural := Key'Length;
    use Bit_Ops;
  begin
    if Len = 0 or else Len > Max_Str_Len then
      return 0;
    end if;
    return Hashing.Max_Hash_Range(
       Shl (Len, 12) or Natural(Hashing.Def_Max_Hash_Func(Key)) );
  end Hash_Func;

  -- Load the content of the file in the list
  procedure Load (File_Name : in String;
                  List : in out List_Mng.List_Type) is
    Line : Long_Longs.Ll_Positive := 1;
    Fd : Sys_Calls.File_Desc;
    File : Text_Line.File_Type;
  begin
        -- Load file
    Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
    File.Open (Text_Line.In_File, Fd);
    loop
      declare
        Word : constant String := File.Get;
        Len : Natural := Word'Length;
      begin
        -- End of file
        exit when Len = 0;
        if Word(Len) = Text_Line.Line_Feed_Char then
          Len := Len - 1;
        end if;
        -- Store line in Storage and its key in H list
        List.Insert (
            (Line, As.U.Tus (Word(1 ..
                    (if Len < Max_Str_Len then Len else Max_Str_Len)))) );
      end;
      Line := Line + 1;
    end loop;

    -- Done
    File.Close;
    Sys_Calls.Close (Fd);

  exception
    when Sys_Calls.Name_Error | Sys_Calls.System_Error =>
      raise Init_Error;
  end Load;

end File_Hash;

