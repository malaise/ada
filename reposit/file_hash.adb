with Bit_Ops, Sys_Calls, Text_Line;
package body File_Hash is

  -- The goal is to store the hash of word (0 to FFF) and a reasonable length
  --  of the word (x?) in *FFF, so x is either F (15!) of FF (255) which is OK
  -- Longer names are not stored
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
        -- Store word in Storage and its key in H list
        if Len < Max_Str_Len then
          List.Insert (As.U.Tus (Word(1 .. Len)));
        end if;
      end;
    end loop;

    -- Done
    File.Close;
    Sys_Calls.Close (Fd);

  exception
    when Sys_Calls.Name_Error | Sys_Calls.System_Error =>
      raise Init_Error;
  end Load;

end File_Hash;

