with As.U; use As.U;
with Hashing, Bit_Ops, Sys_Calls, Text_Line, Basic_Proc;
package body File_Hash is

  -- Storage of Asu_Us
  package Storage is
    subtype Key_Type is Asu_Us_Access;
    function Store (Word : String) return Key_Type;

    procedure Clear;
  end Storage;


  -- Dump of string knowing it key
  procedure Dump (Key : in Storage.Key_Type) is
  begin
    Basic_Proc.Put_Output (Asu_Ts (Key.all));
  end Dump;

  -- The goal is to store the hash of word (0 to 3FF) and the length of the word
  -- (x?) in FFFF, so x is on F3 => 3 * 16
  -- Longer names are not stored
  -- Max_Str_Len : constant := 16#F3#;
  package Hash_Sized is new Hashing.Sized_Hash (16#FFFF#);
  function Hash_Func (Key : String) return Hash_Sized.Hash_Range is
    Len : constant Natural := Key'Length;
    use Bit_Ops;
  begin
    if Len = 0 or else Len > Max_Str_Len then
      return 0;
    end if;
    return Hash_Sized.Hash_Range(
       Shl (Len, 10) or Natural(Hash_Sized.Hash_Def_Func(Key)) );
  end Hash_Func;
  package Hash is new Hash_Sized.Hash_Mng (Storage.Key_Type, Dump, Hash_Func);
  H : Hash.Hash_Table;

  package body Storage is
    List : Asu_Dyn_List_Mng.List_Type;
    function Store (Word : String) return Key_Type is
    begin
      -- Append
      if not List.Is_Empty
      and then List.Get_Position (Asu_Dyn_List_Mng.From_Last) /= 1 then
        List.Rewind (Where => Asu_Dyn_List_Mng.Prev);
      end if;
      List.Insert (Asu_Tus (Word) );
      return List.Access_Current;
    end Store;

    procedure Clear is
    begin
      List.Delete_List;
    end Clear;
  end Storage;

  -- Init the table for a file
  procedure Init (File_Name : in String) is
    Fd : Sys_Calls.File_Desc;
    File : Text_Line.File_Type;
  begin
    -- Clear
    Storage.Clear;
    H.Clear_All;
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
        -- Store word in Storage and its key in H
        if Len < Max_Str_Len then
          H.Store (Word (1 .. Len), Storage.Store (Word(1 .. Len)));
        end if;
      end;
    end loop;

    -- Done
    File.Close;
    Sys_Calls.Close (Fd);

  exception
    when Sys_Calls.Name_Error | Sys_Calls.System_Error =>
      raise Init_Error;
  end Init;

  function Exists (Word : String) return Boolean is
    Hkey : Hash.Hash_Range;
    Res : Hash.Found_Rec;
  begin
    if Word'Length > Max_Str_Len then
      raise Too_Long;
    end if;
    Hkey := Hash_Func (Word);
    H.Reset_Find (Hkey);
    loop
      H.Find_Next (Hkey, Res);
      case Res.Found is
        when True =>
          if Asu_Ts (Res.Data.all) = Word then
            -- Match
            return True;
          end if;
          -- Iterate on the key
        when False =>
          return False;
      end case;
    end loop;
  end Exists;

end File_Hash;

