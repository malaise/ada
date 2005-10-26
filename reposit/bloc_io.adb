-- Perform read/write of several Element_Type in file
with Ada.Characters.Latin_1;
package  body Bloc_Io is

  Element_Size : constant Positive := Element_Type'Size / System.Storage_Unit;

  Nul : Character renames Ada.Characters.Latin_1.Nul;
  -----------------
  -- C interface --
  -----------------
  Create_Str :  constant String := "w+" & Nul;
  Read_Str  : constant String := "r" & Nul;
  Write_Str : constant String := "r+" & Nul;
  subtype Size_T is Long_Integer;

  function Fopen(Path : in System.Address;
                 Mode : in System.Address) return System.Address;
  pragma Import (C, Fopen, "fopen");

  function Fclose(File : System.Address) return Integer;
  pragma Import (C, Fclose, "fclose");

  function Fread(To : System.Address;
                 Size_Item : Size_T;
                 Num_Item  : Size_T;
                 File : System.Address) return Size_T;
  pragma Import (C, Fread, "fread");

  function Fwrite(To : System.Address;
                  Size_Item : Size_T;
                  Num_Item  : Size_T;
                  File : System.Address) return Size_T;
  pragma Import (C, Fwrite, "fwrite");

  Seek_Set : constant Integer := 0;
  Seek_End : constant Integer := 2;
  function Fseek(File : System.Address;
                 Offset : Size_T;
                 Whence : Integer) return Integer;
  pragma Import (C, Fseek, "fseek");

  function Ftell(File : System.Address) return Size_T;
  pragma Import (C, Ftell, "ftell");

  ---------------
  -- Functions --
  ---------------

  -- Create Open / Close stuff
  procedure Create(File : in out File_Type;
                   Name : in String) is
    Name4C : constant String := Name & Nul;
    use type System.Address;
  begin
    if Is_Open(File) then
      raise Status_Error;
    end if;
    File.Ext_File := Fopen(Name4C'Address,
                           Create_Str'Address);
    if File.Ext_File = System.Null_Address then
      raise Name_Error;
    end if;
    File.Mode := Out_File;
  end Create;

  procedure Open(File : in out File_Type;
                 Mode : in File_Mode;
                 Name : in String) is
    Name4C : constant String := Name & Nul;
    use type System.Address;
  begin
    if Mode = Out_File then
      Create(File, Name);
      return;
    end if;

    if Is_Open(File) then
      raise Status_Error;
    end if;

    if Mode = In_File then
      File.Ext_File := Fopen(Name4C'Address,
                             Read_Str'Address);
      File.Mode := In_File;
    else
      File.Ext_File := Fopen(Name4C'Address,
                             Write_Str'Address);
      File.Mode := Inout_File;
    end if;
    if File.Ext_File = System.Null_Address then
      raise Name_Error;
    end if;
  end Open;

  procedure Check_Open(File : in File_Type) is
  begin
    if not Is_Open(File) then
      raise Status_Error;
    end if;
  end Check_Open;

  procedure Close(File : in out File_Type) is
  begin
    Check_Open(File);
    if Fclose(File.Ext_File) /= 0 then
      raise Device_Error;
    end if;
    File.Ext_File := System.Null_Address;
  end Close;

  function Is_Open(File : in File_Type) return Boolean is
    use type System.Address;
  begin
    return File.Ext_File /= System.Null_Address;
  end Is_Open;


  -- Read Element_Array'Length Elements from file
  -- or raise End_Error
  procedure Read(File : in File_Type;
                 Item : in out Element_Array;
                 From : in Positive_Count) is
  begin
    Check_Open(File);
    if File.Mode = Out_File then
      raise Status_Error;
    end if;
    Set_Index(File, From);
    Read(File, Item);
  end Read;

  procedure Read(File : in File_Type;
                 Item : in out Element_Array) is
    Res : Size_T;
  begin
    Check_Open(File);
    if File.Mode = Out_File then
      raise Status_Error;
    end if;
    -- Read
    Res := Fread(Item'Address,
                 Size_T(Element_Size),
                 Size_T(Item'Length),
                 File.Ext_File);
    if Res < 0 then
      raise Device_Error;
    elsif Res /= Size_T(Item'Length) then
      raise End_Error;
    end if;
  end Read;

  -- Write Element_Array'Length Elements to file
  -- May increase size
  procedure Write(File : in File_Type;
                  Item : in Element_Array;
                  To   : in Positive_Count) is
  begin
    Check_Open(File);
    if File.Mode = In_File then
      raise Status_Error;
    end if;
    Set_Index(File, To);
    Write(File, Item);
  end Write;

  procedure Write(File : in File_Type;
                  Item : in Element_Array) is
    Res : Size_T;
  begin
    Check_Open(File);
    if File.Mode = In_File then
      raise Status_Error;
    end if;
    -- Write
    Res := Fwrite(Item'Address,
                  Size_T(Element_Size),
                  Size_T(Item'Length),
                  File.Ext_File);
    if Res < 0 then
      raise Device_Error;
    elsif Res /= Size_T(Item'Length) then
      raise End_Error;
    end if;
    -- Workaround to a bug on Tru64
    -- Index is wrong when reading without index after
    --  a write. Reading the index fixes it!!!
    Bug_Fix:
    declare
      Dummy_Index : Positive_Count;
    begin
      Dummy_Index := Index(File);
    end Bug_Fix;
  end Write;

  -- Amount of Elements in file
  function Size(File : in File_Type) return Count is
    Pos : Size_T;
    Res : Size_T;
  begin
    Check_Open(File);
    -- Save pos
    Pos := Ftell(File.Ext_File);
    if Pos < 0 then
      raise Device_Error;
    end if;
    -- Go to end of file
    if Fseek(File.Ext_File, 0, Seek_End) /= 0 then
      raise Device_Error;
    end if;
    -- Get len
    Res := Ftell(File.Ext_File);
    if Res < 0 then
      raise Device_Error;
    end if;
    -- Restore pos
    if Fseek(File.Ext_File, Pos, Seek_Set) /= 0 then
      raise Device_Error;
    end if;
    return Count(Res) / Count(Element_Size);
  end Size;

  function Index(File : in File_Type) return Positive_Count is
    Res : Size_T;
  begin
    Check_Open(File);
    Res := Ftell(File.Ext_File);
    if Res < 0 then
      raise Device_Error;
    end if;
    return Count(Res) / Count(Element_Size) + 1;
  end Index;

  procedure Set_Index(File : in File_Type; To : in Positive_Count) is
  begin
    Check_Open(File);
    if Fseek(File.Ext_File, Size_T(To - 1) * Size_T(Element_Size),
             Seek_Set) /= 0 then
      raise Device_Error;
    end if;
  end Set_Index;

end Bloc_Io;

