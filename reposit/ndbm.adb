with Ada.Text_Io;
with System;
with Bit_Ops;
package body Ndbm is

  Db : System.Address := System.Null_Address;

  -- Open flags
  O_Rdwr  : constant Integer := 8#0000002#;
  O_Creat : constant Integer := 8#0001000#;

  -- Open mode
  S_Irusr : constant Natural := 8#0000400#;
  S_Iwusr : constant Natural := 8#0000200#;
  S_Irgrp : constant Natural := 8#0000040#;

  -- Store flag
  Dbm_Replace : constant Natural := 1;

  -- Length of key and data in bytes
  Byte_Size : constant := 8;
  Key_Len  : constant Long_Long_Integer
           := (Key'Size + Byte_Size - 1) / Byte_Size;
  Data_Len : constant Long_Long_Integer
           := (Data'Size + Byte_Size - 1) / Byte_Size;

  type Datum is record
    Dptr : System.Address;
    Dsize : Long_Long_Integer;
  end record;
    

  --------------------------
  -- Imported C functions --
  --------------------------
  procedure Memcpy (Dest, Src : in System.Address;
                    N : in Long_Long_Integer);
  pragma Import (C, Memcpy, "memcpy");

  procedure Free (P : in System.Address);
  pragma Import (C, Free, "free");

  function Dbm_Open (File_Name : System.Address;
                     Flags : Integer;
                     Mode  : Natural)
           return System.Address;
  pragma Import (C, Dbm_Open, "dbm_open");

  procedure Dbm_Close (Db : in System.Address);
  pragma Import (C, Dbm_Close, "dbm_close");

  function Dbm_Store (Db   : in System.Address;
                      Key  : in System.Address;
                      Data : in System.Address;
                      Mode : in Integer)
           return Integer;
  pragma Import (C, Dbm_Store, "c_dbm_store");

  procedure Dbm_Fetch (Db   : in System.Address;
                       Key  : in System.Address;
                       Data : in System.Address);
  pragma Import (C, Dbm_Fetch, "c_dbm_fetch");

  function Dbm_Delete (Db  : in System.Address;
                       Key : in System.Address)
           return Integer;
  pragma Import (C, Dbm_Delete, "c_dbm_delete");

  procedure Dbm_FirstKey (Db  : in System.Address;
                          Key : in System.Address);
  pragma Import (C, Dbm_FirstKey, "c_dbm_firstkey");

  procedure Dbm_NextKey (Db  : in System.Address;
                         Key : in System.Address);
  pragma Import (C, Dbm_NextKey, "c_dbm_nextkey");

  ------------------------
  -- Internal functions --
  ------------------------
  procedure Check_Open is
  begin
    if not Is_Open then
      raise Use_Error;
    end if;
  end Check_Open;

  ----------
  -- Open --
  ----------
  procedure Open is
    Name_For_C : constant String := File_Name & Ascii.Nul;
    use Bit_Ops;
  begin
    if Is_Open then
      raise Use_Error;
    end if;
    Db := Dbm_Open (Name_For_C(1)'Address,
                    O_Rdwr or O_Creat,
                    S_Irusr or S_Iwusr or S_Irgrp);
    if not Is_Open then
      raise Name_Error;
    end if;
  end Open;

  -----------
  -- Close --
  -----------
  procedure Close is
  begin
    Check_Open;
    Dbm_Close (Db);
  end Close;

  -------------
  -- Is_Open --
  -------------
  function Is_Open return Boolean is
    use type System.Address;
  begin
    return Db /= System.Null_Address;
  end Is_Open;

  -----------
  -- Write --
  -----------
  procedure Write (K : in Key; D : in Data) is
    The_Key, The_Data : Datum;
    Res : Integer;
  begin
    Check_Open;
    The_Key.Dptr := K'Address;
    The_Key.Dsize := Key_Len;
    The_Data.Dptr := D'Address;
    The_Data.Dsize := Data_Len;
    Res := Dbm_Store (Db, The_Key'Address, The_Data'Address,
                      Dbm_Replace);
    if Res /= 0 then
      raise Ndbm_Error;
    end if;
  end Write;

  ----------
  -- Read --
  ----------
  function Read (K : in Key) return Data is
    The_Key, The_Data : Datum;
    D : Data;
    use type System.Address;
  begin
    Check_Open;
    The_Key.Dptr := K'Address;
    The_Key.Dsize := Key_Len;
    Dbm_Fetch (Db, The_Key'Address, The_Data'Address);
    if The_Data.Dptr = System.Null_Address then
      raise No_Data;
    end if;
    Memcpy (D'Address, The_Data.Dptr, Long_Long_Integer(Data_Len));
    return D;
  end Read;

  ------------
  -- Delete --
  ------------
  procedure Delete (K : in Key) is
    The_Key : Datum;
    Res : Integer;
  begin
    Check_Open;
    The_Key.Dptr := K'Address;
    The_Key.Dsize := Key_Len;
    Res := Dbm_Delete (Db, The_Key'Address);
    if Res /= 0 then
      raise No_Data;
    end if;
  end Delete;

  ---------------
  -- First_Key --
  ---------------
  function First_Key return Key is
    The_Key : Datum;
    K : Key;
    use type System.Address;
  begin
    Check_Open;
    Dbm_FirstKey (Db, The_Key'Address);
    if The_Key.Dptr = System.Null_Address then
      raise No_Data;
    end if;
    Memcpy (K'Address, The_Key.Dptr, Long_Long_Integer(Key_Len));
    return K;
  end First_Key;

  --------------
  -- Next_Key --
  --------------
  function Next_Key return Key is
    The_Key : Datum;
    K : Key;
    use type System.Address;
  begin
    Check_Open;
    Dbm_NextKey (Db, The_Key'Address);
    if The_Key.Dptr = System.Null_Address then
      raise No_Data;
    end if;
    Memcpy (K'Address, The_Key.Dptr, Long_Long_Integer(Key_Len));
    return K;
  end Next_Key;

end Ndbm;

