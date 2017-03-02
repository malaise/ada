with Aski, Bit_Ops, C_Types;
package body Ndbm is

  -- Open flags
  O_Rdwr  : constant C_Types.Int := 8#0000002#;
  O_Creat : constant C_Types.Int := 8#0001000#;

  -- Open mode
  S_Irusr : constant C_Types.Int := 8#0000400#;
  S_Iwusr : constant C_Types.Int := 8#0000200#;
  S_Irgrp : constant C_Types.Int := 8#0000040#;

  -- Store flag
  Dbm_Replace : constant Natural := 1;

  -- Length of key and data in bytes
  subtype Data_Size_T is C_Types.Int;
  Byte_Size : constant := 8;
  Key_Len  : constant Data_Size_T
           := (Key'Size + Byte_Size - 1) / Byte_Size;
  Data_Len : constant Data_Size_T
           := (Data'Size + Byte_Size - 1) / Byte_Size;

  type Datum is record
    Dptr : System.Address;
    Dsize : Data_Size_T;
  end record;


  --------------------------
  -- Imported C functions --
  --------------------------
  procedure Memcpy (Dest, Src : in System.Address;
                    N : in C_Types.Size_T)
    with Import => True, Convention => C, External_Name => "memcpy";

  function Dbm_Open (File_Name : System.Address;
                     Flags : C_Types.Int;
                     Mode  : C_Types.Mode_T)
           return System.Address
    with Import => True, Convention => C, External_Name => "dbm_open";

  procedure Dbm_Close (Db : in System.Address)
    with Import => True, Convention => C, External_Name => "dbm_close";

  function Dbm_Store (Db   : in System.Address;
                      Key  : in System.Address;
                      Data : in System.Address;
                      Mode : in C_Types.Int)
           return C_Types.Int
    with Import => True, Convention => C, External_Name => "c_dbm_store";

  procedure Dbm_Fetch (Db   : in System.Address;
                       Key  : in System.Address;
                       Data : in System.Address)
    with Import => True, Convention => C, External_Name => "c_dbm_fetch";

  function Dbm_Delete (Db  : in System.Address;
                       Key : in System.Address)
           return C_Types.Int
    with Import => True, Convention => C, External_Name => "c_dbm_delete";

  procedure Dbm_Firstkey (Db  : in System.Address;
                          Key : in System.Address)
    with Import => True, Convention => C, External_Name =>  "c_dbm_firstkey";

  procedure Dbm_Nextkey (Db  : in System.Address;
                         Key : in System.Address)
    with Import => True, Convention => C, External_Name => "c_dbm_nextkey";

  ------------------------
  -- Internal functions --
  ------------------------
  procedure Check_Open (Db : Database) is
  begin
    if not Is_Open (Db) then
      raise Use_Error;
    end if;
  end Check_Open;

  procedure Set (Dest : out Db_Rec; Val : in Db_Rec) is
  begin
    Dest := Val;
  end Set;

  procedure Finalize (Dest : in out Db_Rec) is
    use type System.Address;
  begin
    if Dest.Db_Addr /= System.Null_Address then
      Dbm_Close (Dest.Db_Addr);
      Dest.Db_Addr := System.Null_Address;
    end if;
  end Finalize;

  ----------
  -- Open --
  ----------
  procedure Open (Db : in out Database; File_Name : in String) is
    Name_For_C : constant String := File_Name & Aski.Nul;
    Addr : System.Address;
    use Bit_Ops; --## rule line off Use
    use type System.Address;
  begin
    if Is_Open (Db) then
      raise Use_Error;
    end if;
    Addr := Dbm_Open (Name_For_C(1)'Address,
                      O_Rdwr or O_Creat,
                      C_Types.Uint32 (S_Irusr or S_Iwusr or S_Irgrp));
    if Addr = System.Null_Address then
      raise Name_Error;
    end if;
    Db.Init ( (Db_Addr => Addr) );
  end Open;

  -----------
  -- Close --
  -----------
  procedure Close (Db : in out Database) is
  begin
    Check_Open (Db);
    Dbm_Close (Db.Get_Access.Db_Addr);
    Db.Get_Access.Db_Addr := System.Null_Address;
  end Close;

  -------------
  -- Is_Open --
  -------------
  function Is_Open (Db : in Database) return Boolean is
    use type System.Address;
  begin
    return Db.Is_Set and then Db.Get_Access.Db_Addr /= System.Null_Address;
  end Is_Open;

  -----------
  -- Write --
  -----------
  procedure Write (Db : in out Database; K : in Key; D : in Data) is
    The_Key, The_Data : Datum;
    Res : Integer;
  begin
    Check_Open (Db);
    The_Key.Dptr := K'Address;
    The_Key.Dsize := Key_Len;
    The_Data.Dptr := D'Address;
    The_Data.Dsize := Data_Len;
    Res := Dbm_Store (Db.Get_Access.Db_Addr,
                      The_Key'Address, The_Data'Address,
                      Dbm_Replace);
    if Res /= 0 then
      raise Ndbm_Error;
    end if;
  end Write;

  ----------
  -- Read --
  ----------
  function Read (Db : Database; K : in Key) return Data is
    The_Key, The_Data : Datum;
    D : Data;
    use type System.Address;
  begin
    Check_Open (Db);
    The_Key.Dptr := K'Address;
    The_Key.Dsize := Key_Len;
    Dbm_Fetch (Db.Get_Access.Db_Addr, The_Key'Address, The_Data'Address);
    if The_Data.Dptr = System.Null_Address then
      raise No_Data;
    end if;
    Memcpy (D'Address, The_Data.Dptr, C_Types.Size_T (Data_Len));
    return D;
  end Read;

  ------------
  -- Delete --
  ------------
  procedure Delete (Db : in out Database; K : in Key) is
    The_Key : Datum;
    Res : Integer;
  begin
    Check_Open (Db);
    The_Key.Dptr := K'Address;
    The_Key.Dsize := Key_Len;
    Res := Dbm_Delete (Db.Get_Access.Db_Addr, The_Key'Address);
    if Res /= 0 then
      raise No_Data;
    end if;
  end Delete;

  ---------------
  -- First_Key --
  ---------------
  function First_Key (Db : Database) return Key is
    The_Key : Datum;
    K : Key;
    use type System.Address;
  begin
    Check_Open (Db);
    Dbm_Firstkey (Db.Get_Access.Db_Addr, The_Key'Address);
    if The_Key.Dptr = System.Null_Address then
      raise No_Data;
    end if;
    Memcpy (K'Address, The_Key.Dptr, C_Types.Size_T (Key_Len));
    return K;
  end First_Key;

  --------------
  -- Next_Key --
  --------------
  function Next_Key (Db : Database) return Key is
    The_Key : Datum;
    K : Key;
    use type System.Address;
  begin
    Check_Open (Db);
    Dbm_Nextkey (Db.Get_Access.Db_Addr, The_Key'Address);
    if The_Key.Dptr = System.Null_Address then
      raise No_Data;
    end if;
    Memcpy (K'Address, The_Key.Dptr, C_Types.Size_T (Key_Len));
    return K;
  end Next_Key;

end Ndbm;

