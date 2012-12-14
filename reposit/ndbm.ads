-- Interface to gdbm (dbm and ndbm) database manager
with System;
with Ada.Finalization;
generic
  type Key is private;
  type Data is private;
package Ndbm is
  type Database is tagged limited private;

  -- Open error
  Name_Error : exception;

  -- Database not open (or already open on open)
  Use_Error : exception;

  -- No data for key (on Read or Delete or First/Next key)
  No_Data : exception;

  -- Other (unexpected) dbm error
  Ndbm_Error : exception;

  -- Open/create the database for read-write
  procedure Open (Db : in out Database; File_Name : in String);
  -- Close the database
  procedure Close (Db : in out Database);
  -- Is database open
  function Is_Open (Db : Database) return Boolean;

  -- Insert/replace data at a specific key
  procedure Write (Db : in out Database; K : in Key; D : in Data);

  -- Read data at a specific key
  function Read (Db : Database; K : Key) return Data;
  -- Delete data at a specific key
  procedure Delete (Db : in out Database; K : in Key);

  -- First/Next key, may raise No_Data
  function First_Key (Db : Database) return Key;
  function Next_Key  (Db : Database) return Key;

private
  type Database is limited new Ada.Finalization.Limited_Controlled with record
    Acc : System.Address := System.Null_Address;
  end record;
  overriding procedure Finalize (Db : in out Database);
end Ndbm;

