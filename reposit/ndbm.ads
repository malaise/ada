-- Interface to gdbm (dbm and ndbm) database manager
generic
  type Key is private;
  type Data is private;
  File_Name : in String;
package Ndbm is

  -- Open error
  Name_Error : exception;

  -- Database not open (or already open on open)
  Use_Error : exception;

  -- No data for key (on Read or Delete or First/Next key)
  No_Data : exception;

  -- Other (unexpected) dbm error
  Ndbm_Error : exception;

  -- Open/create the database for read-write
  procedure Open;
  -- Close the database
  procedure Close;
  -- Is database open
  function Is_Open return Boolean;

  -- Insert/replace data at a specific key
  procedure Write (K : in Key; D : in Data);

  -- Read data at a specific key
  function Read (K : in Key) return Data;
  -- Delete data at a specific key
  procedure Delete (K : in Key);

  -- First/Next key, may raise No_Data
  function First_Key return Key;
  function Next_Key  return Key;

end Ndbm;

