-- Read/write blocks of several Element_Type in file
private with System;
private with Ada.Finalization;
with Ada.Io_Exceptions;
generic
  type Element_Type is private;
package Bloc_Io is

  -- File to read/write
  type File_Type is tagged limited private;
  type File_Mode is (In_File, Inout_File, Out_File);

  -- Index of Element in file, or number of elements in a bloc
  type Count is range 0 .. Long_Integer'Last;
  subtype Positive_Count is Count range 1 .. Count'Last;

  -- The array of Elements read/written
  type Element_Array is array (Positive_Count range <>) of Element_Type;

  -- Create / Open / Close file
  -- May raise Status or Name_Error
  procedure Create(File : in out File_Type;
                   Name : in String);
  -- May raise Status or Name or Data_Error
  procedure Open(File : in out File_Type;
                 Mode : in File_Mode;
                 Name : in String);
  -- May raise Status or Device_Error
  procedure Close(File : in out File_Type);
  function Is_Open(File : in File_Type) return Boolean;

  -- Read Element_Array'Length Elements from file (at index From)
  -- or raise End_Error
  -- May raise Status, Device or End_Error
  procedure Read(File : in File_Type;
                 Item : in out Element_Array;
                 From : in Positive_Count);
  procedure Read(File : in File_Type;
                 Item : in out Element_Array);

  -- Write Element_Array'Length Elements to file (at index To)
  -- May increase size
  -- May raise Status or Device_Error
  procedure Write(File : in File_Type;
                  Item : in Element_Array;
                  To   : in Positive_Count);
  procedure Write(File : in File_Type;
                  Item : in Element_Array);


  -- Number of Elements in file
  -- May raise Status or Device_Error
  function Size(File : in File_Type) return Count;

  -- Current (next read/written Element) in file
  function Index(File : in File_Type) return Positive_Count;
  procedure Set_Index(File : in File_Type; To : in Positive_Count);

  -- Exceptions
  Status_Error : exception renames Ada.Io_Exceptions.Status_Error;
  Name_Error   : exception renames Ada.Io_Exceptions.Name_Error;
  Device_Error : exception renames Ada.Io_Exceptions.Device_Error;
  End_Error    : exception renames Ada.Io_Exceptions.End_Error;

private

  type File_Type is limited new Ada.Finalization.Limited_Controlled with record
    Ext_File : System.Address := System.Null_Address;
    Mode : File_Mode;
  end record;
  overriding procedure Finalize (File : in out File_Type);

end Bloc_Io;

