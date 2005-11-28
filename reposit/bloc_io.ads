-- Perform read/write of several Element_Type in file
with System;
with Ada.Io_Exceptions;
generic
  type Element_Type is private;
package Bloc_Io is

  type File_Type is limited private;

  type File_Mode is (In_File, Inout_File, Out_File);

  type Count is range 0 .. Long_Integer'Last;
  subtype Positive_Count is Count range 1 .. Count'Last;

  -- The array of Element read/written
  type Element_Array is array (Positive_Count range <>) of Element_Type;

  -- Create / Open / Close stuff
  -- May rase Status or Name_Error
  procedure Create(File : in out File_Type;
                   Name : in String);
  -- May rase Status or Name or Data_Error
  procedure Open(File : in out File_Type;
                 Mode : in File_Mode;
                 Name : in String);
  -- May rase Status or Device_Error
  procedure Close(File : in out File_Type);
  function Is_Open(File : in File_Type) return Boolean;

  -- Read Element_Array'Length Elements from file
  -- or raise End_Error
  -- May rase Status, Device or End_Error
  procedure Read(File : in File_Type;
                 Item : in out Element_Array;
                 From : in Positive_Count);
  procedure Read(File : in File_Type;
                 Item : in out Element_Array);

  -- Write Element_Array'Length Elements to file
  -- May increase size
  -- May rase Status or Device_Error
  procedure Write(File : in File_Type;
                  Item : in Element_Array;
                  To   : in Positive_Count);
  procedure Write(File : in File_Type;
                  Item : in Element_Array);


  -- Amount of Elements in file
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

  type File_Type is record
    Ext_File : System.Address := System.Null_Address;
    Mode : File_Mode;
  end record;

end Bloc_Io;

