with Utf_8;
package Byte_To_Unicode is

  -- Byte
  subtype Byte is Natural range 0 .. 16#FF#;

  -- Unicode: Natural range 0 .. 16#10FFFF#;
  subtype Unicode_Number is Utf_8.Unicode_Number;

  type Map is tagged private;

  -- Load a table from file (or stdin if "")
  -- Raises File_Error is error accessing file
  -- Raises Parse_Error if incorrect or uncomplete table
  -- See byte2unicode.dtd
  File_Error : exception;
  Parse_Error : exception;
  procedure Load (The_Map : out Map; File_Name : in String);

  -- Returns the Unicode corresponding to a given byte in the table
  function Convert (The_Map : Map;
                    Code : Byte) return Unicode_Number;

private

  type Table_Array is array (Byte'Range) of Unicode_Number;
  type Map is tagged record
    Table : Table_Array := (others => 0);
  end record;

end Byte_To_Unicode;

