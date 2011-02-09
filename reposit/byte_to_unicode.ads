-- Convert a byte to a Unicode number accoding to a map
--  that is load from a xml definition
with Unicode;
package Byte_To_Unicode is

  -- Byte
  subtype Byte is Natural range 0 .. 16#FF#;

  type Map is tagged private;

  -- Load a map from file (or stdin if "")
  -- Raises File_Error if error accessing file
  -- Raises Parse_Error if incorrect or uncomplete table
  -- See byte2unicode.dtd
  File_Error : exception;
  Parse_Error : exception;
  procedure Load (The_Map : out Map; File_Name : in String);

  -- Return the Unicode corresponding to a given byte in the map
  function Convert (The_Map : Map;
                    Code : Byte) return Unicode.Unicode_Number;

private

  type Table_Array is array (Byte'Range) of Unicode.Unicode_Number;
  type Map is tagged record
    Table : Table_Array := (others => 0);
  end record;

end Byte_To_Unicode;

