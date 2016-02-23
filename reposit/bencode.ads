-- Bencoder library that encodes  Xml <-> Bencode
with C_Types;
package Bencode is

  -- The XML element and attributes names for each bencoded kind

  -- The root is an element
  Bencode_Name : constant String := "Bencode";

  -- Integer is an element
  Int_Name : constant String := "Int";
  --   with a text that is the integer image (in base 10)

  -- Array of bytes is an element
  Bytes_Name : constant String := "Bytes";
  --   with a text composed of bytes in hexadecimal
  --   (so 2 digits per byte), in uppercase
  -- If all the bytes of the array are printable (from ' ' to '~') then,
  --   after decoding from Bencoded, the attribute contains the readable string
  -- If it is set for the for the conversion from Xml to Bencoded,
  --   then it is used instead of the text
  Str_Name : constant String := "Str";

  -- List is an element
  List_Name : constant String := "List";
  --   containing elements

  -- Dictionary is an element
  Dictio_Name : constant String := "Dictio";
  --  containing pairs of elements


  -- The Bencoded bytes array
  subtype Byte is C_Types.Byte;
  type Byte_Array is array (Positive range <>) of Byte;


  -- Option Check_Dictio enables the check that Dictio keys are Bytes and
  -- that they appear in crescent order

  -- Encode a Xml string into a Bencoded byte array
  function Xml2Bencode (Xml_Stream : String;
                        Check_Dictio : Boolean := True) return Byte_Array;

  -- Decode a Bencoded byte array into a Xml stream
  function Bencode2Xml (Ben_Stream : Byte_Array;
                        Check_Dictio : Boolean := True) return String;

  -- When invalid Bencode or Xml format of the input
  -- The error is logged throught the Trace system
  Format_Error : exception;

end Bencode;

