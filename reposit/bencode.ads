-- Bencoder library that encodes  Xml <-> Bencode
with C_Types;
package Bencode is

  -- The XML element and attributes names for each bencoded kind
  -- The root is an element named
  Bencode_Name : constant String := "Bencode";

  -- Integer is an element named
  Int_Name : constant String := "Int";
  --   with a text that is the integer image (in base 10)

  -- Array of bytes is an element named
  Bytes_Name : constant String := "Bytes";
  --   with an optional text composed of bytes in hexadecimal
  --   (so 2 digits per byte), in uppercase
  --   and an aoptional attribute named
  Str_Name : constant String := "Str";
  -- If all the bytes of the array are printable (from ' ' to '~') then,
  --   after decoding from Bencoded, the attribute contains the readable string
  -- If it is set for the for the conversion from Xml to Bencoded,
  --   then it is used instead of the text

  -- List is an element named
  List_Name : constant String := "List";
  --   containing elements

  -- Dictionary is an element named
  Dictio_Name : constant String := "Dictio";
  --  containing pairs of elements


  -- The Bencoded array of bytes
  subtype Byte is C_Types.Byte;
  type Byte_Array is array (Positive range <>) of Byte;


  -- Option Dictio_Keys drives checks of the keys of the dictionaries:
  type Dictio_Keys_Policy is (
    Check, -- Check that Dictio keys are Bytes and appear in crescent order
    Sort,  -- Check that Dictio keys are Bytes and sort them in crescent order
    None); -- No check and no sort

  -- Decode a Bencoded byte array into a Xml string
  function Bencode2Xml (Ben_Array : Byte_Array;
                        Keys_Policy : Dictio_Keys_Policy := None)
           return String;

  -- Encode a Xml string into a Bencoded byte array
  function Xml2Bencode (Xml_String : String;
                        Keys_Policy : Dictio_Keys_Policy := None)
           return Byte_Array;

  -- When invalid Bencode or Xml format of the input
  -- The error is logged throught the Trace system
  Format_Error : exception;

end Bencode;

