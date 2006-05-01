-- The ada parser analyses the input flow, separates words, delimiters...
--  identifies and returns them through a callback.
-- It stops at the end of the input flow.
with Text_Char;
package Ada_Parser is

  -- The kinds of lexical elements
  -- Note that the following reserved words: access, delta, digits and range
  --  can also be "Identifier" when used as attribute designator
  type Lexical_Kind_List is (
    Separator,     -- Space, HorizTab, NewLine
    Delimiter,     -- & ' ( ) * + , - . / : ; < = > | .. ** := /= >= <= << >> <>
    Reserved_Word, -- from "abort" to "xor",
    Identifier,    -- identifier_letter {[underline] letter_or_digit}
    Numeric_Literal,   -- Based or decimal, integer or real
    Character_Literal, -- ' <character> '
    String_Literal,    -- " { <character> | "" } "
    Comment);      -- -- Text, up to NewLine

  type Parse_Callback is access
        procedure (Text : in String;
                   Lexic : in Lexical_Kind_List);

  -- Parse
  -- May raise Text_Char.End_Error if unexpected end of file
  procedure Parse (File : in Text_Char.File_Type;
                   Cb : in Parse_Callback);

  -- Syntax error detected
  Syntax_Error : exception;
end Ada_Parser;

