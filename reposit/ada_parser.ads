-- The ada parser analyses the input flow, separates words, delimiters...
--  identifies and returns them through a callback.
-- It stops at the end of the input flow.
with As.U; use As.U;
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

  -- Parse flow of File until end of file
  -- Call callback for each lexical element
  -- May raise Syntax_Error
  type Parse_Callback is access
        procedure (Text : in String;
                   Lexic : in Lexical_Kind_List);

  procedure Parse (File : in Text_Char.File_Type;
                   Cb : access
    procedure (Text : in String;
               Lexic : in Lexical_Kind_List));

  -- Parse flow of File until next lexical element
  -- Set Text to "" (and Lexic to Separator) when end of file
  --  or raises End_Error
  -- May raise Syntax_Error
  procedure Parse_Next (File : in Text_Char.File_Type;
                        Text : out Asu_Us;
                        Lexic : out Lexical_Kind_List;
                        Raise_End : in Boolean := False);

  -- Syntax error detected
  Syntax_Error : exception;

  -- Raise_End set and end of file reached
  End_Error : exception;

end Ada_Parser;

