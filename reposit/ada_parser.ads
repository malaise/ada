-- The ada parser analyses the input flow, separates words, delimiters...
--  identifies and returns them through a callback.
-- It stops at the end of the input flow.
with As.U, Text_Char;
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

  -- Reserved words are returned in lowercase
  -- Idendifiers are returned in Mixed_Case
  -- Numeric literals are returned in UPPERCASE

  -- Parse flow of File until end of file
  -- Call callback for each lexical element
  -- May raise Syntax_Error
  type Parse_Callback is access
        procedure (Text : in String;
                   Lexic : in Lexical_Kind_List);

  procedure Parse (File : in out Text_Char.File_Type;
                   Cb : access procedure (
                     Text : in String;
                     Lexic : in Lexical_Kind_List));


  -- Parse flow of File until next lexical element
  -- Set Text to "" (and Lexic to Separator) when end of file
  --  or raises End_Error
  -- May raise Syntax_Error
  type Parsing_Context is private;
  procedure Parse_Next (File : in out Text_Char.File_Type;
                        Context : in out Parsing_Context;
                        Text : out As.U.Asu_Us;
                        Lexic : out Lexical_Kind_List;
                        Raise_End : in Boolean := False);

  -- Syntax error detected
  Syntax_Error : exception;

  -- Raise_End set and end of file reached
  End_Error : exception;

private
  type Parsing_Context is record
    -- Previous significant lexical element (not comment nor separator)
    -- for knowing if access, delta, digits or range are reserved words
    -- or qualifiers
    -- Set by Got_Text and used by Parse_Identifier
    Prev_Lex : As.U.Asu_Us;
  end record;
end Ada_Parser;

