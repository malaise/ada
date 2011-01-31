with Ada.Characters.Latin_1;
with Ada_Words, Upper_Char, Lower_Str, Mixed_Str;
package body Ada_Parser is

  -- Characters that deserve specific handling:
  -- New line
  Lf : constant Character := Ada.Characters.Latin_1.Lf;
  -- Carriage return (skipped)
  Cr : constant Character := Ada.Characters.Latin_1.Cr;

  -- Set Char or string in a Us
  procedure Set (U : in out As.U.Asu_Us; C : in Character) is
  begin
    U.Set (C & "");
  end Set;
  procedure Set (U : in out As.U.Asu_Us; S : in String) is
  begin
    U.Set (S);
  end Set;

  -- Got a word. Save it as Prev_Lex if needed
  procedure Get_Text (Text : in As.U.Asu_Us;
                      Kind : in Lexical_Kind_List;
                      Context : in out Parsing_Context;
                      Got : out As.U.Asu_Us) is
  begin
    -- Save this lexical element if significant
    if Kind /= Separator and then Kind /= Comment then
      Context.Prev_Lex := Text;
    end if;
    Got := Text;
  end Get_Text;
  procedure Get_Text (Text : in String;
                      Kind : in Lexical_Kind_List;
                      Context : in out Parsing_Context;
                      Got : out As.U.Asu_Us) is
  begin
    Get_Text (As.U.Tus (Text), Kind, Context, Got);
  end Get_Text;
  procedure Get_Text (Text : in Character;
                      Kind : in Lexical_Kind_List;
                      Context : in out Parsing_Context;
                      Got : out As.U.Asu_Us) is
  begin
    Get_Text (As.U.Tus (Text & ""), Kind, Context, Got);
  end Get_Text;

  -- Read next char, skipping Cr
  function Text_Char_Get (File : in Text_Char.File_Type)
                         return Character is
    C : Character;
  begin
    loop
      C := Text_Char.Get (File);
      exit when C /= Cr;
    end loop;
    return C;
  end Text_Char_Get;

  -- C is a letter, so it starts an identifier
  -- so parse and return it
  procedure Parse_Identifier (C : in Character;
                              File : in Text_Char.File_Type;
                              Context : in out Parsing_Context;
                              Text : out As.U.Asu_Us;
                              Lexic : out Lexical_Kind_List) is
    -- Current Char
    Cc : Character;
  begin
    Set (Text, C);

    loop
      -- Read as long as letter, digit or '_'
      Cc := Text_Char_Get (File);
      if      (Cc >= 'a' and then Cc <= 'z')
      or else (Cc >= 'A' and then Cc <= 'Z')
      or else (Cc >= '0' and then Cc <= '9')
      or else Cc = '_' then
        Text.Append (Cc);
      else
        exit;
      end if;
    end loop;
    -- For Cc we don't know
    Text_Char.Unget (File, Cc);

    -- End of identifier, check validity
    declare
      Str : constant String := Text.Image;
      Is_Reserved : Ada_Words.Keyword_Res_List;
      use type Ada_Words.Keyword_Res_List;
    begin
        -- Check that identifier is valid
      if not Ada_Words.Is_Identifier (Str) then
        raise Syntax_Error;
      end if;
      -- See if reserved word or normal identifier
      Is_Reserved := Ada_Words.Check_Keyword (Str);
      if Is_Reserved = Ada_Words.May_Be_Keyword then
        -- Access, delta, digits or range,
        -- see if prev significant lexical element is "'"
        if Context.Prev_Lex.Image = "'" then
          -- Prev was "'", so current is a qualifier
          Is_Reserved := Ada_Words.Is_Not_Keyword;
        else
          -- Prev was not "'", so current is a reserved word
          Is_Reserved := Ada_Words.Is_Keyword;
        end if;
      end if;
      if Is_Reserved = Ada_Words.Is_Keyword then
        Lexic := Reserved_Word;
        Get_Text (Lower_Str (Str), Lexic, Context, Text);
      else
        Lexic := Identifier;
        Get_Text (Mixed_Str (Str), Lexic, Context, Text);
      end if;
    end;
  end Parse_Identifier;

  -- C is a digit, so it starts a numeric literal
  -- so parse and return it
  function Parse_Numeric (C : Character;
                          File : Text_Char.File_Type) return As.U.Asu_Us is
    -- Current Char
    Cc : Character;
    -- Current text
    Text : As.U.Asu_Us;
  begin
    Set (Text, C);
    loop
      -- Read next char
      Cc := Text_Char_Get (File);
      -- As long as Cc is a digit,
      --  a letter a to f (or A to F) which includes e/E as exponent
      --  '.', '#', '_', "E+" or "E-", it belongs to the numeric literal
      if      (Cc >= '0' and then Cc <= '9')
      or else (Cc >= 'a' and then Cc <= 'f')
      or else (Cc >= 'A' and then Cc <= 'F')
      or else Cc = '.'
      or else Cc = '#'
      or else Cc = '_'
      or else Cc = 'E'
      or else Cc = 'e' then
        Text.Append (Cc);
      elsif Cc = '+' or else Cc = '-' then
        -- This is an operator ending the literal, except if after 'e' or 'E'
        if Upper_Char (Text.Element (Text.Length)) = 'E' then
          -- Sign after 'E' or 'e'
          Text.Append (Cc);
        else
          -- Operator + or - after the literal
          exit;
        end if;
      else
        -- This Cc does not belong to literal any more
        exit;
      end if;
    end loop;
    -- As soon as Cc does not belong, literal is ended
    -- And for Cc we don't know
    Text_Char.Unget (File, Cc);
    return Text;
  end Parse_Numeric;

  -- The "--" of comment has been parsed.
  -- Parse the remaining of comment and return full comment (prepending "--")
  function Parse_Comment (File : in Text_Char.File_Type) return As.U.Asu_Us is
    Cc : Character;
    Text : As.U.Asu_Us;
  begin
    -- Skip up to Lf
    Set (Text, "--");
    loop
      -- Check if comment ends by end of file
      if Text_Char.End_Of_File (File) then
        return Text;
      end if;
      -- Read next char and check if it the Lf ending the comment
      Cc := Text_Char_Get (File);
      if Cc = Lf then
        Text_Char.Unget (File, Cc);
        return Text;
      end if;
      -- Still in comment
      Text.Append (Cc);
    end loop;
  end Parse_Comment;

  -- Internal parsing of one word
  -- Sets Text to "" (and Lexic to Separator) when end of file
  procedure Parse_Next (File : in Text_Char.File_Type;
                        Context : in out Parsing_Context;
                        Text : out As.U.Asu_Us;
                        Lexic : out Lexical_Kind_List;
                        Raise_End : in Boolean := False) is
    -- Next and nextnext characters (read ahead)
    Cc, Nc, Nnc : Character;
    -- Upper char of Cc
    Uc : Character;
    -- Current text
    -- For Delimeters made of 2 chars
    Str2 : String (1 .. 2);
  begin
    -- Test if End of file
    if Text_Char.End_Of_File (File) then
      if Raise_End then
        raise End_Error;
      end if;
      Lexic := Separator;
      Text.Set_Null;
      return;
    end if;

    -- Read next char
    Cc := Text_Char_Get (File);

    -- Test each kind of lexical element, the most probable (frequent) first
    -- First separators because many spaces, then delimiters, identifiers
    -- comments and literals...
    -- But check for character literal before delimiters because ''' is used
    -- for both

    -- Separator?
    if Ada_Words.Is_Separator (Cc) then
      Lexic := Separator;
      Get_Text (Cc, Lexic, Context, Text);
      return;
    end if;

    -- Character literal or ''' as delimiter?
    if Cc = ''' then
      -- May be a char literal if "'.'": read 2 chars
      Nc := Text_Char_Get (File);
      Nnc := Text_Char_Get (File);
      if Nnc = ''' then
        -- A character literal, Cc, Nc and Nnc are consumed
        Lexic := Character_Literal;
        Get_Text (''' & Nc & ''', Lexic, Context, Text);
      else
        -- For Nc and Nnc we don't know
        Text_Char.Unget (File, Nnc);
        Text_Char.Unget (File, Nc);
        -- Cc is a qualifier or attribute prefix
        Lexic := Delimiter;
        Get_Text (Cc, Lexic, Context, Text);
      end if;
      return;
    end if;

    -- Delimiter?
    if Ada_Words.Is_Delimiter (Cc) then
      -- Check for single-char or double-char delimiter
      Nc := Text_Char_Get (File);
      Str2 := Cc & Nc;
      -- Check most likely first
      if      Str2 = ":=" or else Str2 = "=>" or else Str2 = ".."
      or else Str2 = "**" or else Str2 = "/=" or else Str2 = ">="
      or else Str2 = "<=" or else Str2 = "<<" or else Str2 = ">>" then
        Lexic := Delimiter;
        Get_Text (Str2, Lexic, Context, Text);
      elsif Str2 = "--" then
        -- Skip Comments
        Lexic := Comment;
        Get_Text (Parse_Comment (File), Lexic, Context, Text);
      else
        -- For Nc we don't know
        Text_Char.Unget (File, Nc);
        -- Cc is a single char delimiter
        Lexic := Delimiter;
        Get_Text (Cc, Lexic, Context, Text);
      end if;
      return;
    end if;

    -- Identifer?
    Uc := Upper_Char (Cc);
    if Uc >= 'A' and then Uc <= 'Z' then
      Parse_Identifier (Cc, File, Context, Text, Lexic);
      Get_Text (Text, Lexic, Context, Text);
      return;
    end if;

    -- Numeric literal?
    if Cc >= '0' and then Cc <= '9' then
      Lexic := Numeric_Literal;
      Get_Text (Parse_Numeric (Cc, File), Lexic, Context, Text);
      return;
    end if;

    -- String Literal?
    if Cc = '"' or else Cc = '%' then
      -- Start of String litteral, save delimiting character in Nc
      Nc := Cc;
      Set (Text, Cc);
      loop
        -- Read until same as Nc
        Cc := Text_Char_Get (File);
        Text.Append (Cc);
        if Cc = Lf then
          -- Should not get Lf within string literal
          raise Syntax_Error;
        elsif Cc = Nc then
          -- End of String literal
          exit;
        end if;
      end loop;
      Lexic := String_Literal;
      Get_Text (Text, Lexic, Context, Text);
      return;
    end if;

  exception
    when Text_Char.End_Error =>
      -- Unexpected end of file
      raise Syntax_Error;
  end Parse_Next;

  -- Parse --
  procedure Parse (File : in Text_Char.File_Type;
                   Cb : access
    procedure (Text : in String;
                   Lexic : in Lexical_Kind_List)) is
    Context : Parsing_Context;
    Lexic : Lexical_Kind_List;
    Text : As.U.Asu_Us;
  begin
    -- Loop until end of file
    loop
      Parse_Next (File, Context, Text, Lexic);
      exit when Text.Is_Null;
      -- Call callback
      Cb (Text.Image, Lexic);
    end loop;
  end Parse;

end Ada_Parser;

