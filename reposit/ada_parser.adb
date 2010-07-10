with Ada.Characters.Latin_1;
with Ada_Words, Upper_Char, Lower_Str, Mixed_Str;
package body Ada_Parser is

  -- Characters that deserve specific handling:
  -- New line
  Lf : constant Character := Ada.Characters.Latin_1.Lf;
  -- Carriage return (skipped)
  Cr : constant Character := Ada.Characters.Latin_1.Cr;

  -- Previous significant lexical element (not comment nor separator)
  -- for knowing if access, delta, digits or range are reserved words
  -- or qualifiers
  -- Set by Got_Text and used by Parse_Identifier
  Prev_Lex : Asu_Us;

  -- Set Char or string in a Us
  procedure Set (U : in out Asu_Us; C : in Character) is
  begin
    U := Asu_Tus (C & "");
  end Set;
  procedure Set (U : in out Asu_Us; S : in String) is
  begin
    U := Asu_Tus (S);
  end Set;

  -- Got a word. Save it as Prev_Lex if needed
  function Got_Text (Text : Asu_Us; Kind : Lexical_Kind_List) return Asu_Us is
  begin
    -- Save this lexical element if significant
    if Kind /= Separator and then Kind /= Comment then
      Prev_Lex := Text;
    end if;
    return Text;
  end Got_Text;
  function Got_Text (Text : String; Kind : Lexical_Kind_List) return Asu_Us is
  begin
    return Got_Text (Asu_Tus (Text), Kind);
  end Got_Text;
  function Got_Text (Text : Character; Kind : Lexical_Kind_List) return Asu_Us is
  begin
    return Got_Text (Asu_Tus (Text & ""), Kind);
  end Got_Text;

  -- Read next char, skipping Cr
  function Text_Char_Get (File : in Text_Char.File_Type) return Character is
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
                              Text : out Asu_Us;
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
        Asu.Append (Text, Cc);
      else
        exit;
      end if;
    end loop;
    -- For Cc we don't know
    Text_Char.Unget (File, Cc);

    -- End of identifier, check validity
    declare
      Str : constant String := Asu_Ts (Text);
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
        if Asu_Ts (Prev_Lex) = "'" then
          -- Prev was "'", so current is a qualifier
          Is_Reserved := Ada_Words.Is_Not_Keyword;
        else
          -- Prev was not "'", so current is a reserved word
          Is_Reserved := Ada_Words.Is_Keyword;
        end if;
      end if;
      if Is_Reserved = Ada_Words.Is_Keyword then
        Lexic := Reserved_Word;
        Text := Got_Text (Lower_Str (Str), Lexic);
      else
        Lexic := Identifier;
        Text := Got_Text (Mixed_Str (Str), Lexic);
      end if;
    end;
  end Parse_Identifier;

  -- C is a digit, so it starts a numeric literal
  -- so parse and return it
  function Parse_Numeric (C : Character;
                          File : Text_Char.File_Type) return Asu_Us is
    -- Current Char
    Cc : Character;
    -- Current text
    Text : Asu_Us;
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
        Asu.Append (Text, Cc);
      elsif Cc = '+' or else Cc = '-' then
        -- This is an operator ending the literal, except if after 'e' or 'E'
        if Upper_Char (Asu.Element (Text, Asu.Length (Text))) = 'E' then
          -- Sign after 'E' or 'e'
          Asu.Append (Text, Cc);
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
  function Parse_Comment (File : in Text_Char.File_Type) return Asu_Us is
    Cc : Character;
    Text : Asu_Us;
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
      Asu.Append (Text, Cc);
    end loop;
  end Parse_Comment;

  -- Internal parsing of one word
  -- Sets Text to "" (and Lexic to Separator) when end of file
  procedure Parse_Next (File : in Text_Char.File_Type;
                        Text : out Asu_Us;
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
      Text := Asu_Null;
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
      Text := Got_Text (Cc, Lexic);
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
        Text := Got_Text (''' & Nc & ''', Lexic);
      else
        -- For Nc and Nnc we don't know
        Text_Char.Unget (File, Nnc);
        Text_Char.Unget (File, Nc);
        -- Cc is a qualifier or attribute prefix
        Lexic := Delimiter;
        Text := Got_Text (Cc, Lexic);
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
        Text := Got_Text (Str2, Lexic);
      elsif Str2 = "--" then
        -- Skip Comments
        Lexic := Comment;
        Text := Got_Text (Parse_Comment (File), Lexic);
      else
        -- For Nc we don't know
        Text_Char.Unget (File, Nc);
        -- Cc is a single char delimiter
        Lexic := Delimiter;
        Text := Got_Text (Cc, Lexic);
      end if;
      return;
    end if;

    -- Identifer?
    Uc := Upper_Char (Cc);
    if Uc >= 'A' and then Uc <= 'Z' then
      Parse_Identifier (Cc, File, Text, Lexic);
      Text := Got_Text (Text, Lexic);
      return;
    end if;

    -- Numeric literal?
    if Cc >= '0' and then Cc <= '9' then
      Lexic := Numeric_Literal;
      Text := Got_Text (Parse_Numeric (Cc, File), Lexic);
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
        Asu.Append (Text, Cc);
        if Cc = Lf then
          -- Should not get Lf within string literal
          raise Syntax_Error;
        elsif Cc = Nc then
          -- End of String literal
          exit;
        end if;
      end loop;
      Lexic := String_Literal;
      Text := Got_Text (Text, Lexic);
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
    Lexic : Lexical_Kind_List;
    Text : Asu_Us;
  begin
    -- Loop until end of file
    loop
      Parse_Next (File, Text, Lexic);
      exit when Asu_Is_Null (Text);
      -- Call callback
      Cb (Asu_Ts (Text), Lexic);
    end loop;
  end Parse;

end Ada_Parser;

