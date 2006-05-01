with Ada.Strings.Unbounded, Ada.Characters.Latin_1;
with Ada_Words, Upper_Char, Lower_Str, Mixed_Str;
package body Ada_Parser is

  -- Characters that deserve specific handling
  -- Convention for "No character"
  Nul : constant Character := Ada.Characters.Latin_1.Nul;
  -- New line
  Lf : constant Character := Ada.Characters.Latin_1.Lf;

  -- Unbounded strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Us is Asu.Unbounded_String;

  -- Previous significant lexical element (not comment nor separator)
  Prev_Lex : Us;

  -- Set Char or string in a Us
  procedure Set (U : in out Us; C : in Character) is
  begin
    U := Asu.To_Unbounded_String (C & "");
  end Set;
  procedure Set (U : in out Us; S : in String) is
  begin
    U := Asu.To_Unbounded_String (S);
  end Set;

  -- Word identified as Kind
  procedure Got_Word (Word : in Us;
                      Kind : in Lexical_Kind_List;
                      Cb : in Parse_Callback) is
  begin
    -- Publish this result
    Cb (Asu.To_String (Word), Kind);
    -- Save this lexical element if significant
    if Kind /= Separator and then Kind /= Comment then
      Prev_Lex := Word;
    end if;
  end Got_Word;
  procedure Got_Word (Word : in String;
                      Kind : in Lexical_Kind_List;
                      Cb : in Parse_Callback) is
  begin
    Got_Word (Asu.To_Unbounded_String (Word), Kind, Cb);
  end Got_Word;
  procedure Got_Word (Word : in Character;
                      Kind : in Lexical_Kind_List;
                      Cb : in Parse_Callback) is
  begin
    Got_Word (Asu.To_Unbounded_String (Word & ""), Kind, Cb);
  end Got_Word;

  -- If Cc is a digit, then it starts a numeric literal
  -- so parsed it, otherwise unget it
  function Parse_Numeric (C : in Character;
                          File : in Text_Char.File_Type;
                          Cb : in Parse_Callback) return Boolean is
    -- Current Char
    Cc : Character;
    -- Current text
    Cur_Text : Us;
  begin
    if C < '0' or else C > '9' then
      -- Not a numeric literal
      return False;
    end if;

    Cc := C;
    loop
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
        Asu.Append (Cur_Text, Cc);
      elsif Cc = '+' or else Cc = '-' then
        -- This is an operator ending the literal, except
        --  if after 'e' or 'E'
        -- This cannot be first char of Cur_Text
        if Upper_Char (Asu.Element (Cur_Text, Asu.Length (Cur_Text))) = 'E' then
          Asu.Append (Cur_Text, Cc);
        else
          exit;
        end if;
      else
        exit;
      end if;
      -- Read next char
      Cc := Text_Char.Get (File);
    end loop;
    -- As soon as Cc does not belong, literal is ended
    Got_Word (Cur_Text, Numeric_Literal, Cb);
    -- And for Cc we don't know
    Text_Char.Unget (File, Cc);
    return True;
  end Parse_Numeric;


  -- Parse --
  procedure Parse (File : in Text_Char.File_Type;
                   Cb : in Parse_Callback) is
    -- Current, next and nextnext cahracters (read ahead)
    Cc, Nc, Nnc : Character;
    -- Current text
    Cur_Text : Us;
    -- For Delimeters made of 2 chars
    Str2 : String (1 .. 2);
  begin
    while not Text_Char.End_Of_File (File) loop
      -- Read next char
      Cc := Text_Char.Get (File);

      -- First, handle the situation when one char (Cc) may not be enough
      -- Comment, char literal, delimiter for "2 chars delimiters"
      -- If parsed, set CC to nul, else let CC unchaged
      if Cc = '-' then
        -- May be a comment, if "--"
        Nc := Text_Char.Get (File);
        if Nc = '-' then
          -- This is a comment, skip up to Lf
          Set (Cur_Text, "--");
          loop
            -- Check that comments does not end by end of file
            if Text_Char.End_Of_File (File) then
              -- Simulate a Lf
              Cc := Lf;
              exit;
            end if;
            Cc := Text_Char.Get (File);
            exit when Cc = Lf;
            Asu.Append (Cur_Text, Cc);
          end loop;
          -- Now Cur_Text contains the comment and Cc is Lf
          Got_Word (Cur_Text, Comment, Cb);
          Got_Word (Cc, Separator, Cb);
          Cc := Nul;
        else
          -- Cc is either a unary or binary operator
          Got_Word (Cc, Delimiter, Cb);
          -- For Nc we don't know
          Text_Char.Unget (File, Nc);
          Cc := Nul;
        end if;
      elsif Cc = ''' then
        -- May be a char literal if "'.'": read 2 chars
        Nc := Text_Char.Get (File);
        Nnc := Text_Char.Get (File);
        if Nnc = ''' then
          -- A char, Cc has been consumed
          Got_Word (''' & Nc & ''', Character_Literal, Cb);
          Cc := Nul;
        else
          -- Cc is a qualifier or attribute prefix
          Got_Word (Cc, Delimiter, Cb);
          -- For Nnc and Nc we don't know
          Text_Char.Unget (File, Nnc);
          Text_Char.Unget (File, Nc);
          Cc := Nul;
        end if;
      elsif Ada_Words.Is_Delimiter (Cc) then
        -- Check for single-char or double-char delimiter
        Nc := Text_Char.Get (File);
        Str2 := Cc & Nc;
        -- Check most likely first
        if      Str2 = ":=" or else Str2 = "=>" or else Str2 = ".."
        or else Str2 = "**" or else Str2 = "/=" or else Str2 = ">="
        or else Str2 = "<=" or else Str2 = "<<" or else Str2 = ">>" then
          Got_Word (Str2, Delimiter, Cb);
          Cc := Nul;
        else
          -- Cc is a single char delimiter
          Got_Word (Cc, Delimiter, Cb);
          -- For Nc we don't know
          Text_Char.Unget (File, Nc);
          Cc := Nul;
        end if;
      end if;

      -- From now, Cc itself should be enough to dentify the kind
      if Ada_Words.Is_Separator (Cc) then
        Got_Word (Cc, Separator, Cb);
      elsif Parse_Numeric (Cc, File, Cb) then
        -- Numeric litteral parsed Ok
        null;
      elsif Cc = '"' or else Cc = '%' then
        -- Start of String litteral
        Set (Cur_Text, Cc);
        Nc := Cc;
        loop
          -- Read until same as CC
          Cc := Text_Char.Get (File);
          Asu.Append (Cur_Text, Cc);
          if Cc = Lf then
            -- Should not get Lf within string literal
            raise Syntax_Error;
          elsif Cc = Nc then
            -- End of String literal
            Got_Word (Cur_Text, String_Literal, Cb);
            exit;
          end if;
        end loop;
        Cc := Nul;
      elsif Cc = Nul then
        -- Skip this loop
        null;
      else
        -- Must be an Identifer
        Set (Cur_Text, Cc);
        loop
          -- Read as long as letter, digit or '_'
          Cc := Text_Char.Get (File);
          if      (Cc >= 'a' and then Cc <= 'z')
          or else (Cc >= 'A' and then Cc <= 'Z')
          or else (Cc >= '0' and then Cc <= '9')
          or else Cc = '_' then
            Asu.Append (Cur_Text, Cc);
          else
            -- End of identifier
            exit;
          end if;
        end loop;
        -- End of identifier
        declare
          Str : constant String := Asu.To_String (Cur_Text);
          Is_Reserved : Ada_Words.Keyword_Res_List;
          use type Ada_Words.Keyword_Res_List;
        begin
          -- Check that identifier is valid
          if not Ada_Words.Is_Identifier (Str) then
            raise Syntax_Error;
          end if;
          -- See if reserved word
          Is_Reserved := Ada_Words.Check_Keyword (Str);
          if Is_Reserved = Ada_Words.May_Be_Keyword then
            -- Access, delta, digits or range,
            -- see if prev significant lexical element is "'"
            if Asu.To_String (Prev_Lex) = "'" then
              -- Prev was "'", so current is a qualifier
              Is_Reserved := Ada_Words.Is_Not_Keyword;
            else
              -- Prev was not "'", so current is a reserved word
              Is_Reserved := Ada_Words.Is_Keyword;
            end if;
          end if;
          if Is_Reserved = Ada_Words.Is_Keyword then
            Got_Word (Lower_Str (Str), Reserved_Word, Cb);
          else
            Got_Word (Mixed_Str (Str), Identifier, Cb);
          end if;
          -- For Cc we don't know
          Text_Char.Unget (File, Cc);
          Cc := Nul;
        end;
      end if;
        
    end loop;

  exception
    when Text_Char.End_Error =>
      raise;
  end Parse;


end Ada_Parser;

