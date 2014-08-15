with Ada.Characters.Latin_1;
package body Ada_Words is

  -- Ada separators
  function Is_Separator (C : Character) return Boolean is
  begin
    return C = ' ' or else C = Ada.Characters.Latin_1.Ht
                   or else C = Ada.Characters.Latin_1.Lf;
  end Is_Separator;

  -- Ada delimiters
  function Is_Delimiter (C : Character) return Boolean is
  begin
    case C is
      when '&' | ''' | '(' | ')' | '*' | '+' | ',' | '-' | '.' |
           '/' | ':' | ';' | '<' | '=' | '>' | '|' =>
        return True;
      when others =>
        return False;
    end case;
  end Is_Delimiter;

  -------------------------------------------------------------

  function Is_Upper_Letter (C : Character) return Boolean is
  begin
    return C in 'A' .. 'Z';
  end Is_Upper_Letter;

  function Is_Lower_Letter (C : Character) return Boolean is
  begin
    return C in 'a' .. 'z';
  end Is_Lower_Letter;

  function Is_Digit (C : Character) return Boolean is
  begin
    return C in '0' .. '9';
  end Is_Digit;

  function Is_Letter (C : Character) return Boolean is
  begin
    return Is_Lower_Letter (C) or else Is_Upper_Letter (C);
  end Is_Letter;


  -- Is Word a valid identifier
  function Is_Identifier (Word : String) return Boolean is
    First : constant Integer := Word'First;
    Prev : Character;
  begin
    if Word = "" then
      return False;
    end if;
    -- First has to be a letter
    if not Is_Letter (Word(First)) then
      return False;
    end if;

    -- Letter or digit or '_', never two '_' successively
    Prev := Word(First);
    for I in First+1 .. Word'Last loop
      if Word(I) = '_' and then Prev = '_' then
        return False;
      end if;
      Prev := Word(I);
      if not (Is_Letter (Prev)
      or else Is_Digit (Prev) or else Prev = '_') then
        return False;
      end if;
    end loop;
    -- Last must not be '_'
    if Prev = '_' then
      return False;
    end if;
    -- OK
    return True;
  end Is_Identifier;


end Ada_Words;

