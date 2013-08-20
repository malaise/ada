with Ada.Characters.Latin_1;
with As.B, Hash, Lower_Str;
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
    return C >= 'A' and then C <= 'Z';
  end Is_Upper_Letter;

  function Is_Lower_Letter (C : Character) return Boolean is
  begin
    return C >= 'a' and then C <= 'z';
  end Is_Lower_Letter;

  function Is_Digit (C : Character) return Boolean is
  begin
    return C >= '0' and then C <= '9';
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

  -------------------------------------------------------------

  -- Ada reserved word
  Word_Max_Len : constant := 12;
  type Word_Rec is record
    Str : As.B.Asb_Bs (Word_Max_Len);
    -- Some reserved words can be not keyword (ex: access, digits...)
    Must : Boolean;
    -- Some reserved words were introduced after ADA 83
    Version : Language_Versions;
  end record;

  -- Hash table of Ada reserved words
  package Word_Hash is new Hash.Hash_Mng (Data_Access => Word_Rec);
  Hash_Table : Word_Hash.Hash_Table;

  -- Adds a reserved Word in the table
  procedure Store (Word : in String;
                   Must_Be_Keyword : in Boolean := True;
                   Version : in Language_Versions := Ada83) is
    Low_Word : constant String := Lower_Str (Word);
    Rec : Word_Rec;
  begin
    Rec.Str.Set (Low_Word);
    Rec.Must := Must_Be_Keyword;
    Rec.Version := Version;
    Word_Hash.Store (Hash_Table, Low_Word, Rec);
  end Store;

  Table_Initialised : Boolean := False;

  -- Init the whole table of reserved words
  procedure Init is
  begin
    if Table_Initialised then
      return;
    end if;

    Store ("abort");
    Store ("abs");
    Store ("abstract", Version => Ada95);
    Store ("accept");
    Store ("access", False);
    Store ("aliased", Version => Ada95);
    Store ("all");
    Store ("and");
    Store ("array");
    Store ("at");

    Store ("begin");
    Store ("body");

    Store ("case");
    Store ("constant");

    Store ("declare");
    Store ("delay");
    Store ("delta", False);
    Store ("digits", False);
    Store ("do");

    Store ("else");
    Store ("elsif");
    Store ("end");
    Store ("entry");
    Store ("exception");
    Store ("exit");

    Store ("for");
    Store ("function");

    Store ("generic");
    Store ("goto");

    Store ("if");
    Store ("in");
    Store ("interface", Version => Ada2005);
    Store ("is");

    Store ("limited");
    Store ("loop");

    Store ("mod");

    Store ("new");
    Store ("not");
    Store ("null");

    Store ("of");
    Store ("or");
    Store ("others");
    Store ("out");
    Store ("overriding", Version => Ada2005);

    Store ("package");
    Store ("pragma");
    Store ("private");
    Store ("procedure");
    Store ("protected", Version => Ada95);

    Store ("raise");
    Store ("range", False);
    Store ("record");
    Store ("rem");
    Store ("renames");
    Store ("requeue", Version => Ada95);
    Store ("return");
    Store ("reverse");

    Store ("select");
    Store ("separate");
    Store ("some", Version => Ada2012);
    Store ("subtype");
    Store ("synchronized", Version => Ada2005);

    Store ("tagged", Version => Ada95);
    Store ("task");
    Store ("terminate");
    Store ("then");
    Store ("type");

    Store ("until", Version => Ada95);
    Store ("use");

    Store ("when");
    Store ("while");
    Store ("with");

    Store ("xor");

    Table_Initialised := True;
  end Init;

  -- Check if word is a reserved keyword
  function Check_Keyword (Word : String;
                          Version : Language_Versions := Default_Version)
                         return Keyword_Res_List is
    Low_Word : constant String := Lower_Str (Word);
    Result : Word_Hash.Found_Rec;
  begin
    if Word = "" then
      return False;
    end if;
    Init;
    -- Search in hash table
    Word_Hash.Reset_Find (Hash_Table, Low_Word);
    loop
      Word_Hash.Find_Next (Hash_Table, Low_Word, Result);
      case Result.Found is
        when False =>
          -- Word hash not found => not a keyword
          return False;
        when True =>
          if Result.Data.Str.Image = Low_Word then
            -- Word hash found, and word match otherwise search next word
            return (
              -- Found but in a later version than requested
              if Result.Data.Version > Version then False
              -- This word is a keyword
              elsif Result.Data.Must then True
              -- It is a keyword except if following a ''' (ex range, digits...)
              else Maybe);
          end if;
      end case;
    end loop;
  end Check_Keyword;

end Ada_Words;

