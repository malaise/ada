with Ada.Text_Io, Ada.Characters.Latin_1;
with Hash, Lower_Str;
pragma Elaborate (Hash);
package body Ada_Words is

  function Is_Separator (C : Character) return Boolean is
  begin
    return C = ' ' or else C = Ada.Characters.Latin_1.Ht
                   or else C = Ada.Characters.Latin_1.Lf;
  end Is_Separator;

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

  subtype Word_Len_Range is Natural range 0 .. 15;
  type Word_Rec is record
    Len : Word_Len_Range;
    Str : String (1 .. Word_Len_Range'Last);
    Must : Boolean;
  end record;

  procedure Dump (Word : in Word_Rec) is
  begin
    Ada.Text_Io.Put (Word.Str (1 .. Word.Len) & " " & Word.Must'Img);
  end Dump;

  package Word_Hash is new Hash.Hash_Mng (Data_Access => Word_Rec,
                                          Dump => Dump);
  Hash_Table : Word_Hash.Hash_Table;



  procedure Store (Word : in String; Must_Be_Keyword : in Boolean := True) is
    Low_Word : constant String := Lower_Str (Word);
    Rec : Word_Rec;
  begin
    Rec.Len := Low_Word'Length;
    Rec.Str (1 .. Rec.Len) := Low_Word;
    Rec.Must := Must_Be_Keyword;
    Word_Hash.Store (Hash_Table, Low_Word, Rec);
  end Store;

  Table_Initialised : Boolean := False;

  procedure Init is
  begin
    if Table_Initialised then
      return;
    end if;

    Store ("abort");
    Store ("abs");
    Store ("abstract");
    Store ("accept");
    Store ("access", False);
    Store ("aliased");
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
    Store ("interface");
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
    Store ("overriding");

    Store ("package");
    Store ("pragma");
    Store ("private");
    Store ("procedure");
    Store ("protected");

    Store ("raise");
    Store ("range", False);
    Store ("record");
    Store ("rem");
    Store ("renames");
    Store ("requeue");
    Store ("return");
    Store ("reverse");

    Store ("select");
    Store ("separate");
    Store ("subtype");
    Store ("synchronized");

    Store ("tagged");
    Store ("task");
    Store ("terminate");
    Store ("then");
    Store ("type");

    Store ("until");
    Store ("use");

    Store ("when");
    Store ("while");
    Store ("with");

    Store ("xor");

    Table_Initialised := True;
  end Init;

  function Check_Keyword (Word : String) return Keyword_Res_List is
    Low_Word : constant String := Lower_Str (Word);
    Result : Word_Hash.Found_Rec;
  begin
    if Word = "" then
      return Is_Not_Keyword;
    end if;
    Init;
    -- Search in hash table
    Word_Hash.Reset_Find (Hash_Table, Low_Word);
    loop
      Word_Hash.Find_Next (Hash_Table, Low_Word, Result);
      case Result.Found is
        when False =>
          -- Word hash not found => not a keyword
          return Is_Not_Keyword;
        when True =>
          if Result.Data.Str (1 .. Result.Data.Len) = Low_Word then
            -- Word hash found, and word match otherwise search next word
            if Result.Data.Must then
              -- This word is a keyword
              return Is_Keyword;
            else
              -- This word is a keyword except if following a '''
              --  (like range, digits...)
              return May_Be_Keyword;
            end if;
          end if;
      end case;
    end loop;
  end Check_Keyword;

end Ada_Words;

