with Ada.Text_Io;
with Hash, Lower_Str;
package body Ada_Words is

  function Is_Separator (C : Character) return Boolean is
  begin
    return C = ' ' or else C = Ascii.Ht or else C = Ascii.Lf;
  end Is_Separator;

  -------------------------------------------------------------

  subtype Word_Len_Range is Natural range 0 .. 9;
  type Word_Rec is record
    Len : Word_Len_Range;
    Str : String (1 .. Word_Len_Range'Last);
  end record;

  procedure Dump (Word : in Word_Rec) is
  begin
    Ada.text_Io.Put (Word.Str (1 .. Word.Len));
  end Dump;

  package Word_Hash is new Hash.Hash_Mng (Data_Acess => Word_Rec,
                                          Dump => Dump);



  procedure Store (Word : in String) is
    Low_Word : constant String := Lower_Str (Word);
    Rec : Word_Rec;
  begin
    Rec.Len := Low_Word'Length;
    Rec.Str (1 .. Rec.Len) := Low_Word;
    Word_Hash.Store (Low_Word, Rec);
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
    Store ("access");
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
    Store ("delta");
    Store ("digits");
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

    Store ("package");
    Store ("pragma");
    Store ("private");
    Store ("procedure");
    Store ("protected");

    Store ("raise");
    Store ("range");
    Store ("record");
    Store ("rem");
    Store ("renames");
    Store ("requeue");
    Store ("return");
    Store ("reverse");

    Store ("select");
    Store ("separate");
    Store ("subtype");

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

  function Is_Keyword (Word : String) return Boolean is
    Low_Word : constant String := Lower_Str (Word);
    Result : Word_Hash.Found_Rec;
  begin
    Init;
    Word_Hash.Reset_Find (Low_Word);
    loop
      Result := Word_Hash.Find_Next (Low_Word);
      case Result.Found is
        when False =>
          return False;
        when True =>
          if Result.Data.Str (1 .. Result.Data.Len) = Low_Word then
            return True;
          end if;
      end case;
    end loop;
  end Is_Keyword;

end Ada_Words;

