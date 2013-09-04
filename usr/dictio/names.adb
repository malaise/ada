with Parser, Ada_Words;
with Data_Base, Dictio_Debug;
package body Names is

  -- For parsing
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Sep;
  end Is_Sep;
  Sep_Access : constant Parser.Separing_Function := Is_Sep'Access;

  -- Max name length
  Max_Length : constant Positive := Data_Base.Item_Name'Length;


  -- Syntax of name for Get/Set
  --   <ident> [ { .<ident> } ]
  function Is_Valid_Name (Name : String) return Boolean is
    Iter : Parser.Iterator;
    Ok : Boolean;
  begin

    -- Parse words
    Parser.Set (Iter, Name, Sep_Access);
    Ok := True;
    for I in 1 .. Max_Length loop
      declare
        Word : constant String := Parser.Next_Word (Iter);
      begin
        -- Check first word, it must be identifier
        --  and not have prepended sep (.foo)
        if I = 1 then
          if not Ada_Words.Is_Identifier (Word)
          or else Parser.Prev_Separators (Iter) /= "" then
            Ok := False;
            exit;
          end if;
        else

          -- Check if last word, it must not have prepended sep (foo.)
          -- Anyway this is the end
          if Word = "" then
            Ok := Parser.Prev_Separators (Iter) = "";
            exit;
          end if;

          -- Other word, it must be identifier and have one prepeded sep
          if not Ada_Words.Is_Identifier (Word)
          or else Parser.Prev_Separators (Iter) /= Sep_Str then
            Ok := False;
            exit;
          end if;
        end if;
      end;
    end loop;

    Parser.Del (Iter);
    return Ok;
  end Is_Valid_Name;


  -- Syntax of name for (Un) Notify is
  -- either  <anything>
  -- or <ident_or_anyid> [ { . <ident_or_anyid> } ] [ . <anything> ]
  -- anyid is "*" and anything is "**"

  Anything  : constant String :=  "**";
  Any_Ident : constant String :=  "*";

  function Is_Valid_Notify (Criteria : String) return Boolean is
    Iter : Parser.Iterator;
    Ok : Boolean;
    Must_Be_Last : Boolean;
  begin

   -- Check Anything first
   if Criteria = Anything then
     return True;
   end if;


    -- Parse words
    Parser.Set (Iter, Criteria, Sep_Access);
    Ok := True;
    Must_Be_Last := False;
    for I in 1 .. Max_Length loop
      declare
        Word : constant String := Parser.Next_Word (Iter);
      begin
        -- Check first word, it must not have prepended sep (.foo)
        if I = 1 then
          if (Word /= Any_Ident
              and then not Ada_Words.Is_Identifier (Word))
          or else Parser.Prev_Separators (Iter) /= "" then
            Ok := False;
            exit;
          end if;
        else

          -- Check if last word, it must not have prepended sep (foo.)
          -- Anyway this is the end
          if Word = "" then
            Ok := Parser.Prev_Separators (Iter) = "";
            exit;
          end if;

          -- Other word, it must be identifier or a widcard
          -- and have one prepeded sep and not follow Anything
          if (Word /= Any_Ident
              and then Word /= Anything
              and then not Ada_Words.Is_Identifier (Word))
          or else Parser.Prev_Separators (Iter) /= Sep_Str
          or else Must_Be_Last then
            Ok := False;
            exit;
          end if;

          -- Anything must be last word
          if Word = Anything then
            Must_Be_Last := True;
          end if;
        end if;
      end;
    end loop;

    Parser.Del (Iter);
    return Ok;
  end Is_Valid_Notify;


  -- Do item match criteria
  function Match (Name, Criteria : String) return Boolean is
    Iter_Name, Iter_Crit : Parser.Iterator;
    Ok : Boolean;
  begin

    -- Parse words and compare
    Parser.Set (Iter_Name, Name,     Sep_Access);
    Parser.Set (Iter_Crit, Criteria, Sep_Access);
    loop
      declare
        Word_Name : constant String := Parser.Next_Word (Iter_Name);
        Word_Crit : constant String := Parser.Next_Word (Iter_Crit);
      begin
        -- Match as soon as crit has Anything
        if Word_Crit = Anything then
          Ok := True;
          exit;
        end if;

        -- Check if end of words
        if Word_Name = "" and then Word_Crit = "" then
          -- Match if end of both
          Ok := True;
          exit;
        elsif Word_Name = "" and then Word_Crit /= "" then
          -- Mismatch if end of name and not end of crit
          -- foo and foo.bar
          Ok := False;
          exit;
        elsif Word_Name /= "" and then Word_Crit = "" then
          -- Mismatch if not end of name and end of crit
          -- foo.bar and foo
          Ok := False;
          exit;
        elsif Word_Crit /= Any_Ident and then Word_Name /= Word_Crit then
          -- None end, mismatch if crit is neither Any_Ident nor name
          Ok := False;
          exit;
        end if;

      end;
    end loop;

    Dictio_Debug.Put (Dictio_Debug.Client_Notify,
                      "Client-notify-names: " & Name & " match " & Criteria
                    & " : " & Ok'Img);

    Parser.Del (Iter_Name);
    Parser.Del (Iter_Crit);
    return Ok;
  end Match;

end Names;

