with TEXT_IO;
with TEXT_HANDLER, MATH;
with DEBUG, INPUT_DISPATCHER, BOOL_IO, INTE_IO, REAL_IO;
package body PARSER is
  use MCD_MNG;


  TXT, TXTS : TEXT_HANDLER.TEXT(INPUT_DISPATCHER.MAX_STRING_LG);

  subtype ONE_WORD is STRING(1 .. 2);
  WORDS : constant array (MCD_MNG.OPERATOR_LIST) of ONE_WORD :=
  (ADD     => "+ ",
   SUB     => "- ",
   MULT    => "* ",
   DIV     => "/ ",
   REMIND  => "% ",
   POW     => "**",
   SWAP    => "<>",

   BITAND  => "&&",
   BITOR   => "||",
   BITXOR  => "^^",

   SHL     => "<<",
   SHR     => ">>",

   MINUS   => "+-",
   BITNEG  => "~~",

   EQUAL   => "= ",
   DIFF    => "/=",
   GREATER => "> ",
   SMALLER => "< ",
   GREATEQ => ">=",
   SMALLEQ => "<=",

   TOREAL  => "$ ",
   TOINTE  => "! ",

   ISREAL  => "?$",
   ISINTE  => "?!",

   BOLAND  => "& ",
   BOLOR   => "| ",
   BOLXOR  => "^ ",

   BOLNEG  => "~ ",

   DUP     => "><",
   POP     => "--",

   IFTE    => "? ",

   SSIZE   => ". ",
  
   POPR    => "->",
   COPYR   => "=>",
   PUSHR   => "<-",

   CALL    => "@ ",
   IFCALL  => "?@",
   RET     => "_ ",
   RETN    => "__",
   IFRETN  => "?_",

   FORMAT  => "//",
   PUT     => ", ",
   NEWL    => ": ",
   PUTL    => "; ");


  function NEXT_ITEM return MCD_MNG.ITEM_REC is
    LEVEL : NATURAL;
    ITEM_CHRS : MCD_MNG.ITEM_REC(MCD_MNG.CHRS);
    FIRST_WORD : BOOLEAN;
    W : ONE_WORD;
    C : CHARACTER;
    B : BOOLEAN;
    I : MATH.INTE;
    R : MATH.REAL;
    L : POSITIVE;
  begin

    TEXT_HANDLER.SET (TXT, INPUT_DISPATCHER.NEXT_WORD);
    if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.PARSER) then
      TEXT_IO.PUT_LINE ("Parser: Getting >"
               & TEXT_HANDLER.VALUE(TXT)  & "<");
    end if;

    -- EOF
    if TEXT_HANDLER.EMPTY(TXT) then
      if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.PARSER) then
        TEXT_IO.PUT_LINE ("Parser: Eof >");
      end if;
      return (KIND => OPER, VAL_OPER => RET);
    end if;

    C := TEXT_HANDLER.VALUE(TXT)(1);

    -- Parse [ or REGI
    if TEXT_HANDLER.LENGTH(TXT) = 1 then
      
      if C in 'a' .. 'z' or else C in 'A' .. 'Z' then
        -- A register
        return (KIND => MCD_MNG.REGI, VAL_REGI => C);
      elsif C = '[' then
        -- Get rid of strings
        TEXT_HANDLER.EMPTY(TXTS);
        FIRST_WORD := TRUE;
        LEVEL := 1;
        while LEVEL /= 0 loop
          TEXT_HANDLER.SET(TXT, INPUT_DISPATCHER.NEXT_WORD);
          if TEXT_HANDLER.VALUE(TXT) = "[" then
            LEVEL := LEVEL + 1;
          elsif TEXT_HANDLER.VALUE(TXT) = "]" then
            LEVEL := LEVEL - 1;
            exit when LEVEL = 0;
          end if;
          -- No space before first word
          if FIRST_WORD then
            FIRST_WORD  := FALSE;
          else
            TEXT_HANDLER.APPEND (TXTS, ' ');
          end if;
          TEXT_HANDLER.APPEND (TXTS, TXT);
        end loop;
        ITEM_CHRS.VAL_LEN := TEXT_HANDLER.LENGTH(TXTS);
        ITEM_CHRS.VAL_TEXT(1 .. ITEM_CHRS.VAL_LEN) := TEXT_HANDLER.VALUE(TXTS);
        return ITEM_CHRS;
      end if;

    end if;

    -- Parse OPER
    if TEXT_HANDLER.LENGTH(TXT) <= 2 then
      if TEXT_HANDLER.LENGTH(TXT) = 2 then
        W := TEXT_HANDLER.VALUE(TXT);
      else
        W(1) := TEXT_HANDLER.VALUE(TXT)(1);
        W(2) := ' ';
      end if;
      for O in MCD_MNG.OPERATOR_LIST loop
        if WORDS(O) = W then
          return (KIND => MCD_MNG.OPER, VAL_OPER => O);
        end if;
      end loop;
    end if;
     
    -- Parse INTE REAL BOOL
    begin
      BOOL_IO.GET(TEXT_HANDLER.VALUE(TXT), B, L);
      if L = TEXT_HANDLER.LENGTH(TXT) then
        return (KIND => MCD_MNG.BOOL, VAL_BOOL => B);
      end if;
    exception
      when others => null;
    end;
    begin
      INTE_IO.GET(TEXT_HANDLER.VALUE(TXT), I, L);
      if L = TEXT_HANDLER.LENGTH(TXT) then
        return (KIND => MCD_MNG.INTE, VAL_INTE => I);
      end if;
    exception
      when others => null;
    end;
    begin
      REAL_IO.GET(TEXT_HANDLER.VALUE(TXT), R, L);
      if L = TEXT_HANDLER.LENGTH(TXT) then
        return (KIND => MCD_MNG.REAL, VAL_REAL => R);
      end if;
    exception
      when others => null;
    end;
    
    -- Cannot recognise anything
    raise PARSING_ERROR;  
    
  exception
    when others =>
      raise PARSING_ERROR;  
  end NEXT_ITEM;


  procedure PRINT_HELP is
    use TEXT_IO;
  begin
    PUT_LINE ("Commands are read fromn standard input. No argument accepted.");
    PUT_LINE ("Item ::= <integer> <real> <boolean> <operator> <register> <string/subprogram>");
    PUT_LINE ("  <register>          ::= 'a' .. 'z'  | 'A' .. 'Z'");
    PUT_LINE ("  <string/subprogram> ::= '[' <text> ']'");
    PUT_LINE ("  <operators>         ::=");
    for O in MCD_MNG.OPERATOR_LIST loop
      PUT_LINE("       " & WORDS(O) & "  " & MCD_MNG.OPERATOR_LIST'IMAGE(O));
    end loop;
  end PRINT_HELP;
    

end PARSER;
