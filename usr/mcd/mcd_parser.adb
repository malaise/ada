with TEXT_IO;
with TEXT_HANDLER, MATH, QUEUES, SYS_CALLS;
with DEBUG, INPUT_DISPATCHER, BOOL_IO, INTE_IO, REAL_IO;
package body PARSER is
  use MCD_MNG;

  subtype ITEM_CHRS_REC is MCD_MNG.ITEM_REC(MCD_MNG.CHRS);
  package INSTR_STACK is new QUEUES.CIRC(7, ITEM_CHRS_REC);

  TXT, TXTS : TEXT_HANDLER.TEXT(INPUT_DISPATCHER.MAX_STRING_LG);

  INPUT_ERROR : BOOLEAN := FALSE;

  subtype ONE_WORD is STRING(1 .. 2);
  subtype ONE_COMMENT is STRING (1 .. 20);
  type ONE_REC is record
    WORD : ONE_WORD;
    COMMENT : ONE_COMMENT;
  end record;

  WORDS : constant array (MCD_MNG.OPERATOR_LIST) of ONE_REC :=
  (ADD     => ("+ ", "push A + B          "),
   SUB     => ("- ", "push A - B          "),
   MULT    => ("* ", "push A * B          "),
   DIV     => ("/ ", "push A / B          "),
   REMIND  => ("% ", "push A  % B         "),
   POW     => ("**", "push A ** B         "),
   SWAP    => ("<>", "push B, push A      "),

   BITAND  => ("&&", "push A & B          "),
   BITOR   => ("||", "push A | B          "),
   BITXOR  => ("^^", "push A ^ B          "),
 
   SHL     => ("<<", "push A << B         "),
   SHR     => (">>", "push A >> B         "),

   MINUS   => ("+-", "push -A             "),
   BITNEG  => ("~~", "push ~A             "),

   EQUAL   => ("= ", "push A = B          "),
   DIFF    => ("/=", "push A /= B         "),
   GREATER => ("> ", "push A > B          "),
   SMALLER => ("< ", "push A < B          "),
   GREATEQ => (">=", "push A >= B         "),
   SMALLEQ => ("<=", "push A <= B         "),

   TOREAL  => ("$ ", "push REAL(A)        "),
   TOINTE  => ("! ", "push INTE(A)        "),

   ISREAL  => ("?$", "push IS_REAL(A)     "),
   ISINTE  => ("?!", "push IS_INTE(A)     "),

   OBASE   => (">#", "set output base     "),

   BOLAND  => ("& ", "push A and B        "),
   BOLOR   => ("| ", "push A or B         "),
   BOLXOR  => ("^ ", "push A xor B        "),

   BOLNEG  => ("~ ", "push not A          "),
 
   DUP     => ("><", "push A, push A      "),
   POP     => ("--", "pop                 "),

   IFTE    => ("? ", "if A then B else C  "),

   SSIZE   => (". ", "push stack_size     "),
  
   POPR    => ("->", "A -> regB           "),
   COPYR   => ("=>", "A -> regB, push A   "),
   PUSHR   => ("<-", "push regA           "),

   CALL    => ("@ ", "call A              "),
   IFCALL  => ("?@", "if A then call B    "),
   RET     => ("_ ", "return              "),
   RETN    => ("__", "return A levels     "),
   IFRETN  => ("?_", "if A return B levels"),
   RETACAL => ("_@", "return and call A   "),

   FORMAT  => ("//", "xx or xx.yyy fmt    "),
   PUT     => (", ", "put A               "),
   NEWL    => (": ", "new line            "),
   PUTL    => ("; ", "put_line A          "));


  function NEXT_ITEM return MCD_MNG.ITEM_REC is
    LEVEL : NATURAL;
    ITEM_CHRS, SAVED_ITEM_CHRS : ITEM_CHRS_REC;
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
    ITEM_CHRS.VAL_LEN := TEXT_HANDLER.LENGTH(TXT);
    ITEM_CHRS.VAL_TEXT(1 .. ITEM_CHRS.VAL_LEN) := TEXT_HANDLER.VALUE(TXT);

    -- EOF
    if TEXT_HANDLER.EMPTY(TXT) then
      if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.PARSER) then
        TEXT_IO.PUT_LINE ("Parser: Eof >");
      end if;
      ITEM_CHRS.VAL_LEN := 3;
      ITEM_CHRS.VAL_TEXT(1 .. 3) := "EOF";
      INSTR_STACK.PUSH(ITEM_CHRS);
      return (KIND => OPER, VAL_OPER => RET);
    end if;

    C := TEXT_HANDLER.VALUE(TXT)(1);

    -- Parse [ or REGI
    if TEXT_HANDLER.LENGTH(TXT) = 1 then
      
      if C in 'a' .. 'z' or else C in 'A' .. 'Z' then
        -- A register
        INSTR_STACK.PUSH(ITEM_CHRS);
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
        if ITEM_CHRS.VAL_LEN + 4 <= INPUT_DISPATCHER.MAX_STRING_LG then
          SAVED_ITEM_CHRS.VAL_LEN := TEXT_HANDLER.LENGTH(TXTS) + 4;
          SAVED_ITEM_CHRS.VAL_TEXT(1 .. SAVED_ITEM_CHRS.VAL_LEN) := "[ " & TEXT_HANDLER.VALUE(TXTS) & " ]";
        else
          SAVED_ITEM_CHRS := ITEM_CHRS;
        end if;
        INSTR_STACK.PUSH(SAVED_ITEM_CHRS);
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
        if WORDS(O).WORD = W then
          INSTR_STACK.PUSH(ITEM_CHRS);
          return (KIND => MCD_MNG.OPER, VAL_OPER => O);
        end if;
      end loop;
    end if;
     
    -- Parse INTE REAL BOOL
    begin
      BOOL_IO.GET(TEXT_HANDLER.VALUE(TXT), B, L);
      if L = TEXT_HANDLER.LENGTH(TXT) then
        INSTR_STACK.PUSH(ITEM_CHRS);
        return (KIND => MCD_MNG.BOOL, VAL_BOOL => B);
      end if;
    exception
      when others => null;
    end;
    begin
      INTE_IO.GET(TEXT_HANDLER.VALUE(TXT), I, L);
      if L = TEXT_HANDLER.LENGTH(TXT) then
        INSTR_STACK.PUSH(ITEM_CHRS);
        return (KIND => MCD_MNG.INTE, VAL_INTE => I);
      end if;
    exception
      when others => null;
    end;
    begin
      REAL_IO.GET(TEXT_HANDLER.VALUE(TXT), R, L);
      if L = TEXT_HANDLER.LENGTH(TXT) then
        INSTR_STACK.PUSH(ITEM_CHRS);
        return (KIND => MCD_MNG.REAL, VAL_REAL => R);
      end if;
    exception
      when others => null;
    end;
    
    -- Cannot recognise anything
    INSTR_STACK.PUSH(ITEM_CHRS);
    raise PARSING_ERROR;  
    
  exception
    when INPUT_DISPATCHER.STRING_ERROR =>
      INPUT_ERROR := TRUE;
      raise PARSING_ERROR;
    when others =>
      raise PARSING_ERROR;  
  end NEXT_ITEM;


  procedure PRINT_HELP is
    use TEXT_IO;
  begin
    PUT_LINE ("Commands are read from standard input. No argument accepted.");
    PUT_LINE ("Item ::= <integer> <real> <boolean> <operator> <register> <string/subprogram>");
    PUT_LINE ("  <register>          ::= 'a' .. 'z'  | 'A' .. 'Z'");
    PUT_LINE ("  <string/subprogram> ::= '[' <text> ']'");
    PUT_LINE ("  <operators>         ::=");
    for O in MCD_MNG.OPERATOR_LIST loop
      PUT_LINE("       " & WORDS(O).WORD & "  " & WORDS(O).COMMENT & "  " & MCD_MNG.OPERATOR_LIST'IMAGE(O));
    end loop;
  end PRINT_HELP;
    
  procedure DUMP_STACK is
    ITEM_CHRS : ITEM_CHRS_REC;
  begin
    if not DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.HISTORY) then
      return;
    end if;
    SYS_CALLS.PUT_LINE_ERROR ("History:");
    loop
      begin
        INSTR_STACK.POP(ITEM_CHRS);
        SYS_CALLS.PUT_ERROR (ITEM_CHRS.VAL_TEXT(1 .. ITEM_CHRS.VAL_LEN) & " ");
      exception
        when INSTR_STACK.CIRC_EMPTY =>
         exit;
      end;
    end loop;
    if INPUT_ERROR then
      SYS_CALLS.PUT_LINE_ERROR (INPUT_DISPATCHER.ERROR_STRING);
    else
      SYS_CALLS.NEW_LINE_ERROR;
    end if;
  end DUMP_STACK;


end PARSER;
