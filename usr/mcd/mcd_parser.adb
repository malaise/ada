with TEXT_IO;
with TEXT_HANDLER, MY_MATH, QUEUES, SYS_CALLS, LOWER_STR;
with DEBUG, INPUT_DISPATCHER, BOOL_IO, INTE_IO, REAL_IO;
package body PARSER is
  use MCD_MNG;

  subtype ITEM_CHRS_REC is MCD_MNG.ITEM_REC(MCD_MNG.CHRS);

  -- Instruction stack for debug history
  package INSTR_STACK is new QUEUES.CIRC(7, ITEM_CHRS_REC);

  TXT, TXTS : TEXT_HANDLER.TEXT(INPUT_DISPATCHER.MAX_STRING_LG);

  INPUT_ERROR : BOOLEAN := FALSE;

  subtype ONE_WORD is STRING(1 .. 2);
  NOSY : constant ONE_WORD := (others => ' ');
  subtype ONE_COMMENT is STRING (1 .. 30);
  type ONE_REC is record
    WORD : ONE_WORD;
    COMMENT : ONE_COMMENT;
  end record;

  WORDS : constant array (MCD_MNG.OPERATOR_LIST) of ONE_REC :=
  (ADD     => ("+ ", "push B + A                    "),
   SUB     => ("- ", "push B - A                    "),
   MULT    => ("* ", "push B * A                    "),
   DIV     => ("/ ", "push B / A                    "),
   REMIND  => ("% ", "push B % A (rest of division) "),
   POW     => ("**", "push B ** A (pow)             "),
   MINUS   => ("+-", "push -A                       "),

   BITAND  => ("&&", "push B & A (bit and)          "),
   BITOR   => ("||", "push B | A (bit and)          "),
   BITXOR  => ("^^", "push B ^ A (bit and)          "),
   BITNEG  => ("~~", "push ~A    (bit neg)          "),
   SHL     => ("<<", "push B << A (bits shift left) "),
   SHR     => (">>", "push B >> A (bits shift right)"),

   EQUAL   => ("= ", "push B = A                    "),
   DIFF    => ("/=", "push B /= A                   "),
   GREATER => ("> ", "push B > A                    "),
   SMALLER => ("< ", "push B < A                    "),
   GREATEQ => (">=", "push B >= A                   "),
   SMALLEQ => ("<=", "push B <= A                   "),

   BOLAND  => ("& ", "push B and A                  "),
   BOLOR   => ("| ", "push B or A                   "),
   BOLXOR  => ("^ ", "push B xor A                  "),
   BOLNEG  => ("~ ", "push not A                    "),

   TOREAL  => (NOSY, "push REAL(A)                  "),
   TOINTE  => (NOSY, "push INTE(A)                  "),

   ISREAL  => (NOSY, "push IS_REAL(A)               "),
   ISINTE  => (NOSY, "push IS_INTE(A)               "),

   OBASE   => (NOSY, "set output base to A          "),
 
   SSIZE   => (NOSY, "push stack size               "),
   SWAP    => (NOSY, "push A, push B                "),
   DUP     => (NOSY, "push A, push A                "),
   POP     => (NOSY, "pop A                         "),
   POPN    => (NOSY, "pop B A times                 "),

   POPR    => (NOSY, "B -> regA                     "),
   COPYR   => (NOSY, "B -> regA, push B             "),
   PUSHR   => (NOSY, "push regA                     "),

   POPE    => (NOSY, "pop A push_extra A            "),
   COPYE   => (NOSY, "pop A push_extra A push A     "),
   PUSHLE  => (NOSY, "pop_extra last  X push X      "),
   PUSHFE  => (NOSY, "pop_extra first X push X      "),
   ESIZE   => (NOSY, "push extra_stack size         "),

   IFTHEN  => (NOSY, "if A then push B              "),
   IFTE    => (NOSY, "if C then push B else push A  "),
   ETFI    => (NOSY, "if A then push C else push B  "),

   CALL    => (NOSY, "call A                        "),
   IFCALL  => (NOSY, "if B then call A              "),
   RET     => (NOSY, "return                        "),
   RETN    => (NOSY, "return A levels (0=all)       "),
   IFRET   => (NOSY, "if B return                   "),
   IFRETN  => (NOSY, "if B return A levels (0=all)  "),
   RETACAL => (NOSY, "return and call A             "),

   FORMAT  => (NOSY, "xx or xx.yyy format           "),
   PUT     => (NOSY, "put A                         "),
   NEWL    => (NOSY, "new line                      "),
   PUTL    => (NOSY, "put_line A                    "),

   STRLEN  => (NOSY, "push length of A              "),
   STRCAT  => (NOSY, "push B & A                    "),
   STRSUB  => (NOSY, "push C(B..A)                  "),
   STRLOC  => (NOSY, "push C occurence of B in A    "),
   STRREP  => (NOSY, "push C replaced by B at pos A "),
   STRUPP  => (NOSY, "push A in uppercase           "),
   STRLOW  => (NOSY, "push A in lowercase           "),
   STRREAL => (NOSY, "push A converted to real      "),
   STRINTE => (NOSY, "push A converted to inte      "),
   STRBOOL => (NOSY, "push A converted to bool      "),
   STROF =>   (NOSY, "push formated string of A     "),

   HELP    => (NOSY, "put help                      ") );


  function NEXT_ITEM return MCD_MNG.ITEM_REC is
    LEVEL : NATURAL;
    ITEM_CHRS, SAVED_ITEM_CHRS : ITEM_CHRS_REC;
    FIRST_WORD : BOOLEAN;
    W : ONE_WORD;
    C : CHARACTER;
    B : BOOLEAN;
    I : MY_MATH.INTE;
    R : MY_MATH.REAL;
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
        TEXT_IO.PUT_LINE ("Parser: Eof");
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

    -- Parse OPER : string
    declare
      OP : MCD_MNG.OPERATOR_LIST;
    begin
      OP := MCD_MNG.OPERATOR_LIST'VALUE(TEXT_HANDLER.VALUE(TXT));
      -- Allow string only if no symbol defined
      if WORDS(OP).WORD = NOSY then
        INSTR_STACK.PUSH(ITEM_CHRS);
        return (KIND => MCD_MNG.OPER, VAL_OPER => OP);
      end if;
    exception
      -- Does not match
      when others => null;
    end;

    -- Parse OPER : synbol
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
    OPE_NAME : STRING (1 .. 7);
  begin
    PUT_LINE ("Commands are read from standard input. No argument accepted.");
    PUT_LINE ("Separators are space and horiz tab.");
    PUT_LINE ("Item ::= <integer> <real> <boolean> <operator> <register> <string/subprogram>");
    PUT_LINE ("  <integer>           ::= <number> | <base>#<number># ");
    PUT_LINE ("  <register>          ::= 'a' .. 'z'  | 'A' .. 'Z'");
    PUT_LINE ("  <string/subprogram> ::= '[' <text> ']'");
    PUT_LINE ("  <operator>          ::= <operator_name> | <operator_symbol>");
    PUT_LINE ("Operators are: Name      Action (A is top of stack, then B...)");
    for O in MCD_MNG.OPERATOR_LIST loop
      OPE_NAME:= (others => ' ');
      if WORDS(O).WORD /= NOSY then
        OPE_NAME(1 .. 2) := WORDS(O).WORD;
      else 
        OPE_NAME(1 .. MCD_MNG.OPERATOR_LIST'IMAGE(O)'LENGTH)
                := LOWER_STR(MCD_MNG.OPERATOR_LIST'IMAGE(O));
      end if;
      PUT_LINE("               " & OPE_NAME & "   " & WORDS(O).COMMENT);
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

