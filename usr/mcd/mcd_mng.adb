with TEXT_IO;
with TEXT_HANDLER;
with DEBUG, INPUT_DISPATCHER, PARSER;
package body MCD_MNG is


  A, B, C : ITEM_REC;
  CALL_ENTRY : TEXT_HANDLER.TEXT (INPUT_DISPATCHER.MAX_STRING_LG);

  package STACK is 
    -- What can we store in stack
    subtype OPERAND_KIND_LIST is ITEM_KIND_LIST range INTE .. REGI;
    -- On push : INVALID_ITEM;

    procedure PUSH (ITEM : in ITEM_REC; DEFAULT_STACK : in BOOLEAN := TRUE);

    procedure POP (ITEM : out ITEM_REC; DEFAULT_STACK : in BOOLEAN := TRUE);
    procedure READ (ITEM : out ITEM_REC; DEFAULT_STACK : in BOOLEAN := TRUE);

    function STACK_SIZE (DEFAULT_STACK : BOOLEAN := TRUE;
                         NO_DEBUG : BOOLEAN := FALSE) return NATURAL;
  end STACK;

  package OPERATIONS is

    function IS_TRUE (X : ITEM_REC) return BOOLEAN;

    -- INTE,INTE->INTE or REAL,REAL->REAL
    function ADD     (L, R : ITEM_REC) return ITEM_REC;
    function SUB     (L, R : ITEM_REC) return ITEM_REC;
    function MULT    (L, R : ITEM_REC) return ITEM_REC;
    function DIV     (L, R : ITEM_REC) return ITEM_REC;
    function POW     (L, R : ITEM_REC) return ITEM_REC;

    -- INTE,INTE->INTE
    function REMIND  (L, R : ITEM_REC) return ITEM_REC;

    -- INTE,INTE->INTE
    function BITAND  (L, R : ITEM_REC) return ITEM_REC;
    function BITOR   (L, R : ITEM_REC) return ITEM_REC;
    function BITXOR  (L, R : ITEM_REC) return ITEM_REC;
    function SHL     (L, R : ITEM_REC) return ITEM_REC;
    function SHR     (L, R : ITEM_REC) return ITEM_REC;

    -- INTE->INTE or REAL->REAL
    function MINUS   (X : ITEM_REC) return ITEM_REC;

    -- INTE->INTE
    function BITNEG  (X : ITEM_REC) return ITEM_REC;

    -- INTE,INTE->BOOL or REAL,REAL->BOOL or BOOL,BOOL->BOOL 
    function EQUAL   (L, R : ITEM_REC) return ITEM_REC;
    function DIFF    (L, R : ITEM_REC) return ITEM_REC;
    function GREATER (L, R : ITEM_REC) return ITEM_REC;
    function SMALLER (L, R : ITEM_REC) return ITEM_REC;
    function GREATEQ (L, R : ITEM_REC) return ITEM_REC;
    function SMALLEQ (L, R : ITEM_REC) return ITEM_REC;

    -- INTE<->REAL
    function TOREAL  (X : ITEM_REC) return ITEM_REC;
    function TOINTE  (X : ITEM_REC) return ITEM_REC;

    -- INTE->BOOL REAL->BOOL
    function ISREAL  (X : ITEM_REC) return ITEM_REC;
    function ISINTE  (X : ITEM_REC) return ITEM_REC;

    -- BOOL,BOOL->BOOL
    function BOLAND  (L, R : ITEM_REC) return ITEM_REC;
    function BOLOR   (L, R : ITEM_REC) return ITEM_REC;
    function BOLXOR  (L, R : ITEM_REC) return ITEM_REC;

    -- BOOL->BOOL
    function BOLNEG  (X : ITEM_REC) return ITEM_REC;

    -- BOOL,*,*->*
    function IFTE    (X, A, B : ITEM_REC) return ITEM_REC;

    -- Argument does not mach operator
    -- INVALID_ARGUMENT : exception;
    -- Arguments are not compatible to each other
    -- ARGUMENT_MISMATCH : exception;
  end OPERATIONS;

  package REGISTERS is
    subtype REGISTER_CONTENT_LIST is ITEM_KIND_LIST range INTE .. REGI;

    procedure STORE (VAL : in ITEM_REC; TO_REG : in ITEM_REC); 
    function  RETRIEVE (FROM_REG : in ITEM_REC) return ITEM_REC;

    -- Valid registers  are 'a' .. 'z' and 'A' .. 'Z'
    -- INVALID_REGISTER : exception;

    -- Valid contents are INTE REAL BOOL SUBP CHRS
    -- INVALID_ARGUMENT : exception;

    -- Nothing to retrieve
    -- EMTPY_REGISTER : exception;
  end REGISTERS;

  package IOS is

    procedure SET_OBASE (BASE : in ITEM_REC);

    subtype IO_KIND_LIST is ITEM_KIND_LIST range INTE .. BOOL;
    procedure FORMAT (ITEM : in ITEM_REC);
    procedure PUT (ITEM : in ITEM_REC);
    procedure PUT_LINE (ITEM : in ITEM_REC);
    procedure NEW_LINE;

    function STRINTE (S : ITEM_REC) return ITEM_REC;
    function STRREAL (S : ITEM_REC) return ITEM_REC;
    function STRBOOL (S : ITEM_REC) return ITEM_REC;
    function STROF (ITEM : ITEM_REC) return ITEM_REC;
    -- INVALID_ARGUMENT : exception;
  end IOS;

  package CALL_STACK is 

    procedure PUSH (ITEM : in STRING);
    function  POP return STRING;

    function LEVEL return NATURAL;

  end CALL_STACK;

  package STRINGS is
    function STRLEN (S : ITEM_REC) return ITEM_REC;
    function STRCAT (S1, S2 : ITEM_REC) return ITEM_REC;
    function STRSUB (S, I1, I2 : ITEM_REC) return ITEM_REC;
    function STRLOC (OCC, PAT, S : ITEM_REC) return ITEM_REC;
    function STRREP (I, PAT, S : ITEM_REC) return ITEM_REC;
    function STRUPP (S : ITEM_REC) return ITEM_REC;
    function STRLOW (S : ITEM_REC) return ITEM_REC;

  end STRINGS;

  package body STACK is separate;
  package body OPERATIONS is separate;
  package body REGISTERS is separate;
  package body IOS is separate;
  package body CALL_STACK is separate;
  package body STRINGS is separate;


  procedure NEW_ITEM (ITEM : in ITEM_REC; THE_END : out BOOLEAN) is
    use STACK;

    procedure DO_CALL is
    begin
      if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.OPER) then
        TEXT_IO.PUT_LINE("Mng: Do_call");
      end if;
      POP(A);
      if A.KIND /= CHRS then
        raise INVALID_ARGUMENT;
      end if;
      if CALL_STACK.LEVEL /= 0 then
        -- Save contect;
        TEXT_HANDLER.SET(CALL_ENTRY, INPUT_DISPATCHER.GET_REMAINING);
        -- Even if end of subprog, this is not stdin
        if TEXT_HANDLER.EMPTY(CALL_ENTRY) then
          TEXT_HANDLER.SET(CALL_ENTRY, " ");
        end if;
        CALL_STACK.PUSH (TEXT_HANDLER.VALUE(CALL_ENTRY));
      else
        -- Dummy context
        CALL_STACK.PUSH ("");
      end if;
      -- Call
      if A.VAL_LEN = 0 then
        -- Empty subprogram : not stdin
        INPUT_DISPATCHER.SET_INPUT(" ");
      else
        INPUT_DISPATCHER.SET_INPUT(A.VAL_TEXT(1 .. A.VAL_LEN));
      end if;
    end DO_CALL;

    procedure DO_RET (ALLOW_LEVEL_0 : in BOOLEAN := TRUE) is
      L : INTEGER;
      CALL_STACK_LEVEL : NATURAL;
    begin
      if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.OPER) then
        TEXT_IO.PUT_LINE("Mng: Do_ret");
      end if;
      POP(A);
      CALL_STACK_LEVEL := CALL_STACK.LEVEL;
      -- has to be INTE and val NATURAL
      begin
        L := NATURAL(A.VAL_INTE);
      exception
        when others => raise INVALID_ARGUMENT;
      end;
      if L = 0 then
        -- Return all
        L := CALL_STACK_LEVEL + 1;
      end if;
      -- Can return by one more than level
      if L - 1 > CALL_STACK_LEVEL then
        raise INVALID_ARGUMENT;
      elsif L - 1 = CALL_STACK_LEVEL then
        if ALLOW_LEVEL_0 then
          THE_END := TRUE;
          return;
        else
          -- RETACAL from level 0
          raise INVALID_ARGUMENT;
        end if;
      end if;
      -- Return N times
      for I in reverse 1 .. A.VAL_INTE loop
        -- Restart form previous context
        TEXT_HANDLER.SET(CALL_ENTRY, CALL_STACK.POP);
      end loop;
      INPUT_DISPATCHER.SET_INPUT(TEXT_HANDLER.VALUE(CALL_ENTRY));
    end DO_RET;

  begin
    -- Default, except RET
    THE_END := FALSE;
    -- Dispatch
    if ITEM.KIND /= OPER then
        -- Push operand
        STACK.PUSH(ITEM);
    else -- OPERATOR
      if DEBUG.DEBUG_LEVEL_ARRAY(DEBUG.OPER) then
        TEXT_IO.PUT("Mng: ");
        DEBUG.PUT(ITEM);
        TEXT_IO.NEW_LINE;
      end if;
      case ITEM.VAL_OPER is 
        -- These 3 I do it myself
        when SWAP =>
          POP(A); POP(B); PUSH(A); PUSH(B);
        when DUP =>
          READ(A); PUSH(A);
        when POP =>
          POP(A);

        -- These are operations
        when ADD =>
          POP(A); POP(B); PUSH (OPERATIONS.ADD(B,A));
        when SUB =>
          POP(A); POP(B); PUSH (OPERATIONS.SUB(B,A));
        when MULT =>
          POP(A); POP(B); PUSH (OPERATIONS.MULT(B,A));
        when DIV =>
          POP(A); POP(B); PUSH (OPERATIONS.DIV(B,A));
        when REMIND =>
          POP(A); POP(B); PUSH (OPERATIONS.REMIND(B,A));
        when POW =>
          POP(A); POP(B); PUSH (OPERATIONS.POW(B,A));
        when BITAND =>
          POP(A); POP(B); PUSH (OPERATIONS.BITAND(B,A));
        when BITOR =>
          POP(A); POP(B); PUSH (OPERATIONS.BITOR(B,A));
        when BITXOR =>
          POP(A); POP(B); PUSH (OPERATIONS.BITXOR(B,A));
        when SHL =>
          POP(A); POP(B); PUSH (OPERATIONS.SHL(B,A));
        when SHR =>
          POP(A); POP(B); PUSH (OPERATIONS.SHR(B,A));
        when MINUS =>
          POP(A); PUSH (OPERATIONS.MINUS(A));
        when BITNEG =>
          POP(A); PUSH (OPERATIONS.BITNEG(A));
        when EQUAL =>
          POP(A); POP(B); PUSH (OPERATIONS.EQUAL(B,A));
        when DIFF =>
          POP(A); POP(B); PUSH (OPERATIONS.DIFF(B,A));
        when GREATER =>
          POP(A); POP(B); PUSH (OPERATIONS.GREATER(B,A));
        when SMALLER =>
          POP(A); POP(B); PUSH (OPERATIONS.SMALLER(B,A));
        when GREATEQ =>
          POP(A); POP(B); PUSH (OPERATIONS.GREATEQ(B,A));
        when SMALLEQ =>
          POP(A); POP(B); PUSH (OPERATIONS.SMALLEQ(B,A));
        when TOREAL =>
          POP(A); PUSH (OPERATIONS.TOREAL(A));
        when TOINTE =>
          POP(A); PUSH (OPERATIONS.TOINTE(A));
        when ISREAL =>
          POP(A); PUSH (OPERATIONS.ISREAL(A));
        when ISINTE =>
          POP(A); PUSH (OPERATIONS.ISINTE(A));
        when BOLAND =>
          POP(A); POP(B); PUSH (OPERATIONS.BOLAND(B,A));
        when BOLOR =>
          POP(A); POP(B); PUSH (OPERATIONS.BOLOR(B,A));
        when BOLXOR =>
          POP(A); POP(B); PUSH (OPERATIONS.BOLXOR(B,A));
        when BOLNEG =>
          POP(A); PUSH (OPERATIONS.BOLNEG(A));

        -- Conditions
        when IFTHEN =>
          POP(A); POP(B);
          if OPERATIONS.IS_TRUE(A) then
            PUSH(B);
          end if;
        when IFTE =>
          POP(A); POP(B); POP(C); PUSH (OPERATIONS.IFTE(C,B,A));
        when ETFI =>
          POP(A); POP(B); POP(C); PUSH (OPERATIONS.IFTE(A,C,B));
 
        when OBASE =>
          POP(A); IOS.SET_OBASE(A);

        -- These are about registers
        when POPR =>
          -- store B in reg A
          POP(A); POP(B); REGISTERS.STORE(B, A);
        when COPYR =>
          -- store B in reg A and push B
          POP(A); READ(B); REGISTERS.STORE(B, A);
        when PUSHR =>
          -- A -> push content of reg A
          POP(A); PUSH(REGISTERS.RETRIEVE(A));

        -- Stack size
        when SSIZE =>
          PUSH( (KIND => INTE, VAL_INTE => MY_MATH.INTE(STACK.STACK_SIZE)));

        -- Extra stack
        when POPE =>
          -- pushe A
          POP(A); PUSH (A, DEFAULT_STACK => FALSE);
        when COPYE =>
          -- pushe A push A
          READ(A); PUSH (A, DEFAULT_STACK => FALSE); 
        when PUSHE =>
          -- pushe A
          POP(A, DEFAULT_STACK => FALSE); PUSH (A);
        when ESIZE =>
           PUSH( (KIND => INTE,
                  VAL_INTE => MY_MATH.INTE(STACK.STACK_SIZE(
                                DEFAULT_STACK => FALSE))));


        -- These ones are subprogram
        when CALL =>
          DO_CALL;
        when IFCALL =>
          POP(A);
          POP(B);
          if OPERATIONS.IS_TRUE(B) then
            PUSH(A);
            DO_CALL;
          end if;

        when RET =>
          PUSH( (KIND => INTE, VAL_INTE => 1) );
          DO_RET;
        when RETN =>
          DO_RET;
        when IFRETN =>
          POP(A);
          POP(B);
          if OPERATIONS.IS_TRUE(B) then
            PUSH(A);
            DO_RET;
          end if;

        when RETACAL =>
          PUSH( (KIND => INTE, VAL_INTE => 1) );
          DO_RET;
          DO_CALL;

        -- PUTs
        when FORMAT =>
          POP(A); IOS.FORMAT(A);
        when PUT =>
          POP(A); IOS.PUT(A);
        when PUTL =>
          POP(A); IOS.PUT_LINE(A);
        when NEWL =>
          IOS.NEW_LINE;


        -- Strings
        when STRLEN =>
          POP(A); PUSH (STRINGS.STRLEN(A));
        when STRCAT =>
          POP(A); POP(B); PUSH (STRINGS.STRCAT(B, A));
        when STRSUB =>
          POP(A); POP(B); POP(C); PUSH (STRINGS.STRSUB(C, B, A));
        when STRLOC =>
          POP(A); POP(B); POP(C); PUSH (STRINGS.STRLOC(C, B, A));
        when STRREP =>
          POP(A); POP(B); POP(C); PUSH (STRINGS.STRREP(C, B, A));
        when STRUPP =>
          POP(A); PUSH (STRINGS.STRUPP(A));
        when STRLOW =>
          POP(A); PUSH (STRINGS.STRLOW(A));
        when STRREAL =>
          POP(A); PUSH (IOS.STRREAL(A));
        when STRINTE =>
          POP(A); PUSH (IOS.STRINTE(A));
        when STRBOOL =>
          POP(A); PUSH (IOS.STRBOOL(A));
        when STROF =>
          POP(A); PUSH (IOS.STROF(A));

        when HELP =>
          PARSER.PRINT_HELP;
      end case;
    end if;
  end NEW_ITEM;

  function CHECK_EMPTY_STACK return BOOLEAN is
  begin
    return STACK.STACK_SIZE(NO_DEBUG => TRUE) = 0;
  end;

end MCD_MNG;

