with TEXT_HANDLER;
with DEBUG, INPUT_DISPATCHER;
package body MCD_MNG is


  A, B, C : ITEM_REC;
  CALL_ENTRY : TEXT_HANDLER.TEXT (INPUT_DISPATCHER.MAX_STRING_LG);

  package STACK is 
    -- What can we store in stack
    subtype OPERAND_KIND_LIST is ITEM_KIND_LIST range INTE .. REGI;
    -- On push : INVALID_ITEM;

    procedure PUSH (ITEM : in ITEM_REC);

    procedure POP (ITEM : out ITEM_REC);

    function STACK_SIZE return NATURAL;
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

    -- INTE->REAL
    function TOREAL  (X : ITEM_REC) return ITEM_REC;

    -- REAL->INTE
    function TOINTE (X : ITEM_REC) return ITEM_REC;

    -- BOOL,BOOL->BOOL
    function BOLAND  (L, R : ITEM_REC) return ITEM_REC;
    function BOLOR   (L, R : ITEM_REC) return ITEM_REC;
    function BOLXOR  (L, R : ITEM_REC) return ITEM_REC;

    -- BOOL->BOOL
    function BOLNEG  (X : ITEM_REC) return ITEM_REC;

    -- BOOL,INTE,INTE->INT or BOOL,REAL,REAL->REAL
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
    subtype IO_KIND_LIST is ITEM_KIND_LIST range INTE .. BOOL;
    procedure PUT (ITEM : in ITEM_REC);
    procedure PUT_LINE (ITEM : in ITEM_REC);
    procedure NEW_LINE;

    -- INVALID_ARGUMENT : exception;
  end IOS;

  package CALL_STACK is 

    procedure PUSH (ITEM : in STRING);
    function  POP return STRING;

    function LEVEL return NATURAL;

  end CALL_STACK;

  package body STACK is separate;
  package body OPERATIONS is separate;
  package body REGISTERS is separate;
  package body IOS is separate;
  package body CALL_STACK is separate;


  procedure NEW_ITEM (ITEM : in ITEM_REC; THE_END : out BOOLEAN) is
    use STACK;

    procedure DO_CALL is
    begin
      POP(A);
      if A.KIND /= CHRS then
        raise INVALID_ARGUMENT;
      end if;
      if CALL_STACK.LEVEL /= 0 then
        -- Save contect;
        TEXT_HANDLER.SET(CALL_ENTRY, INPUT_DISPATCHER.GET_REMAINING);
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

    procedure DO_RET is
      L : INTEGER;
    begin
      POP(A);
      -- has to be INTE and val NATURAL
      begin
        L := NATURAL(A.VAL_INTE);
      exception
        when others => raise INVALID_ARGUMENT;
      end;
      if L = 0 then
        -- Exit
        THE_END := TRUE;
        return;
      end if;
      -- Can return by one more than level
      if L - 1 > CALL_STACK.LEVEL then
        raise INVALID_ARGUMENT;
      elsif L - 1 = CALL_STACK.LEVEL then
        THE_END := TRUE;
        return;
      end if;
      -- Return N times
      for I in reverse 1 .. A.VAL_INTE loop
        -- Restart form previous context
        TEXT_HANDLER.SET(CALL_ENTRY, CALL_STACK.POP);
      end loop;
      INPUT_DISPATCHER.SET_INPUT(TEXT_HANDLER.VALUE(CALL_ENTRY));
    end DO_RET;

  begin
    -- Default, except RET(0)
    THE_END := FALSE;
    -- Dispatch
    if ITEM.KIND /= OPER then
        -- Push operand
        STACK.PUSH(ITEM);
    else -- OPERATOR
      case ITEM.VAL_OPER is 
        -- These 3 I do it myself
        when SWAP =>
          POP(B); POP(A); PUSH(B); PUSH(A);
        when DUP =>
          POP(A); PUSH(A); PUSH(A);
        when POP =>
          POP(A);

        -- These are operations
        when ADD =>
          POP(B); POP(A); PUSH (OPERATIONS.ADD(A,B));
        when SUB =>
          POP(B); POP(A); PUSH (OPERATIONS.SUB(A,B));
        when MULT =>
          POP(B); POP(A); PUSH (OPERATIONS.MULT(A,B));
        when DIV =>
          POP(B); POP(A); PUSH (OPERATIONS.DIV(A,B));
        when REMIND =>
          POP(B); POP(A); PUSH (OPERATIONS.REMIND(A,B));
        when POW =>
          POP(B); POP(A); PUSH (OPERATIONS.POW(A,B));
        when BITAND =>
          POP(B); POP(A); PUSH (OPERATIONS.BITAND(A,B));
        when BITOR =>
          POP(B); POP(A); PUSH (OPERATIONS.BITOR(A,B));
        when BITXOR =>
          POP(B); POP(A); PUSH (OPERATIONS.BITXOR(A,B));
        when SHL =>
          POP(B); POP(A); PUSH (OPERATIONS.SUB(A,B));
        when SHR =>
          POP(B); POP(A); PUSH (OPERATIONS.SHR(A,B));
        when MINUS =>
          POP(A); PUSH (OPERATIONS.MINUS(A));
        when BITNEG =>
          POP(A); PUSH (OPERATIONS.BITNEG(A));
        when EQUAL =>
          POP(B); POP(A); PUSH (OPERATIONS.EQUAL(A,B));
        when DIFF =>
          POP(B); POP(A); PUSH (OPERATIONS.DIFF(A,B));
        when GREATER =>
          POP(B); POP(A); PUSH (OPERATIONS.GREATER(A,B));
        when SMALLER =>
          POP(B); POP(A); PUSH (OPERATIONS.SMALLER(A,B));
        when GREATEQ =>
          POP(B); POP(A); PUSH (OPERATIONS.GREATEQ(A,B));
        when SMALLEQ =>
          POP(B); POP(A); PUSH (OPERATIONS.SMALLEQ(A,B));
        when TOREAL =>
          POP(A); PUSH (OPERATIONS.TOREAL(A));
        when TOINTE =>
          POP(A); PUSH (OPERATIONS.TOINTE(A));
        when BOLAND =>
          POP(B); POP(A); PUSH (OPERATIONS.BOLAND(A,B));
        when BOLOR =>
          POP(B); POP(A); PUSH (OPERATIONS.BOLOR(A,B));
        when BOLXOR =>
          POP(B); POP(A); PUSH (OPERATIONS.BOLXOR(A,B));
        when BOLNEG =>
          POP(A); PUSH (OPERATIONS.BOLNEG(A));
        when IFTE =>
          POP(C); POP(B); POP(A); PUSH (OPERATIONS.IFTE(A,B,C));
 
        -- These are about registers
        when POPR =>
          -- A B -> store A in reg B
          POP(B); POP(A); REGISTERS.STORE(A, B);
        when PUSHR =>
          -- A -> push content of reg B
          POP(B); PUSH(REGISTERS.RETRIEVE(B));

        -- Stack size
        when SSIZE =>
          PUSH( (KIND => INTE, VAL_INTE => MATH.INTE(STACK.STACK_SIZE)));

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

        -- PUTs
        when PUT =>
          POP(A); IOS.PUT(A);
        when PUTL =>
          POP(A); IOS.PUT_LINE(A);
        when NEWL =>
          IOS.NEW_LINE;
      end case;
    end if;
  end NEW_ITEM;

  function CHECK_EMPTY_STACK return BOOLEAN is
  begin
    return STACK.STACK_SIZE = 0;
  end;

end MCD_MNG;

