with MATH;
with INPUT_DISPATCHER;
package MCD_MNG is

  type ITEM_KIND_LIST is (INTE, REAL, BOOL, CHRS, REGI, OPER);

  type OPERATOR_LIST is (
   -- Number, number -> number
   ADD,      -- "+"
   SUB,      -- "-"
   MULT,     -- "*"
   DIV,      -- "/"
   REMIND,   -- "%"
   POW,      -- "**"
   SWAP,     -- "<>"

   BITAND,   -- "&&"
   BITOR,    -- "||"
   BITXOR,   -- "^^"

   SHL,      -- "<<"
   SHR,      -- ">>"
   
   -- Number -> number
   MINUS,    -- "+-"
   BITNEG,   -- "~~"

   -- Number, number -> boolean
   EQUAL,    -- "="
   DIFF,     -- "/="
   GREATER,  -- ">"
   SMALLER,  -- "<"
   GREATEQ,  -- ">="
   SMALLEQ,  -- "<="

   -- INTE <-> REAL
   TOREAL,   -- "$"
   TOINTE,   -- "!"

    -- Boolean, boolean -> boolean
   BOLAND,   -- "&"
   BOLOR,    -- "|"
   BOLXOR,   -- "^"

   -- Boolean -> boolean
   BOLNEG,   -- "~"

   -- Operand -> operand, operand
   DUP,      -- "><"

   -- Nothing
   POP,      -- "--"

   -- Boolean, operand, operand -> operand
   IFTE,     -- "?"
  
   -- Pop in / Push from register
   POPR,     -- "->"
   PUSHR,    -- "<-"

   -- -> Number
   SSIZE,    -- "."

   -- Exec Subprog, return N times
   CALL,     -- "@"
   RET,      -- "_"

   -- Print (with new_line)
   PUT,      -- ","
   NEWL,     -- ":"
   PUTL      -- ";"
   );


  subtype CHARS_TEXT is STRING (1 .. INPUT_DISPATCHER.MAX_STRING_LG);
  subtype REGISTER_LIST is CHARACTER;

  type ITEM_REC (KIND : ITEM_KIND_LIST := ITEM_KIND_LIST'FIRST) is record
    case KIND is
      when INTE =>
        VAL_INTE : MATH.INTE;
      when REAL =>
        VAL_REAL : MATH.REAL;
      when BOOL =>
        VAL_BOOL : BOOLEAN;
      when CHRS =>
        VAL_LEN  : NATURAL;
        VAL_TEXT : CHARS_TEXT;
      when REGI => 
        VAL_REGI : REGISTER_LIST;
      when OPER =>
        VAL_OPER : OPERATOR_LIST;
    end case;
  end record;


  -- Treat a new item
  procedure NEW_ITEM (ITEM : in ITEM_REC; THE_END : out BOOLEAN);

  function CHECK_EMPTY_STACK return BOOLEAN;

  INVALID_ARGUMENT, ARGUMENT_MISMATCH, INVALID_REGISTER, EMTPY_REGISTER, EMPTY_STACK : exception;

end MCD_MNG;

