with TEXT_IO, INT_IO, FLO_IO, LONG_IO, BOOL_IO;
package MY_IO is 

  DEFAULT_EXP : TEXT_IO.FIELD := 4; 

  procedure PUT(ITEM  : in BOOLEAN; 
                WIDTH : in TEXT_IO.FIELD := BOOL_IO.DEFAULT_WIDTH; 
                SET   : in TEXT_IO.TYPE_SET := BOOL_IO.DEFAULT_SETTING)
    renames BOOL_IO.PUT;

  procedure PUT(ITEM : in CHARACTER) renames TEXT_IO.PUT;

  procedure PUT(ITEM  : in INTEGER;
                WIDTH : in TEXT_IO.FIELD := INT_IO.DEFAULT_WIDTH;
                BASE  : in TEXT_IO.NUMBER_BASE := INT_IO.DEFAULT_BASE)
    renames INT_IO.PUT;

  procedure PUT(ITEM  : in LONG_LONG_INTEGER;
                WIDTH : in TEXT_IO.FIELD := LONG_IO.DEFAULT_WIDTH;
                BASE  : in TEXT_IO.NUMBER_BASE := LONG_IO.DEFAULT_BASE)
    renames LONG_IO.PUT;

  procedure PUT(ITEM : in FLOAT;
                FORE : in TEXT_IO.FIELD := FLO_IO.DEFAULT_FORE;
                AFT  : in TEXT_IO.FIELD := FLO_IO.DEFAULT_AFT;
                EXP  : in TEXT_IO.FIELD := DEFAULT_EXP) renames FLO_IO.PUT;

  procedure PUT(ITEM : in STRING) renames TEXT_IO.PUT;

  procedure PUT_LINE(ITEM  : in BOOLEAN;
                     WIDTH : in TEXT_IO.FIELD := BOOL_IO.DEFAULT_WIDTH;
                     SET   : in TEXT_IO.TYPE_SET := BOOL_IO.DEFAULT_SETTING);

  procedure PUT_LINE(ITEM : in CHARACTER);

  procedure PUT_LINE(ITEM  : in INTEGER;
                     WIDTH : in TEXT_IO.FIELD := INT_IO.DEFAULT_WIDTH;
                     BASE  : in TEXT_IO.NUMBER_BASE := INT_IO.DEFAULT_BASE);

  procedure PUT_LINE(ITEM  : in LONG_LONG_INTEGER;
                     WIDTH : in TEXT_IO.FIELD := LONG_IO.DEFAULT_WIDTH;
                     BASE  : in TEXT_IO.NUMBER_BASE := LONG_IO.DEFAULT_BASE);

  procedure PUT_LINE(ITEM : in FLOAT;
                     FORE : in TEXT_IO.FIELD := FLO_IO.DEFAULT_FORE;
                     AFT  : in TEXT_IO.FIELD := FLO_IO.DEFAULT_AFT;
                     EXP  : in TEXT_IO.FIELD := DEFAULT_EXP);

  procedure PUT_LINE(ITEM : in STRING) renames TEXT_IO.PUT_LINE;

  procedure NEW_LINE(SPACING : in TEXT_IO.POSITIVE_COUNT := 1)
    renames TEXT_IO.NEW_LINE;

  procedure GET(ITEM : out BOOLEAN) renames BOOL_IO.GET;

  procedure GET(ITEM : out CHARACTER) renames TEXT_IO.GET;

  procedure GET(ITEM  : out INTEGER;
                WIDTH : in TEXT_IO.FIELD := 0) renames INT_IO.GET;

  procedure GET(ITEM  : out LONG_LONG_INTEGER;
                WIDTH : in TEXT_IO.FIELD := 0) renames LONG_IO.GET;

  procedure GET(ITEM  : out FLOAT;
                WIDTH : in TEXT_IO.FIELD := 0) renames FLO_IO.GET;

  procedure GET(ITEM : out STRING) renames TEXT_IO.GET;

  procedure GET_LINE(ITEM : out STRING;
                     LAST : out NATURAL) renames TEXT_IO.GET_LINE;

  -- Display prompt, then get item until no error
  procedure SAFE_GET(PROMPT : in STRING;
                     ITEM   : out INTEGER);

  procedure SAFE_GET(PROMPT : in STRING;
                     ITEM   : out LONG_LONG_INTEGER);

  procedure SAFE_GET(PROMPT : in STRING;
                     ITEM   : out FLOAT);

  procedure SKIP_LINE(SPACING : in TEXT_IO.POSITIVE_COUNT := 1)
    renames TEXT_IO.SKIP_LINE;

  function END_OF_LINE return BOOLEAN renames TEXT_IO.END_OF_LINE;

  procedure FLUSH renames TEXT_IO.FLUSH;

end MY_IO;
