with TEXT_IO;
with ARGUMENT, SYS_CALLS;
with DEBUG, PARSER, MCD_MNG;

procedure MCD is
  ITEM : MCD_MNG.ITEM_REC;
  THE_END : BOOLEAN;
  INVALID_ARGUMENT, ARGUMENT_MISMATCH, INVALID_REGISTER, EMTPY_REGISTER,
                    EMPTY_STACK : exception;
  PARSING_ERROR : exception;

begin

  DEBUG.INIT;

  if ARGUMENT.GET_NBRE_ARG /= 0 then
    PARSER.PRINT_HELP;
    return;
  end if;

  loop 
    begin
      ITEM := PARSER.NEXT_ITEM;
      MCD_MNG.NEW_ITEM(ITEM, THE_END);
      exit when THE_END;
    exception
      when others =>
        PARSER.DUMP_STACK;
        raise;
    end;
   end loop;
   
   if not MCD_MNG.CHECK_EMPTY_STACK then
     SYS_CALLS.PUT_LINE_ERROR ("Warning: The stack was not empty.");
   end if;

exception
  -- Clean mapping of exceptions
  when MCD_MNG.INVALID_ARGUMENT =>
    SYS_CALLS.PUT_LINE_ERROR ("Error: Invalid argument");
    SYS_CALLS.SET_ERROR_EXIT_CODE;
  when MCD_MNG.ARGUMENT_MISMATCH =>
    SYS_CALLS.PUT_LINE_ERROR ("Error: Argument mismatch");
    SYS_CALLS.SET_ERROR_EXIT_CODE;
  when MCD_MNG.INVALID_REGISTER =>
    SYS_CALLS.PUT_LINE_ERROR ("Error: Invalid register");
    SYS_CALLS.SET_ERROR_EXIT_CODE;
  when MCD_MNG.EMTPY_REGISTER =>
    SYS_CALLS.PUT_LINE_ERROR ("Error: Empty register");
    SYS_CALLS.SET_ERROR_EXIT_CODE;
  when MCD_MNG.EMPTY_STACK =>
    SYS_CALLS.PUT_LINE_ERROR ("Error: Empty stack");
    SYS_CALLS.SET_ERROR_EXIT_CODE;
  when PARSER.PARSING_ERROR =>
    SYS_CALLS.PUT_LINE_ERROR ("Error: Parsing error");
    SYS_CALLS.SET_ERROR_EXIT_CODE;
end MCD;

