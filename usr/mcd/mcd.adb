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
    ITEM := PARSER.NEXT_ITEM;
    MCD_MNG.NEW_ITEM(ITEM, THE_END);
    exit when THE_END;
   end loop;
   
   if not MCD_MNG.CHECK_EMPTY_STACK then
     SYS_CALLS.PUT_LINE_ERROR ("Warning, the stack was not empty.");
   end if;

exception
  -- Clean mapping of exceptions
  when MCD_MNG.INVALID_ARGUMENT =>
    raise INVALID_ARGUMENT;
  when MCD_MNG.ARGUMENT_MISMATCH =>
    raise ARGUMENT_MISMATCH;
  when MCD_MNG.INVALID_REGISTER =>
    raise INVALID_REGISTER;
  when MCD_MNG.EMTPY_REGISTER =>
    raise EMTPY_REGISTER;
  when MCD_MNG.EMPTY_STACK =>
    raise EMPTY_STACK;
  when PARSER.PARSING_ERROR =>
    raise PARSING_ERROR;
end MCD;

