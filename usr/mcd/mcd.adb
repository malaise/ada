with TEXT_IO;
with ARGUMENT;
with DEBUG, PARSER, MCD_MNG;

procedure MCD is
  ITEM : MCD_MNG.ITEM_REC;
  THE_END : BOOLEAN;
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
     TEXT_IO.PUT_LINE ("Warning, the stack was not empty.");
   end if;

end MCD;

