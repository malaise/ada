with MY_IO;
with RND;
with ARGUMENT;

procedure ONE_OF_ARG is
begin
  if ARGUMENT.GET_NBRE_ARG = 0 then
    return;
  end if;
  RND.RANDOMIZE;

  MY_IO.PUT_LINE (STRING'(ARGUMENT.GET_PARAMETER(
         RND.INT_RANDOM(1, ARGUMENT.GET_NBRE_ARG))));
end ONE_OF_ARG;
  
