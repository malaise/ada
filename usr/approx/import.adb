with TEXT_IO;
with ARGUMENT, TEXT_HANDLER, GET_LINE;
with POINTS, POINT_STR, FILE;

procedure IMPORT is

  POINT : POINTS.P_T_ONE_POINT;

  package MY_GET_LINE is new GET_LINE (
     MAX_WORD_LEN => 40,
     MAX_WORD_NB  => 3,
     MAX_LINE_LEN => 132,
     COMMENT      => '#');

  LINE  : MY_GET_LINE.LINE_ARRAY;

  procedure ERROR (MSG : in STRING)  is
  begin
    TEXT_IO.PUT_LINE ("ERROR: " & MSG);
    TEXT_IO.PUT_LINE ("Usage: " & ARGUMENT.GET_PROGRAM_NAME & " <src_ascii_file> <dst_approx_file>");
  end ERROR;

begin

  if ARGUMENT.GET_NBRE_ARG /= 2 then
    ERROR ("Invalid arguments");
    return;
  end if;
  if FILE.F_EXISTS(ARGUMENT.GET_PARAMETER(OCCURENCE => 2)) then
    ERROR ("File " & ARGUMENT.GET_PARAMETER(OCCURENCE => 2) & " exists");
    return;
  end if;

  begin
    MY_GET_LINE.OPEN(ARGUMENT.GET_PARAMETER(OCCURENCE => 1));
  exception
    when TEXT_IO.NAME_ERROR =>
      ERROR ("File " & ARGUMENT.GET_PARAMETER(OCCURENCE => 1) & " not found");
      return;
    when others =>
      ERROR ("Opening file " & ARGUMENT.GET_PARAMETER(OCCURENCE => 1));
      raise;
  end;
      
  POINTS.P_CLEAR;


  loop
    MY_GET_LINE.GET_WORDS (LINE);
    if MY_GET_LINE.GET_WORD_NUMBER /= 2 then
      ERROR ("At line " & TEXT_IO.POSITIVE_COUNT'IMAGE(MY_GET_LINE.GET_LINE_NO)
                        & " two reals expected");
      MY_GET_LINE.CLOSE;
      return;
    end if;

    begin
      POINT.X := POINT_STR.COORDINATE_VALUE(TEXT_HANDLER.VALUE(LINE(1)));
      POINT.Y := POINT_STR.COORDINATE_VALUE(TEXT_HANDLER.VALUE(LINE(2)));
    exception
      when others =>
        ERROR ("At line " & TEXT_IO.POSITIVE_COUNT'IMAGE(MY_GET_LINE.GET_LINE_NO)
                          & " two reals expected");
        MY_GET_LINE.CLOSE;
        return;
    end;

    POINTS.P_UPD_POINT (POINTS.ADD, 1, POINT);

    MY_GET_LINE.READ_NEXT_LINE;
  end loop;

exception
  when MY_GET_LINE.NO_MORE_LINE =>
    MY_GET_LINE.CLOSE;
    FILE.F_WRITE(ARGUMENT.GET_PARAMETER(OCCURENCE => 2),
                 POINTS.P_THE_POINTS);
    TEXT_IO.PUT_LINE ("Done.");
end IMPORT;
