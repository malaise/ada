-- One argument : file name
-- Reads this file describing a linear system (get_line & get_float)
-- Solve linear system and put solution

with TEXT_IO;
with TEXT_HANDLER, ARGUMENT, NORMAL, SYSLIN, FLO_IO, GET_LINE, GET_FLOAT;

procedure T_SYSLIN is

  package MY_SYSLIN is new SYSLIN(FLOAT);

  MAX_LINE_LEN : constant := 1024;
  MAX_WORD_NB  : constant := 500;
  MAX_WORD_LEN : constant := 15;

  -- Matrix dimension
  DIM : POSITIVE := 1;
  LINE_NO : NATURAL := 0;

  FILE_ERROR : exception;


begin
  -- Check syntax
  if ARGUMENT.GET_NBRE_ARG /= 1 then
    TEXT_IO.PUT_LINE ("ERROR. Syntax : t_syslin <file_name>");
    return;
  end if;

  declare
    package MY_GET_LINE is new GET_LINE (
      MAX_WORD_LEN => MAX_WORD_LEN,
      MAX_WORD_NB  => MAX_WORD_NB,
      MAX_LINE_LEN => MAX_LINE_LEN);

    LINE  : MY_GET_LINE.LINE_ARRAY;

    function LINE_IS_SIGNIFICANT return BOOLEAN is
    begin
      return MY_GET_LINE.GET_WORD_NUMBER /= 0 and then
        TEXT_HANDLER.VALUE(LINE(1))(1) /= '#';
    end LINE_IS_SIGNIFICANT;

    procedure READ_NEXT_SIGNIFICANT_LINE is
    begin
      loop
        MY_GET_LINE.READ_NEXT_LINE;
        MY_GET_LINE.GET_WORDS (LINE);
        LINE_NO := LINE_NO + 1;
        exit when LINE_IS_SIGNIFICANT;
      end loop;
    end READ_NEXT_SIGNIFICANT_LINE;

  begin

    -- open file
    begin
      MY_GET_LINE.OPEN (ARGUMENT.GET_PARAMETER);
    exception
      when others =>
        TEXT_IO.PUT_LINE ("ERROR opening file " & ARGUMENT.GET_PARAMETER & ".");
        raise;
    end;
    LINE_NO := 1;

    -- Compute dimension from first line : nb of words - 1
    begin
      MY_GET_LINE.GET_WORDS (LINE);
      if not LINE_IS_SIGNIFICANT then
        READ_NEXT_SIGNIFICANT_LINE;
      end if;
      DIM := MY_GET_LINE.GET_WORD_NUMBER - 1;
      if DIM = 0 then
        TEXT_IO.PUT_LINE ("ERROR in file " & ARGUMENT.GET_PARAMETER
                          & " only one word in first line.");
        raise FILE_ERROR;
      end if;
    exception
      when TEXT_IO.END_ERROR =>
        TEXT_IO.PUT_LINE ("ERROR in file " & ARGUMENT.GET_PARAMETER
                          & " file is empty.");
        raise FILE_ERROR;
    end;

    -- Load matrix and vector from file.
    declare
      MATRIX : MY_SYSLIN.MATRIX(1..DIM, 1..DIM);
      VECTOR : MY_SYSLIN.VECTOR(1..DIM);
      SOLUTION : MY_SYSLIN.VECTOR(1..DIM);
    begin
      for I in 1 .. DIM loop
        -- Parse current line in matrix and vector
        begin
          for J in 1 .. DIM loop
            MATRIX (I, J) := GET_FLOAT(TEXT_HANDLER.VALUE(LINE(J)));
          end loop;
          VECTOR (I) := GET_FLOAT(TEXT_HANDLER.VALUE(LINE(DIM+1)));
        exception
          when others =>
            TEXT_IO.PUT_LINE ("ERROR, when reading data at line "
                              & INTEGER'IMAGE(LINE_NO) & ".");
            raise FILE_ERROR;
        end;

        if I /= DIM then
          -- read next not empty line
          READ_NEXT_SIGNIFICANT_LINE;

          -- Check number of words
          if MY_GET_LINE.GET_WORD_NUMBER /= DIM + 1 then
            TEXT_IO.PUT_LINE ("ERROR in file. Wrong number of words at line "
                              & INTEGER'IMAGE(LINE_NO) & ".");
            raise FILE_ERROR;
          end if;
        end if;

      end loop;

      -- Check nothing else in file
      begin
        READ_NEXT_SIGNIFICANT_LINE;
        TEXT_IO.PUT_LINE ("ERROR. Unexpected data at line "
                          & INTEGER'IMAGE(LINE_NO) & ".");
        MY_GET_LINE.CLOSE;
        raise FILE_ERROR;
      exception
        when MY_GET_LINE.NO_MORE_LINE =>
          MY_GET_LINE.CLOSE;
      end;

      -- Solve
      begin
        SOLUTION :=  MY_SYSLIN.GAUSS(MATRIX, VECTOR);
      exception
        when MY_SYSLIN.DISCRIMINENT_ERROR =>
          TEXT_IO.PUT_LINE ("Unable to solve: Discriminent is nul.");
          return;
        when MY_SYSLIN.DIMENSION_ERROR =>
          TEXT_IO.PUT_LINE ("Unable to solve: ERROR in dimensions.");
          return;
        when others =>
          TEXT_IO.PUT_LINE ("ERROR solving linear system.");
          raise;
      end;

      -- Put solution
      for I in 1 .. DIM loop
        TEXT_IO.PUT ("X(" & NORMAL(I, 3) & ") = ");
        FLO_IO.PUT (SOLUTION(I));
        TEXT_IO.NEW_LINE;
      end loop;

    end;
  end;
  -- Done
end T_SYSLIN;
