with TEXT_IO;

with TEXT_HANDLER;
with MATH;

with GET_LINE;
with GET_FLOAT;

package body FILE is

  MAX_DIM : constant := 100;

  FILE_READ : BOOLEAN := FALSE;

  -- Input mattrix splitted in rows to reduce size of object
  type FLOAT_CELL_RANGE is digits 5 range 0.00 .. 100.00;
  type INPUT_ROW_TAB is array (1 .. MAX_DIM) of FLOAT_CELL_RANGE;
  type INPUT_ROW_TAB_ACCESS is access INPUT_ROW_TAB;
  type INPUT_MATTRIX_TAB is array (1 .. MAX_DIM) of
    INPUT_ROW_TAB_ACCESS;
  INPUT_MATTRIX : INPUT_MATTRIX_TAB
                := (others => new INPUT_ROW_TAB);

  LOC_KIND : TYPES.MATTRIX_KIND_LIST;

  function READ (FILE_NAME : STRING) return TYPES.MATTRIX_REC is

    package MY_GET_LINE is new GET_LINE(
      MAX_WORD_LEN => 6, -- 100.00
      MAX_WORD_NB => MAX_DIM,
      MAX_LINE_LEN => 1024);
    LINE  : MY_GET_LINE.LINE_ARRAY;
    LINE_NO : TEXT_IO.COUNT := 0;
    DIM : POSITIVE;
    F   : FLOAT_CELL_RANGE;

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
        LINE_NO := TEXT_IO."+" (LINE_NO, 1);
        exit when LINE_IS_SIGNIFICANT;
      end loop;
    end READ_NEXT_SIGNIFICANT_LINE;
  begin
    -- Open file
    begin
      MY_GET_LINE.OPEN (FILE_NAME);
    exception
      when others =>
        TEXT_IO.PUT_LINE ("ERROR opening file " & FILE_NAME);
        raise READ_ERROR;
    end;

    -- Read mattrix kind
    begin
      MY_GET_LINE.GET_WORDS (LINE);
      if not LINE_IS_SIGNIFICANT then
        READ_NEXT_SIGNIFICANT_LINE;
      end if;
    exception
      when MY_GET_LINE.NO_MORE_LINE =>
        TEXT_IO.PUT_LINE ("ERROR in file " & FILE_NAME
                          & ". File is empty.");
        raise READ_ERROR;
    end;
    if MY_GET_LINE.GET_WORD_NUMBER /= 1 then
      TEXT_IO.PUT_LINE ("ERROR in file " & FILE_NAME
                        & ", only one word, WISH or REGRET, allowed in first line.");
      raise READ_ERROR;
    end if;
    begin
      LOC_KIND := TYPES.MATTRIX_KIND_LIST'VALUE (TEXT_HANDLER.VALUE(LINE(1)));
    exception
      when others =>
        TEXT_IO.PUT_LINE ("ERROR in file " & FILE_NAME
                        & ", only one word, WISH or REGRET, allowed in first line.");
        raise READ_ERROR;
    end;

    -- Compute dimension from first line : nb of words
    begin
      READ_NEXT_SIGNIFICANT_LINE;
      DIM := MY_GET_LINE.GET_WORD_NUMBER;
    exception
      when TEXT_IO.END_ERROR =>
        TEXT_IO.PUT_LINE ("ERROR in file " & FILE_NAME
                          & ", no mattrix.");
        raise READ_ERROR;
      when others =>
        TEXT_IO.PUT_LINE ("ERROR in file " & FILE_NAME
                          & ", impossible to compute mattrix size.");
    end;

    -- Load matrix and vector from file.
    for I in 1 .. DIM loop
      -- Parse current line in matrix and vector
      begin
        for J in 1 .. DIM loop
          F := FLOAT_CELL_RANGE (GET_FLOAT(TEXT_HANDLER.VALUE(LINE(J))));
          if F > 100.00 or else MATH.FRAC(LONG_FLOAT(F)) * 100.0 > 100.0 then
            raise READ_ERROR;
          end if;
          INPUT_MATTRIX(I).all(J) := F;
        end loop;
      exception
        when others =>
          TEXT_IO.PUT_LINE ("ERROR, when reading data at line "
                            & TEXT_IO.COUNT'IMAGE(LINE_NO) & " of file " & FILE_NAME);
          raise READ_ERROR;
      end;

      if I /= DIM then
        -- read next not empty line
        READ_NEXT_SIGNIFICANT_LINE;

        -- Check number of words
        if MY_GET_LINE.GET_WORD_NUMBER /= DIM then
          TEXT_IO.PUT_LINE ("ERROR in file. Wrong number of words at line "
                            & TEXT_IO.COUNT'IMAGE(LINE_NO) & " of file " & FILE_NAME);
          raise READ_ERROR;
        end if;
      end if;

    end loop;

    -- Check nothing else in file
    begin
      READ_NEXT_SIGNIFICANT_LINE;
      TEXT_IO.PUT_LINE ("ERROR. Unexpected data at line "
                        & TEXT_IO.COUNT'IMAGE(LINE_NO) & " of file " & FILE_NAME);
      MY_GET_LINE.CLOSE;
      raise READ_ERROR;
    exception
      when MY_GET_LINE.NO_MORE_LINE =>
        MY_GET_LINE.CLOSE;
    end;

    declare
     LOC_MATTRIX : TYPES.MATTRIX_TAB (1 .. DIM, 1 .. DIM);
    begin
      -- Affect to INTEGERS
      for I in 1 .. DIM loop
        for J in 1 .. DIM loop
          LOC_MATTRIX(I, J) := TYPES.CELL_RANGE(
             MATH.ROUND (LONG_FLOAT((INPUT_MATTRIX(I).all(J))) * 100.0));
        end loop;
      end loop;

      if TYPES."=" (LOC_KIND, TYPES.WISH) then
        -- Make a regret mattrix by substracting each to 10_000
        for I in 1 .. DIM loop
          for J in 1 .. DIM loop
            LOC_MATTRIX(I, J) := 10_000 - LOC_MATTRIX(I, J);
          end loop;
        end loop;
      end if;

      FILE_READ := TRUE;
      return (DIM => DIM, NOTES => LOC_MATTRIX);
    end;

  end READ;

  function GET_KIND return TYPES.MATTRIX_KIND_LIST is
  begin
    if not FILE_READ then
      raise FILE_NOT_READ;
    end if;
    return LOC_KIND;
  end GET_KIND;

  function GET_NOTE (ROW, COL : POSITIVE) return FLOAT is
  begin
    if not FILE_READ then
      raise FILE_NOT_READ;
    end if;
    return FLOAT(INPUT_MATTRIX(ROW).all(COL));
  end GET_NOTE;

end FILE;

