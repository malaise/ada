-- Source file indenter. See procedure USAGE.
with TEXT_IO;

with SYS_CALLS;

with MY_IO, TEXT_HANDLER, ARGUMENT;

procedure ADAND is
  LINE_DEB, LINE_FIN : POSITIVE;
  L : NATURAL;

  subtype INDENT_RANGE is INTEGER range -12 .. +12;
  IND : INDENT_RANGE;

  FILE_NAME : TEXT_HANDLER.TEXT(1024);
  SAV_SUF : constant STRING := ".bak";
  FILE_SUF : TEXT_HANDLER.TEXT(1024);

  STR_MAX : constant := 500;
  STR : STRING(1..STR_MAX+1);
  LST : NATURAL;

  TLD, TLF, TI : TEXT_HANDLER.TEXT(10);

  F, FB : TEXT_IO.FILE_TYPE;

  SYSTEM_CALL_ERROR : exception;

  procedure USAGE is
  begin
    MY_IO.PUT_LINE ("Usage: indent "
     & "[-F]file_name [-ffirst_line] [-llast_line] [-i[+|-]col]");
  end USAGE;


begin

  -- parse arguments (file_name, lines, indentation)
  begin
    ARGUMENT.GET_PARAMETER(FILE_NAME, 1, ARGUMENT.NOT_KEY);
  exception
    when ARGUMENT.ARGUMENT_NOT_FOUND =>

      begin
        ARGUMENT.GET_PARAMETER(FILE_NAME, 1, "F");
      exception
        when ARGUMENT.ARGUMENT_NOT_FOUND =>
          USAGE;
          raise;
        when CONSTRAINT_ERROR =>
          MY_IO.PUT_LINE ("File name too long to store.");
          raise;
      end;

    when CONSTRAINT_ERROR =>
      MY_IO.PUT_LINE ("File name too long to store.");
      raise;
  end;

  begin
    ARGUMENT.GET_PARAMETER(TLD, 1, "f");
    LINE_DEB := POSITIVE'VALUE(TEXT_HANDLER.VALUE(TLD));
  exception
    when ARGUMENT.ARGUMENT_NOT_FOUND =>
      LINE_DEB := 1;
    when CONSTRAINT_ERROR | NUMERIC_ERROR =>
      USAGE;
      raise;
  end;

  begin
    ARGUMENT.GET_PARAMETER(TLF, 1, "l");
    LINE_FIN := POSITIVE'VALUE(TEXT_HANDLER.VALUE(TLF));
  exception
    when ARGUMENT.ARGUMENT_NOT_FOUND =>
      LINE_FIN := POSITIVE'LAST;
    when CONSTRAINT_ERROR | NUMERIC_ERROR =>
      USAGE;
      raise;
  end;

  begin
    ARGUMENT.GET_PARAMETER(TI, 1, "i");
    IND := INDENT_RANGE'VALUE(TEXT_HANDLER.VALUE(TI));
  exception
    when ARGUMENT.ARGUMENT_NOT_FOUND =>
      IND := 2;
    when CONSTRAINT_ERROR | NUMERIC_ERROR =>
      USAGE;
      raise;
  end;

  -- mv file to file.bak
  declare
    NO_ERR : BOOLEAN;
  begin
    -- build .BAK file name
    TEXT_HANDLER.SET (FILE_SUF, FILE_NAME);
    TEXT_HANDLER.APPEND (FILE_SUF, SAV_SUF);

    -- eventualy remove .bak file
    NO_ERR := SYS_CALLS.UNLINK (TEXT_HANDLER.VALUE(FILE_SUF));
    -- rename file to file.bak
    NO_ERR := SYS_CALLS.RENAME (TEXT_HANDLER.VALUE(FILE_NAME),
            TEXT_HANDLER.VALUE(FILE_SUF));
    if not NO_ERR then
      raise SYSTEM_CALL_ERROR;
    end if;
  exception
    when SYSTEM_CALL_ERROR =>
      MY_IO.PUT_LINE ("ERROR : " & SYS_CALLS.STR_ERROR(SYS_CALLS.ERRNO)
       & " renaming file "
       & TEXT_HANDLER.VALUE(FILE_NAME) & " to "
       & TEXT_HANDLER.VALUE(FILE_SUF));
      raise;
    when CONSTRAINT_ERROR =>
      MY_IO.PUT_LINE ("File name too long to build commands.");
      raise;
  end;

  -- open file.bak file and create file
  begin
    TEXT_IO.OPEN (FB, TEXT_IO.IN_FILE,
     TEXT_HANDLER.VALUE(FILE_SUF));
  exception
    when others =>
      MY_IO.PUT_LINE ("Error opening file " &
       TEXT_HANDLER.VALUE(FILE_SUF));
      raise;
  end;
  begin
    TEXT_IO.CREATE (F, TEXT_IO.OUT_FILE,
     TEXT_HANDLER.VALUE(FILE_NAME));
  exception
    when others =>
      MY_IO.PUT_LINE ("Error creating file " &
       TEXT_HANDLER.VALUE(FILE_NAME));
  end;

  L := 0;
  loop
    -- read file.bak line
    TEXT_IO.GET_LINE (FB, STR, LST);
    L := L + 1;
    if LST /= 0 and then L>= LINE_DEB and then L<=LINE_FIN then
      -- if ld<=line<=lf and non empty then indent
      if IND > 0 then
        if LST + IND <= STR_MAX then
          STR (IND+1 .. IND+LST) := STR (1 .. LST);
          STR (1 .. IND) := (others => ' ');
          LST := LST + IND;
        end if;
      elsif IND < 0 then
        declare
          MIND : constant POSITIVE := - IND;
          SPACES : constant STRING (1..MIND) := (others => ' ');
        begin
          if LST >= MIND and then STR (1 .. MIND) = SPACES then
            STR (1 .. LST - MIND) := STR (MIND+1 .. LST);
            LST := LST - MIND;
          end if;
        end;
      end if;
    end if;

    -- write line in file
    TEXT_IO.PUT_LINE (F, STR(1..LST));

    exit when TEXT_IO.END_OF_FILE(FB);

  end loop;

  -- close files
  TEXT_IO.CLOSE (FB);
  TEXT_IO.CLOSE (F);

  MY_IO.PUT_LINE ("Done.");

exception
  when others =>
    MY_IO.PUT_LINE ("Exception "
     & "raised when processing file "
     & TEXT_HANDLER.VALUE (FILE_NAME));
    raise;
end ADAND;

