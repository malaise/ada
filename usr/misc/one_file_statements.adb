with TEXT_IO;
with NORMAL;

package body ONE_FILE_STATEMENTS is

  TOTAL : NATURAL := 0;
  FILE_ERROR : exception;
  function COUNT_STATEMENTS_OF_FILE (FILE_NAME : STRING) return NATURAL is

    FILE  : TEXT_IO.FILE_TYPE;
    C     : CHARACTER := ' ';
    STATEMENTS : NATURAL := 0;
    LEVELS : NATURAL := 0;

    procedure GET (FILE : in TEXT_IO.FILE_TYPE; C : out CHARACTER) is
    begin
      TEXT_IO.GET (FILE, C);
    exception
      when TEXT_IO.END_ERROR =>
        raise;
      when others =>
        TEXT_IO.PUT_LINE ("Exception raised when reading line "
                        & TEXT_IO.POSITIVE_COUNT'IMAGE(TEXT_IO.LINE(FILE))
                        & " of file " & FILE_NAME);
        raise FILE_ERROR;
    end GET;

    procedure CLOSE (FILE : in out TEXT_IO.FILE_TYPE) is
    begin
      TEXT_IO.CLOSE (FILE);
    exception
      when others => null;
    end CLOSE;

  begin

    begin
      TEXT_IO.OPEN (FILE, TEXT_IO.IN_FILE, FILE_NAME);
    exception
      when others =>
        TEXT_IO.PUT_LINE ("Exception raised when opening file " & FILE_NAME);
        raise FILE_ERROR;
    end;

    loop

      GET (FILE, C);

      -- Check for comment on the line
      if C = '-' then
        GET (FILE, C);
        -- Which is signaled by the '-' following a '-'
        if C = '-' then
          -- Then just skip the rest of the line and go to the next
          TEXT_IO.SKIP_LINE (FILE);
        end if;
      end if;

      -- Check for one of the characters which introduce code constructs
      -- like string or character litteral or formal parameter list
      -- within which a ';' does not terminate a "line of code"

      if C = '(' or C = '"' or C = '%' or C = ''' then

        -- Check for opening parentheses
        -- Every ';' within is a formal parameter list
        if  C = '(' then
          -- Count the number of levels of parentheses
          LEVELS := LEVELS + 1;

          -- Read ahead until the whole construct is closed, LEVEL = 0
          while LEVELS > 0 loop
            GET (FILE, C);
            if C = '(' then
              -- Increase the level if another '(' is found
              LEVELS := LEVELS + 1;
            elsif C = ')' then
              -- Decrease the leval if a ')' is found
              LEVELS := LEVELS - 1;
            end if;
          end loop;

        -- Now, check for string brackets of either kind, " or %
        elsif C = '"' or C = '%' then
          -- Treat them in parallel, one must lead off
          if C = '"' then
            loop
              GET (FILE, C);
              -- Loop until  the close comes
              -- If there is a doubled character it just starts again
              exit when C = '"';
            end loop;
          elsif C = '%' then
            -- The '%' is handled exactly the same way as '"'
            loop
              GET (FILE, C);
              exit when C = '%';
            end loop;
          end if;

        elsif C = ''' then
          -- Character literals are just three characters long including '
          GET (FILE, C);
          GET (FILE, C);
        end if;

    elsif C = ';' then
      -- Any ';' that can be found at this point after all exclusions
      -- must be a valid "line of code terminator"
      STATEMENTS := STATEMENTS + 1;
    end if;
  end loop;

  exception
    when TEXT_IO.END_ERROR =>
      CLOSE (FILE);
      return STATEMENTS;
    when FILE_ERROR =>
      CLOSE (FILE);
      raise;
    when others =>
      CLOSE (FILE);
      TEXT_IO.PUT_LINE ("Exception raised when processing line "
                      & TEXT_IO.POSITIVE_COUNT'IMAGE(TEXT_IO.LINE(FILE))
                      & " of file " & FILE_NAME);
      raise;
  end COUNT_STATEMENTS_OF_FILE;

  -- If FILE_NAME is empty, put total so far and reset it
  procedure PRINT_STATEMENTS_OF_FILE (
             FILE_NAME : STRING;
             PUT_IT : in BOOLEAN := TRUE) is

    FILE_NAME_LEN : constant NATURAL := FILE_NAME'LENGTH;
    COUNT : INTEGER range -1 ..INTEGER'LAST;
    MAX_TAB : constant := 60;
    MAX_DIG : constant := 8;
    GAP : constant STRING := "  ";
  begin

    if FILE_NAME = "" then
      if PUT_IT then
        for I in INTEGER range 1 .. MAX_TAB + GAP'LENGTH + MAX_DIG + 1 loop
          TEXT_IO.PUT ("-");
        end loop;
        TEXT_IO.NEW_LINE;

        declare
          TOTAL_STR : constant STRING := "TOTAL statements";
        begin
          TEXT_IO.PUT (TOTAL_STR);
          for I in INTEGER range TOTAL_STR'LENGTH .. MAX_TAB loop
            TEXT_IO.PUT (" ");
          end loop;
        end;
        TEXT_IO.PUT_LINE (GAP & NORMAL(TOTAL, MAX_DIG));
      end if;
      TOTAL := 0;

    else 

      begin
        COUNT := ONE_FILE_STATEMENTS.COUNT_STATEMENTS_OF_FILE(FILE_NAME);
      exception
        when others =>
          COUNT := -1;
      end;

      if PUT_IT then
        TEXT_IO.PUT (FILE_NAME);
        if FILE_NAME_LEN < MAX_TAB then
          TEXT_IO.PUT (" ");
          for I in FILE_NAME_LEN+2 .. MAX_TAB loop
            TEXT_IO.PUT (".");
          end loop;
        elsif FILE_NAME_LEN > MAX_TAB then
          TEXT_IO.NEW_LINE;
          for I in INTEGER range 1 .. MAX_TAB loop
            TEXT_IO.PUT (".");
          end loop;
        end if;

        if COUNT >= 0 then
          TEXT_IO.PUT_LINE (GAP & NORMAL(COUNT, MAX_DIG));
        else
          TEXT_IO.PUT_LINE (GAP & " SKIPPED");
        end if;
      end if;

      if COUNT >= 0 then
        TOTAL := TOTAL + COUNT;
      end if;

    end if;

  end PRINT_STATEMENTS_OF_FILE;

end ONE_FILE_STATEMENTS;

