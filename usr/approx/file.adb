with SEQUENTIAL_IO;
with DIRECTORY, TEXT_HANDLER, MATH;
package body FILE is

  MAGIC_X : constant POINTS.P_T_COORDINATE := 21.21;

  -- Point sequential read/write
  package F_POINTS_IO is new SEQUENTIAL_IO (POINTS.P_T_ONE_POINT);
  use F_POINTS_IO;

  -- read a file of points
  function F_READ (NAME : F_T_FILE_NAME) return POINTS.P_T_THE_POINTS is
    SIZE : NATURAL;
    FILE : FILE_TYPE;
    MAGIC_POINT : POINTS.P_T_ONE_POINT;
    use MATH;
  begin
    begin
      OPEN (FILE, IN_FILE, NAME);
    exception
      when others => raise F_ACCESS_ERROR;
    end;
    RESET (FILE);
    
    -- Check magic x and get size
    READ (FILE, MAGIC_POINT);
    if MAGIC_POINT.X /= MAGIC_X then
      raise F_IO_ERROR;
    end if;
    SIZE := NATURAL(MAGIC_POINT.Y);
    -- Should be int
    if POINTS.P_T_COORDINATE(SIZE) /= MAGIC_POINT.Y then
      raise F_IO_ERROR;
    end if;
    
    -- read the SIZE points
    declare
      THE_POINTS : POINTS.P_T_THE_POINTS (1 .. SIZE);
    begin
      for INDEX in 1 .. SIZE loop
        READ (FILE, THE_POINTS (INDEX));
      end loop;
      CLOSE (FILE);
      POINTS.P_SAVED;
      return (THE_POINTS);
    end;
  exception
    when others =>
      CLOSE (FILE);
      raise F_IO_ERROR;
  end F_READ;

  -- Write the points in file
  procedure F_WRITE (NAME : in F_T_FILE_NAME;
    THE_POINTS : in POINTS.P_T_THE_POINTS) is
    FILE : FILE_TYPE;
  begin
    begin
      OPEN(FILE, OUT_FILE, NAME);
      DELETE(FILE);
      CREATE(FILE, OUT_FILE, NAME);
    exception
      when NAME_ERROR =>
        -- New file
        begin
          CREATE (FILE, OUT_FILE, NAME);
        exception
          when others => raise F_ACCESS_ERROR;
        end;
      when others => raise F_ACCESS_ERROR;
    end;
    WRITE (FILE, (X => MAGIC_X, 
                  Y => POINTS.P_T_COORDINATE(THE_POINTS'LENGTH)));
    begin
      for INDEX in THE_POINTS'RANGE loop
        WRITE (FILE, THE_POINTS(INDEX));
      end loop;
    exception
      when others =>
        CLOSE (FILE);
        raise F_IO_ERROR;
    end;
    POINTS.P_SAVED;
    CLOSE (FILE);
  end F_WRITE;

  -- Check if file exists
  function F_EXISTS (NAME : F_T_FILE_NAME) return BOOLEAN is
    FILE : FILE_TYPE;
  begin
    OPEN (FILE, IN_FILE, NAME);
    CLOSE (FILE);
    return (TRUE);
  exception
    when others => return FALSE;
  end F_EXISTS;

end FILE;
