with POINTS;
package FILE is

  subtype F_T_FILE_NAME is STRING;

  -- Read points from file
  function F_READ (NAME : F_T_FILE_NAME) return POINTS.P_T_THE_POINTS;

  -- Write points to file
  procedure F_WRITE (NAME : in F_T_FILE_NAME;
    THE_POINTS : in POINTS.P_T_THE_POINTS);

  -- Check if file exists
  function F_EXISTS (NAME : F_T_FILE_NAME) return BOOLEAN;

  -- File not found (or protected...)
  F_ACCESS_ERROR : exception;
  -- read/Write error
  F_IO_ERROR     : exception;

end FILE;
