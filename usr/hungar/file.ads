with TYPES;

package FILE is

  -- Delta above which one considers it is zero (for loss printing)
  EPSILON : constant FLOAT := 1.0E-5;

  function READ (FILE_NAME : STRING) return TYPES.MATTRIX_REC;

  function GET_KIND return TYPES.MATTRIX_KIND_LIST;
  function GET_NOTE (ROW, COL : POSITIVE) return FLOAT;

  READ_ERROR : exception;
  FILE_NOT_READ : exception;
  
end FILE;
