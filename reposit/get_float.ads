package GET_FLOAT is

  -- Get a float (converts int if needed)
  -- May raise CONSTRAINT_ERROR
  function GET_FLOAT (STR : STRING) return FLOAT;

  type INT_FLOAT_REC (IS_FLOAT : BOOLEAN := TRUE) is record
    case IS_FLOAT is
      when TRUE =>
        FLOAT_VALUE : FLOAT;
      when FALSE =>
        INT_VALUE : INTEGER;
    end case;
  end record;

  -- Gets a float or an integer
  function GET_INT_FLOAT (STR : STRING) return INT_FLOAT_REC;

end GET_FLOAT;

