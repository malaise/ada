with UNCHECKED_CONVERSION;
with SYSTEM.BIT_OPS;
with INTERFACES;

package body BIT_OPS is

  function TO_UNSIGNED_32 is new UNCHECKED_CONVERSION
    (SOURCE => INTEGER, TARGET => INTERFACES.UNSIGNED_32);
  function TO_INTEGER is new UNCHECKED_CONVERSION
    (SOURCE => INTERFACES.UNSIGNED_32, TARGET => INTEGER);

  function TO_UNSIGNED_64 is new UNCHECKED_CONVERSION
    (SOURCE => LONG_LONG_INTEGER, TARGET => INTERFACES.UNSIGNED_64);
  function TO_LONG_LONG_INTEGER is new UNCHECKED_CONVERSION
    (SOURCE => INTERFACES.UNSIGNED_64, TARGET => LONG_LONG_INTEGER);

  function "AND" (LEFT, RIGHT : INTEGER) return INTEGER is
    RES : INTEGER;
  begin
    SYSTEM.BIT_OPS.BIT_AND (LEFT'ADDRESS, LEFT'SIZE,
                            RIGHT'ADDRESS, RIGHT'SIZE,
                            RES'ADDRESS);
    return RES;
  end "AND";

  function "OR"  (LEFT, RIGHT : INTEGER) return INTEGER is
    RES : INTEGER;
  begin
    SYSTEM.BIT_OPS.BIT_OR (LEFT'ADDRESS, LEFT'SIZE,
                            RIGHT'ADDRESS, RIGHT'SIZE,
                            RES'ADDRESS);
    return RES;
  end "OR";

  function "XOR" (LEFT, RIGHT : INTEGER) return INTEGER is
    RES : INTEGER;
  begin
    SYSTEM.BIT_OPS.BIT_XOR (LEFT'ADDRESS, LEFT'SIZE,
                            RIGHT'ADDRESS, RIGHT'SIZE,
                            RES'ADDRESS);
    return RES;
  end "XOR";

  function "NOT" (VAL : INTEGER) return INTEGER is
    RES : INTEGER;
  begin
    SYSTEM.BIT_OPS.BIT_NOT (VAL'ADDRESS, VAL'SIZE,
                            RES'ADDRESS);
    return RES;
  end "NOT";

  function SHL (VAL : INTEGER; BITS : INTEGER) return INTEGER is
  begin
    return TO_INTEGER(INTERFACES.SHIFT_LEFT(TO_UNSIGNED_32(VAL), BITS));
  end SHL;

  function SHR (VAL : INTEGER; BITS : INTEGER) return INTEGER is
  begin
    return TO_INTEGER(INTERFACES.SHIFT_RIGHT(TO_UNSIGNED_32(VAL), BITS));
  end SHR;

  function "AND" (LEFT, RIGHT : LONG_LONG_INTEGER) return LONG_LONG_INTEGER is
    RES : LONG_LONG_INTEGER;
  begin
    SYSTEM.BIT_OPS.BIT_AND (LEFT'ADDRESS, LEFT'SIZE,
                            RIGHT'ADDRESS, RIGHT'SIZE,
                            RES'ADDRESS);
    return RES;
  end "AND";

  function "OR"  (LEFT, RIGHT : LONG_LONG_INTEGER) return LONG_LONG_INTEGER is
    RES : LONG_LONG_INTEGER;
  begin
    SYSTEM.BIT_OPS.BIT_OR (LEFT'ADDRESS, LEFT'SIZE,
                            RIGHT'ADDRESS, RIGHT'SIZE,
                            RES'ADDRESS);
    return RES;
  end "OR";

  function "XOR" (LEFT, RIGHT : LONG_LONG_INTEGER) return LONG_LONG_INTEGER is
    RES : LONG_LONG_INTEGER;
  begin
    SYSTEM.BIT_OPS.BIT_XOR (LEFT'ADDRESS, LEFT'SIZE,
                            RIGHT'ADDRESS, RIGHT'SIZE,
                            RES'ADDRESS);
    return RES;
  end "XOR";

  function "NOT" (VAL : LONG_LONG_INTEGER) return LONG_LONG_INTEGER is
    RES : LONG_LONG_INTEGER;
  begin
    SYSTEM.BIT_OPS.BIT_NOT (VAL'ADDRESS, VAL'SIZE,
                            RES'ADDRESS);
    return RES;
  end "NOT";

  function SHL (VAL : LONG_LONG_INTEGER; BITS : INTEGER)
  return LONG_LONG_INTEGER is
  begin
    return TO_LONG_LONG_INTEGER(
      INTERFACES.SHIFT_LEFT(TO_UNSIGNED_64(VAL), BITS));
  end SHL;

  function SHR (VAL : LONG_LONG_INTEGER; BITS : INTEGER)
  return LONG_LONG_INTEGER is
  begin
    return TO_LONG_LONG_INTEGER(
      INTERFACES.SHIFT_RIGHT(TO_UNSIGNED_64(VAL), BITS));
  end SHR;

end BIT_OPS;
