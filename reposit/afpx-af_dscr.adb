with DIRECT_IO;
with SYS_CALLS, TEXT_HANDLER;
separate (AFPX)
package body AF_DSCR is

  -- Direct_io of descriptors, fields, init strings
  package DSCR_IO is new DIRECT_IO (AFPX_TYP.DESCRIPTORS_ARRAY);
  DSCR_FILE : DSCR_IO.FILE_TYPE;
  package FLD_IO  is new DIRECT_IO (AFPX_TYP.FIELDS_ARRAY);
  FLD_FILE : FLD_IO.FILE_TYPE;
  package INIT_IO is new DIRECT_IO (AFPX_TYP.CHAR_STR);
  INIT_FILE : INIT_IO.FILE_TYPE;

  -- Has a descriptor been set
  DSCR_SET : BOOLEAN := FALSE;

  -- Colors from file
  INIT_COLORS : array (AFPX_TYP.ABSOLUTE_FIELD_RANGE) of AFPX_TYP.COLORS_REC;

  -- Chars from file
  INIT_STR : AFPX_TYP.CHAR_STR;

  -- Descriptor read
  DSCRS : AFPX_TYP.DESCRIPTORS_ARRAY;

  -- Load a descriptor
  procedure LOAD_DSCR (DSCR_NO : in AFPX_TYP.DESCRIPTOR_RANGE) is
    DSCR_INDEX : AFPX_TYP.DESCRIPTOR_RANGE;
  begin

    -- Check it is defined
    if not DSCRS(DSCR_NO).MODIFIED then
      raise NO_DESCRIPTOR;
    end if;

    -- Save index
    DSCR_INDEX := DSCRS(DSCR_NO).DSCR_INDEX;

    -- Copy descriptor, read fields and chars
    CURRENT_DSCR := DSCRS(DSCR_NO);
    CURRENT_DSCR.DSCR_INDEX := DSCR_NO;

    FLD_IO.READ  (FLD_FILE , FIELDS,   FLD_IO.POSITIVE_COUNT(DSCR_INDEX));
    INIT_IO.READ (INIT_FILE, INIT_STR, INIT_IO.POSITIVE_COUNT(DSCR_INDEX));

    -- Copy descriptor, save colors, copy chars
    for I in
    AFPX_TYP.ABSOLUTE_FIELD_RANGE'FIRST .. DSCRS(DSCR_NO).NB_FIELDS loop
      INIT_COLORS(I) := FIELDS(I).COLORS;
    end loop;
    CHARS := INIT_STR;

    DSCR_SET := TRUE;

  end LOAD_DSCR;

  -- Check if a descriptor i in use
  procedure CHECK is
  begin
    if not DSCR_SET then
      raise NO_DESCRIPTOR;
    end if;
  end CHECK;


  -- Check if a descriptor i in use and if field is valid
  procedure CHECK (FIELD_NO : in AFPX_TYP.ABSOLUTE_FIELD_RANGE) is
    use AFPX_TYP;
  begin
    CHECK;

    if FIELD_NO = LFN then
      if FIELDS(LFN).KIND = AFPX_TYP.BUTTON then
        -- A list in the descriptor
        return;
      end if;
    else
      if FIELD_NO <= CURRENT_DSCR.NB_FIELDS then
        -- This field is in the descriptor
        return;
      end if;
    end if;
    -- No such field/list
    raise INVALID_FIELD;
  end CHECK;

  -- Load a field
  procedure LOAD_FIELD (FIELD_NO : in AFPX_TYP.ABSOLUTE_FIELD_RANGE;
        LOAD_COLORS : in BOOLEAN;
        LOAD_CHARS  : in BOOLEAN) is
    FIELD : constant AFPX_TYP.FIELD_REC := FIELDS(FIELD_NO);
    NB_CHARS : constant POSITIVE := FIELD.HEIGHT * FIELD.WIDTH;
    use AFPX_TYP;
  begin
    CHECK (FIELD_NO);
    FIELDS(FIELD_NO).ACTIVATED := TRUE;
    FIELDS(FIELD_NO).ISPROTECTED := FALSE;
    if LOAD_COLORS then
      FIELDS(FIELD_NO).COLORS := INIT_COLORS(FIELD_NO);
    end if;

    if LOAD_CHARS and then FIELD_NO /= 0 then
      -- Copy the nb_chars from init_str to char_str
      for I in FIELD.CHAR_INDEX .. FIELD.CHAR_INDEX + NB_CHARS - 1 loop
        CHARS(I) := INIT_STR(I);
      end loop;
    end if;
  exception
    when others =>
      raise AFPX_INTERNAL_ERROR;
  end LOAD_FIELD;

begin
  -- Try to getenv data dir then to open the files
  declare
    FILE_DIR_ENV_NAME : constant STRING := "AFPX_DATA_DIR";
    ENV_SET, ENV_TRUNC : BOOLEAN;
    ENV_VALUE : STRING (1 .. AFPX_TYP.DEST_PATH.MAX_LEN - 1);
    ENV_LEN : NATURAL;
  begin
    SYS_CALLS.GETENV(FILE_DIR_ENV_NAME, ENV_SET, ENV_TRUNC, ENV_VALUE, ENV_LEN);
    if ENV_SET and then not ENV_TRUNC and then ENV_LEN /= 0 then
      TEXT_HANDLER.SET (AFPX_TYP.DEST_PATH, ENV_VALUE (1 .. ENV_LEN));
    else
      TEXT_HANDLER.SET (AFPX_TYP.DEST_PATH, ".");
    end if;
    TEXT_HANDLER.APPEND (AFPX_TYP.DEST_PATH, "/");

    DSCR_IO.OPEN (DSCR_FILE, DSCR_IO.IN_FILE,
                  TEXT_HANDLER.VALUE(AFPX_TYP.DEST_PATH) & AFPX_TYP.DSCR_FILE_NAME);
    FLD_IO.OPEN  (FLD_FILE,  FLD_IO.IN_FILE,
                  TEXT_HANDLER.VALUE(AFPX_TYP.DEST_PATH) & AFPX_TYP.FLD_FILE_NAME);
    INIT_IO.OPEN (INIT_FILE, INIT_IO.IN_FILE,
                  TEXT_HANDLER.VALUE(AFPX_TYP.DEST_PATH) & AFPX_TYP.INIT_FILE_NAME);
  exception
    when others =>
      raise FILE_NOT_FOUND;
  end;

  -- Read first descriptor
  begin
    DSCR_IO.READ (DSCR_FILE, DSCRS, 1);
  exception
    when others =>
      raise FILE_READ_ERROR;
  end;

  -- Check AFPX version
  if DSCRS(1).VERSION /= AFPX_TYP.AFPX_VERSION then
    raise FILE_VERSION_ERROR;
  end if;
end AF_DSCR;
