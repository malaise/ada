with CON_IO, TEXT_HANDLER, DIRECTORY;
package AFPX_TYP is

  -- Version of AFPX
  AFPX_VERSION : constant FLOAT := 2.1;

  -- Files path
  DEST_PATH : TEXT_HANDLER.TEXT (DIRECTORY.MAX_DIR_NAME_LEN + 1);

  -- Files name
  DSCR_FILE_NAME : constant STRING := "AFPX.DSC";
  FLD_FILE_NAME  : constant STRING := "AFPX.FLD";
  INIT_FILE_NAME : constant STRING := "AFPX.INI";

  -- Descriptor, field index
  type DESCRIPTOR_RANGE is new POSITIVE range 1 .. 50;
  type ABSOLUTE_FIELD_RANGE is new NATURAL range 0 .. 200;
  subtype FIELD_RANGE is ABSOLUTE_FIELD_RANGE
          range 1 .. ABSOLUTE_FIELD_RANGE 'LAST;

  -- A descriptor
  type DSCR_REC is record
    -- To be checked prior to loading
    VERSION    : FLOAT;
    -- To generate refresh. True in file if used
    MODIFIED   : BOOLEAN;
    -- In the file: index of the dscr. In memory, its No
    DSCR_INDEX : DESCRIPTOR_RANGE;
    -- Nb of fields of the dscr
    NB_FIELDS  : ABSOLUTE_FIELD_RANGE;
  end record;
  type DESCRIPTORS_ARRAY is array (DESCRIPTOR_RANGE) of DSCR_REC;

  -- Field kind
  type FIELD_KIND_LIST is (PUT, BUTTON, GET);

  -- Field colors
  type COLORS_REC is record
    FOREGROUND : CON_IO.EFFECTIVE_COLORS;
    BLINK_STAT : CON_IO.EFFECTIVE_BLINK_STATS;
    BACKGROUND : CON_IO.EFFECTIVE_BASIC_COLORS;
    SELECTED   : CON_IO.EFFECTIVE_BASIC_COLORS;
  end record;

  -- Width and height of a field
  subtype HEIGHT_RANGE is POSITIVE range 1 .. CON_IO.ROW_RANGE_LAST + 1;
  subtype WIDTH_RANGE  is POSITIVE range 1 .. CON_IO.COL_RANGE_LAST + 1;

  -- Characters of the fields
  MAX_INIT_LEN : constant INTEGER :=
   (CON_IO.ROW_RANGE_LAST + 1) * (CON_IO.COL_RANGE_LAST + 1);
  subtype CHAR_STR_RANGE is POSITIVE range 1 .. MAX_INIT_LEN;

  -- A field
  type FIELD_REC is record
    -- First (0) field is the LIST.
    --   Set if KIND is BUTTON, and not if KIND is PUT
    -- Others are fields
    KIND : FIELD_KIND_LIST;
    -- Field activation
    ACTIVATED : BOOLEAN;
    -- Field protection for GET / BUTTON fields
    ISPROTECTED : BOOLEAN;
    -- Upper left, lower_right corners of the field
    UPPER_LEFT, LOWER_RIGHT : CON_IO.SQUARE;
    -- Width, height
    HEIGHT : HEIGHT_RANGE;
    WIDTH  : WIDTH_RANGE;
    -- Colors
    COLORS : COLORS_REC;
    -- Index in CHAR_STR of start of field content
    CHAR_INDEX : CHAR_STR_RANGE;
  end record;

  -- The array of fields of current descriptor
  type FIELDS_ARRAY is array (ABSOLUTE_FIELD_RANGE) of FIELD_REC;

  -- The init/current charcters of current descriptor
  subtype CHAR_STR is STRING (CHAR_STR_RANGE);


  -- Check is square (relative to field) is in field
  function IN_FIELD (FIELD  : in FIELD_REC;
                     SQUARE : in CON_IO.SQUARE) return BOOLEAN;

  -- Check is square (absolute) is in field
  function IN_FIELD_ABSOLUTE (FIELD  : in FIELD_REC;
                              SQUARE : in CON_IO.SQUARE) return BOOLEAN;

end AFPX_TYP;

