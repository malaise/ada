with TEXT_HANDLER;
with POINTS, SCREEN, RESOL;
package DIALOG is

  -- If points are not saved, ask for confirmation
  function CONFIRM_LOST return BOOLEAN;

  -- Remove trailing spaces. No heading nor intermediate spaces allowed
  procedure PARSE_SPACES (TXT : in out TEXT_HANDLER.TEXT;
                          OK : out BOOLEAN);

  -- Get a coordinate
  --  If SET is set IN, then a put_then_get is performed, else a get
  --  Validity is checked and SET is set OUT according to the final result
  subtype D_COORDINATE_LIST is SCREEN.S_INFO_LIST range SCREEN.I_X .. SCREEN.I_YMAX;
  procedure READ_COORDINATE (KIND : in D_COORDINATE_LIST;
           SET : in out BOOLEAN; COORDINATE : in out POINTS.P_T_COORDINATE;
           SUBTITLE : in BOOLEAN := FALSE);

  -- Get/update degree
  procedure READ_DEGREE;

  -- Display polynom
  procedure PUT_POLYNOM (POLYNOM : RESOL.VECTOR);

  -- Display y=f(x). Continue with another x?
  function PUT_YFX (POINT : POINTS.P_T_ONE_POINT) return BOOLEAN;

end DIALOG;

