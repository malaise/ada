with DIRECT_IO;
with SEQUENTIAL_IO;
with NORMAL;

separate (SOK_MANAGER)
-- Sokoban frames reading.
package body SOK_FILE is

  -- frame as it is on disk : no mutant
  type FILE_SQUARE_REC is record
    PATTERN : SOK_TYPES.PATTERN_LIST;
    CONTENT : SOK_TYPES.CONTENT_LIST;
  end RECORD;
  type FILE_FRAME_TAB is array (SOK_TYPES.ROW_RANGE, SOK_TYPES.COL_RANGE)
   of FILE_SQUARE_REC;

  -- internal state of a frame
  type FILE_STATE_REC is record
    DUR          : CALENDAR.DAY_DURATION;
    DAY          : NATURAL;
    FRAME        : FILE_FRAME_TAB;
    NO_FRAME     : SOK_TYPES.FRAME_RANGE;
    POSITION     : SOK_TYPES.COORDINATE_REC;
    NBRE_TARGETS : NATURAL;
    BOX_OK       : NATURAL;
    MOVES        : NATURAL;
    PUSHES       : NATURAL;
  end record;

  -- for read frame
  package SOK_FILE_MNG is new DIRECT_IO (FILE_FRAME_TAB);


  -- for save and restore sate
  package SOK_STATE_MNG is new SEQUENTIAL_IO (FILE_STATE_REC);
  -- for save and restore saved movements
  package SOK_SAVED_MNG is new SEQUENTIAL_IO (SOK_MOVEMENT.SAVED_DATA_REC);

  -- for save and restore scores
  package SOK_SCORE_MNG is new DIRECT_IO (SOK_TYPES.SCORE_REC);

  -- for read frame
  SOK_FILE_NAME : constant STRING := "SOKOBAN.DAT";

  -- for save and restore frame and movements
  SOK_STATE_NAME : constant STRING := "STATE.DAT";
  SOK_SAVED_NAME : constant STRING := "SAVED.DAT";

  -- for scores
  SOK_SCORE_NAME : constant STRING := "SCORES.DAT";

  -- to convert from a frame on file to a frame
  procedure FROM_FILE_TO_FRAME (FILE  : in  FILE_FRAME_TAB;
                                FRAME : out SOK_TYPES.FRAME_TAB) is
    use SOK_TYPES;
  begin
    for I in SOK_TYPES.ROW_RANGE loop
      for J in SOK_TYPES.COL_RANGE loop
        case FILE(I,J).PATTERN is
          when SOK_TYPES.WALL =>
            FRAME(I,J) := (PATTERN => SOK_TYPES.WALL);
          when SOK_TYPES.FREE =>
            FRAME(I,J) := (PATTERN => SOK_TYPES.FREE,
                           CONTENT => FILE(I, J).CONTENT);
          when SOK_TYPES.TARGET =>
            FRAME(I,J) := (PATTERN => SOK_TYPES.TARGET,
                           CONTENT => FILE(I, J).CONTENT);

        end case;
      end loop;
    end loop;
  end FROM_FILE_TO_FRAME;

  -- to convert from a frame to a frame on file
  procedure FROM_FRAME_TO_FILE (FRAME : in  SOK_TYPES.FRAME_TAB;
                                FILE  : out FILE_FRAME_TAB) is
    use SOK_TYPES;
  begin
    for I in SOK_TYPES.ROW_RANGE loop
      for J in SOK_TYPES.COL_RANGE loop
        case FRAME(I,J).PATTERN is
          when SOK_TYPES.WALL =>
            FILE(I,J) := (PATTERN => SOK_TYPES.WALL,
                          CONTENT => SOK_TYPES.NOTHING);
          when SOK_TYPES.FREE =>
            FILE(I,J) := (PATTERN => SOK_TYPES.FREE,
                          CONTENT => FRAME(I,J).CONTENT);
          when SOK_TYPES.TARGET =>
            FILE(I,J) := (PATTERN => SOK_TYPES.TARGET,
                          CONTENT => FRAME(I,J).CONTENT);

        end case;
      end loop;
    end loop;
  end FROM_FRAME_TO_FILE;


  -- to read a new frame
  procedure READ (NO_FRAME : in  SOK_TYPES.FRAME_RANGE;
                  FRAME    : out SOK_TYPES.FRAME_TAB) is
    SOK_FILE : SOK_FILE_MNG.FILE_TYPE;
    FILE_FRAME : FILE_FRAME_TAB;
  begin
    begin
      SOK_FILE_MNG.OPEN (SOK_FILE, SOK_FILE_MNG.IN_FILE, SOK_FILE_NAME);
    exception
      when SOK_FILE_MNG.NAME_ERROR =>
        raise DATA_FILE_NOT_FOUND;
    end;

    begin
      SOK_FILE_MNG.READ (SOK_FILE, FILE_FRAME, SOK_FILE_MNG.COUNT(NO_FRAME));
      FROM_FILE_TO_FRAME (FILE_FRAME, FRAME);
    exception
      when others =>
        begin
          SOK_FILE_MNG.CLOSE (SOK_FILE);
        exception
          when others => null;
        end;
        raise ERROR_READING_DATA;
    end;

    begin
      SOK_FILE_MNG.CLOSE (SOK_FILE);
    exception
      when others => null;
    end;
  end READ;

  -- closes frame and saved file
  procedure CLOSE_FILES (
   SOK_STATE_FILE : in out SOK_STATE_MNG.FILE_TYPE;
   SOk_SAVED_FILE : in out SOK_SAVED_MNG.FILE_TYPE) is
  begin
    begin
      -- close state file
      SOK_STATE_MNG.CLOSE (SOK_STATE_FILE);
    exception
      when others => null;
    end;

    -- close saved file
    begin
      SOK_SAVED_MNG.CLOSE (SOK_SAVED_FILE);
    exception
      when others => null;
    end;
  end CLOSE_FILES;

  --save a frame with saved movements
  procedure SAVE (STATE : in STATE_REC) is
    SOK_STATE_FILE : SOK_STATE_MNG.FILE_TYPE;
    SOK_SAVED_FILE : SOK_SAVED_MNG.FILE_TYPE;
    FILE_STATE : FILE_STATE_REC;
  begin
    -- be sure that there is no file
    begin
      SOK_STATE_MNG.OPEN (SOK_STATE_FILE, SOK_STATE_MNG.IN_FILE,
       SOK_STATE_NAME);
      SOK_STATE_MNG.DELETE (SOK_STATE_FILE);
    exception
      when SOK_STATE_MNG.NAME_ERROR => null;
    end;
    begin
      SOK_SAVED_MNG.OPEN (SOK_SAVED_FILE, SOK_SAVED_MNG.IN_FILE,
       SOK_SAVED_NAME);
      SOK_SAVED_MNG.DELETE (SOK_SAVED_FILE);
    exception
      when SOK_SAVED_MNG.NAME_ERROR => null;
    end;

    -- now create new files
    SOK_STATE_MNG.CREATE (SOK_STATE_FILE, SOK_STATE_MNG.OUT_FILE,
     SOK_STATE_NAME);
    SOK_SAVED_MNG.CREATE (SOK_SAVED_FILE, SOK_SAVED_MNG.OUT_FILE,
     SOK_SAVED_NAME);

    -- fill state to be saved
    SOK_TIME.GET_TIME (FILE_STATE.DAY, FILE_STATE.DUR);
    FROM_FRAME_TO_FILE (STATE.FRAME, FILE_STATE.FRAME);
    FILE_STATE.NO_FRAME     := STATE.NO_FRAME;
    FILE_STATE.POSITION     := STATE.POSITION;
    FILE_STATE.NBRE_TARGETS := STATE.NBRE_TARGETS;
    FILE_STATE.BOX_OK       := STATE.BOX_OK;
    FILE_STATE.MOVES        := STATE.MOVES;
    FILE_STATE.PUSHES       := STATE.PUSHES;

    -- save state
    SOK_STATE_MNG.WRITE (SOK_STATE_FILE, FILE_STATE);

    -- save saved movements
    begin
      SOK_SAVED_MNG.WRITE (SOK_SAVED_FILE, SOK_SAVE.LOOK (SOK_SAVE.FIRST));
      loop
        SOK_SAVED_MNG.WRITE (SOK_SAVED_FILE, SOK_SAVE.LOOK (SOK_SAVE.NEXT));
      end loop;
    exception
      when SOK_SAVE.NO_MORE_SAVED_MOVEMENTS => null;
    end;

    CLOSE_FILES (SOK_STATE_FILE, SOK_SAVED_FILE);
  exception
    when others =>
      CLOSE_FILES (SOK_STATE_FILE, SOK_SAVED_FILE);
      raise ERROR_WRITING_FRAME;
  end SAVE;

  --restore a frame without saved movements
  procedure RESTORE (STATE : out STATE_REC) is
    SOK_STATE_FILE : SOK_STATE_MNG.FILE_TYPE;
    SOK_SAVED_FILE : SOK_SAVED_MNG.FILE_TYPE;
    FILE_STATE : FILE_STATE_REC;
  begin
    begin
      SOK_STATE_MNG.OPEN (SOK_STATE_FILE, SOK_STATE_MNG.IN_FILE,
       SOK_STATE_NAME);
      SOK_SAVED_MNG.OPEN (SOK_SAVED_FILE, SOK_SAVED_MNG.IN_FILE,
       SOK_SAVED_NAME);
    exception
      when -- SOK_STATE_MNG.NAME_ERROR |
           SOK_SAVED_MNG.NAME_ERROR =>
        raise FRAME_FILE_NOT_FOUND;
    end;

    -- read state
    SOK_STATE_MNG.READ (SOK_STATE_FILE, FILE_STATE);

    -- fill returned state
    SOK_TIME.SET_TIME (FILE_STATE.DAY, FILE_STATE.DUR);
    FROM_FILE_TO_FRAME (FILE_STATE.FRAME, STATE.FRAME);
    STATE.NO_FRAME     := FILE_STATE.NO_FRAME;
    STATE.POSITION     := FILE_STATE.POSITION;
    STATE.NBRE_TARGETS := FILE_STATE.NBRE_TARGETS;
    STATE.BOX_OK       := FILE_STATE.BOX_OK;
    STATE.MOVES        := FILE_STATE.MOVES;
    STATE.PUSHES       := FILE_STATE.PUSHES;

    -- read saved movements
    SOK_SAVE.RESET;
    loop
      declare
        SAVED_MOVEMENT : SOK_MOVEMENT.SAVED_DATA_REC;
      begin
        SOK_SAVED_MNG.READ (SOK_SAVED_FILE, SAVED_MOVEMENT);
        SOK_SAVE.PUSH (SAVED_MOVEMENT);
      exception
        when SOK_SAVED_MNG.END_ERROR => exit;
      end;
    end loop;
    CLOSE_FILES (SOK_STATE_FILE, SOK_SAVED_FILE);
  exception
    when FRAME_FILE_NOT_FOUND =>
      CLOSE_FILES (SOK_STATE_FILE, SOK_SAVED_FILE);
      raise;
    when others =>
      CLOSE_FILES (SOK_STATE_FILE, SOK_SAVED_FILE);
      raise ERROR_READING_FRAME;
  end RESTORE;


  procedure INIT_SCORES is
    SOK_SCORE_FILE : SOK_SCORE_MNG.FILE_TYPE;
    SCORE : SOK_TYPES.SCORE_REC;
  begin
    SCORE.SET := FALSE;
    begin
      SOK_SCORE_MNG.OPEN (SOK_SCORE_FILE, SOK_SCORE_MNG.INOUT_FILE,
       SOK_SCORE_NAME);
      SOK_SCORE_MNG.CLOSE (SOK_SCORE_FILE);
    exception
      when SOK_SCORE_MNG.NAME_ERROR =>
        -- Create default score file
        SOK_SCORE_MNG.CREATE (SOK_SCORE_FILE, SOK_SCORE_MNG.INOUT_FILE,
             SOK_SCORE_NAME);
        for I in SOK_TYPES.FRAME_RANGE loop
          SOK_SCORE_MNG.WRITE (SOK_SCORE_FILE, SCORE);
        end loop;
        SOK_SCORE_MNG.CLOSE (SOK_SCORE_FILE);
    end;
  exception
    when others =>      
      raise SCORE_IO_ERROR;
  end INIT_SCORES;

   
  function READ_SCORE (NO : SOK_TYPES.FRAME_RANGE) return SOK_TYPES.SCORE_REC is
    SOK_SCORE_FILE : SOK_SCORE_MNG.FILE_TYPE;
    SCORE : SOK_TYPES.SCORE_REC;
  begin
    SOK_SCORE_MNG.OPEN (SOK_SCORE_FILE, SOK_SCORE_MNG.IN_FILE,
     SOK_SCORE_NAME);
    SOK_SCORE_MNG.READ (SOK_SCORE_FILE, SCORE,
      SOK_SCORE_MNG.POSITIVE_COUNT(NO)); 
    SOK_SCORE_MNG.CLOSE (SOK_SCORE_FILE);
    return SCORE;
  exception
    when others =>      
      raise SCORE_IO_ERROR;
  end READ_SCORE;

  procedure WRITE_SCORE (NO : in SOK_TYPES.FRAME_RANGE;
                         SCORE : in SOK_TYPES.SCORE_REC) is
    SOK_SCORE_FILE : SOK_SCORE_MNG.FILE_TYPE;
  begin
    SOK_SCORE_MNG.OPEN (SOK_SCORE_FILE, SOK_SCORE_MNG.OUT_FILE,
     SOK_SCORE_NAME);
    SOK_SCORE_MNG.WRITE (SOK_SCORE_FILE, SCORE,
      SOK_SCORE_MNG.POSITIVE_COUNT(NO)); 
    SOK_SCORE_MNG.CLOSE (SOK_SCORE_FILE);
  exception
    when others =>      
      raise SCORE_IO_ERROR;
  end WRITE_SCORE;

end SOK_FILE;

