with TEXT_IO, DIRECT_IO;

with MY_IO, NORMAL, ARGUMENT;

with SOK_TYPES;
use SOK_TYPES;

procedure TRANS is

  FRAME : SOK_TYPES.FRAME_TAB;

  type FILE_FRAME_REC is record
    PATTERN : SOK_TYPES.PATTERN_LIST;
    CONTENT : SOK_TYPES.CONTENT_LIST;
  end RECORD;
  type FILE_FRAME_TAB is array (SOK_TYPES.ROW_RANGE, SOK_TYPES.COL_RANGE)
    of FILE_FRAME_REC;
  FILE_FRAME : FILE_FRAME_TAB;
  package D is new DIRECT_IO (FILE_FRAME_TAB);
  D_FILE_NAME : constant STRING := "SOKOBAN.DAT";
  D_FILE : D.FILE_TYPE;
  

  A_FILE_NAME : constant STRING := "SOKOBAN.ASC";
  A_FILE : TEXT_IO.FILE_TYPE;
  CHAR : CHARACTER;

  DAT_TO_ASC : BOOLEAN;

  -- to convert from a frame to a frame on file
  procedure FROM_FRAME_TO_FILE (FRAME : in  SOK_TYPES.FRAME_TAB;
                                FILE  : out FILE_FRAME_TAB) is
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

  -- to convert from a frame on file to a frame
  procedure FROM_FILE_TO_FRAME (FILE  : in  FILE_FRAME_TAB;
                                FRAME : out SOK_TYPES.FRAME_TAB) is
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


begin -- trans

  if ARGUMENT.GET_NBRE_ARG /= 1 then
    TEXT_IO.PUT_LINE ("Wrong arg");
    return;
  end if;
  if ARGUMENT.GET_PARAMETER = "asc2dat" then
    DAT_TO_ASC := FALSE;
  elsif ARGUMENT.GET_PARAMETER = "dat2asc" then
    DAT_TO_ASC := TRUE;
  else
    TEXT_IO.PUT_LINE ("Wrong arg");
    return;
  end if;

  if DAT_TO_ASC then
    D.OPEN (D_FILE, D.IN_FILE, D_FILE_NAME);
    begin
      TEXT_IO.OPEN (A_FILE, TEXT_IO.OUT_FILE, A_FILE_NAME);
      TEXT_IO.PUT_LINE ("File exists");
      return;
    exception
      when TEXT_IO.NAME_ERROR =>
        TEXT_IO.CREATE (A_FILE, TEXT_IO.OUT_FILE, A_FILE_NAME);
    end;



    for F in SOK_TYPES.FRAME_RANGE loop
      D.READ (D_FILE, FILE_FRAME);
      FROM_FILE_TO_FRAME (FILE_FRAME, FRAME);
      for R in SOK_TYPES.ROW_RANGE loop
        for C in SOK_TYPES.COL_RANGE loop
          case FRAME(R, C).PATTERN is
            when SOK_TYPES.WALL =>
              TEXT_IO.PUT (A_FILE, 'w');
            when FREE =>
              case FRAME(R, C).CONTENT is
                when SOK_TYPES.MAN =>
                  TEXT_IO.PUT (A_FILE, 'm');
                when SOK_TYPES.BOX =>
                  TEXT_IO.PUT (A_FILE, 'b');
                when SOK_TYPES.NOTHING =>
                  TEXT_IO.PUT (A_FILE, 'n');
              end case;
            when TARGET =>
              case FRAME(R, C).CONTENT is
                when SOK_TYPES.MAN =>
                  TEXT_IO.PUT (A_FILE, 'M');
                when SOK_TYPES.BOX =>
                  TEXT_IO.PUT (A_FILE, 'B');
                when SOK_TYPES.NOTHING =>
                  TEXT_IO.PUT (A_FILE, 'N');
              end case;
          end case;
        end loop;
      end loop;
    end loop;
  else

    TEXT_IO.OPEN (A_FILE, TEXT_IO.IN_FILE, A_FILE_NAME);
    begin
      D.OPEN (D_FILE, D.OUT_FILE, D_FILE_NAME);
      TEXT_IO.PUT_LINE ("File exists");
      return;
    exception
      when D.NAME_ERROR =>
        D.CREATE (D_FILE, D.OUT_FILE, D_FILE_NAME);
    end;

    for F in SOK_TYPES.FRAME_RANGE loop
      for R in SOK_TYPES.ROW_RANGE loop
        for C in SOK_TYPES.COL_RANGE loop
          TEXT_IO.GET (A_FILE, CHAR);
          case CHAR is
            when 'w' =>
              FRAME(R, C) := (PATTERN => SOK_TYPES.WALL);
            when 'm' =>
              FRAME(R, C) := (PATTERN => SOK_TYPES.FREE, CONTENT => SOK_TYPES.MAN);
            when 'b' =>
              FRAME(R, C) := (PATTERN => SOK_TYPES.FREE, CONTENT => SOK_TYPES.BOX);
            when 'n' =>
              FRAME(R, C) := (PATTERN => SOK_TYPES.FREE, CONTENT => SOK_TYPES.NOTHING);
            when 'M' =>
              FRAME(R, C) := (PATTERN => SOK_TYPES.TARGET, CONTENT => SOK_TYPES.MAN);
            when 'B' =>
              FRAME(R, C) := (PATTERN => SOK_TYPES.TARGET, CONTENT => SOK_TYPES.BOX);
            when 'N' =>
              FRAME(R, C) := (PATTERN => SOK_TYPES.TARGET, CONTENT => SOK_TYPES.NOTHING);
            when others =>
              TEXT_IO.PUT_LINE ("Invalid character");
          end case;
        end loop;
      end loop;
      FROM_FRAME_TO_FILE (FRAME, FILE_FRAME);
      D.WRITE (D_FILE, FILE_FRAME);
    end loop;

  end if;

  TEXT_IO.CLOSE (A_FILE);
  D.CLOSE (D_FILE);
  TEXT_IO.PUT_LINE ("Done");

end TRANS;

