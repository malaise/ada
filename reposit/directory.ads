with SYSTEM;
with CALENDAR;
with TEXT_HANDLER;
package DIRECTORY is

  MAX_DIR_NAME_LEN : constant := 1024;

  -- Returns current working directory
  function GET_CURRENT return STRING;
  procedure GET_CURRENT (CUR_DIR : in out TEXT_HANDLER.TEXT);

  -- Changes current working directory
  procedure CHANGE_CURRENT (NEW_DIR : in STRING);
  -- May raise NAME_ERROR


  type DIR_DESC is private;

  -- Opens a directory for list of entries
  function OPEN (DIR_NAME : in STRING) return DIR_DESC;
  -- May raise OPEN_ERROR if dir desc is already open
  -- May raise NAME_ERROR if not found
  -- May raise ACCESS_ERROR 

  -- Gets next entry of the opened directory
  function NEXT_ENTRY (DESC : DIR_DESC) return STRING;
  procedure NEXT_ENTRY (DESC : in DIR_DESC; DIR_ENTRY : in out TEXT_HANDLER.TEXT);
  -- May raise OPEN_ERROR if dir desc is not open
  -- Will raise END_ERROR if no more entry

  -- Reset entries for the first 
  procedure REWIND (DESC : in DIR_DESC);
  -- May raise OPEN_ERROR if dir desc is not open

  -- Closes a directory
  procedure CLOSE (DESC : in out DIR_DESC);
  -- May raise OPEN_ERROR if dir desc is not open

  type FILE_KIND_LIST is (FILE, DIR, LINK,
           BLOCK_DEVICE, CHARACTER_DEVICE, PIPE, SOCKET, UNKNOWN);
  type TIME_T is private;
  -- RIGHTS are :
  --  1st bit OX
  --  2nd bit OW
  --  3rd bit OR
  --  4th bit GX
  --  5th bit GW
  --  6th bit GR
  --  7th bit UX
  --  8th bit UW
  --  9th bit UR
  -- 10th bit ST (sticky)
  -- 11th bit GS (set GID)
  -- 12th bit US (set UID))
  procedure FILE_STAT (FILE_NAME : in STRING;
                       KIND       : out FILE_KIND_LIST;
                       RIGHTS     : out NATURAL;
                       MODIF_TIME : out TIME_T);
  -- May raise NAME_ERROR or ACCESS_ERROR

  function TIME_OF (TIME : TIME_T) return CALENDAR.TIME;
  
  function READ_LINK (FILE_NAME : STRING; RECURSIVE : BOOLEAN := TRUE)
                      return STRING;
  procedure READ_LINK (FILE_NAME : in STRING;
                       TARGET : in out TEXT_HANDLER.TEXT;
                       RECURSIVE : in BOOLEAN := TRUE);
  -- May raise NAME_ERROR if FILE_NAME does not exist
  --           ACCESS_ERROR if FILE_NAME cannot be read
  --           OPEN_ERROR if FILE_NAME is not a link

  -- Does file name match a pattern
  function FILE_MATCH (FILE_NAME : STRING; TEMPLATE : STRING) return BOOLEAN;

  NAME_ERROR   : exception;
  OPEN_ERROR   : exception;
  ACCESS_ERROR : exception;
  END_ERROR    : exception;

  
private

  type DIR_DESC is record
    DIR_ADDR : SYSTEM.ADDRESS := SYSTEM.NULL_ADDRESS;
  end record;

  type TIME_T is new INTEGER;
end DIRECTORY;


