with SOK_MOVEMENT;

package DUMP_SAVE is
  procedure DUMP (MVT : in SOK_MOVEMENT.SAVED_DATA_REC; PUSHED : in BOOLEAN);
end DUMP_SAVE;

with SOK_TYPES;
with TEXT_IO; use TEXT_IO;
package body DUMP_SAVE is
  FILE : FILE_TYPE;
  OPEN : BOOLEAN := FALSE;

  procedure DUMP (MVT : in SOK_MOVEMENT.SAVED_DATA_REC; PUSHED : in BOOLEAN) is
  begin
    if not OPEN then
      CREATE (FILE, OUT_FILE, "dump");
      OPEN := TRUE;
    end if;

    if PUSHED then
      PUT_LINE ("PUSHED");
    else
      PUT_LINE ("POPED");
    end if;

    PUT_LINE ("POS_ORIG " &
     SOK_TYPES.ROW_RANGE'IMAGE(MVT.POS_ORIG.ROW) & " " &
     SOK_TYPES.COL_RANGE'IMAGE(MVT.POS_ORIG.COL) );

    PUT_LINE ("MOVEMENT " & SOK_MOVEMENT.MOVEMENT_LIST'IMAGE (MVT.MOVEMENT) );

    PUT_LINE ("RESULT " & SOK_MOVEMENT.SAVED_RESULT_LIST'IMAGE (MVT.RESULT) );

    PUT_LINE ("---------------------");
  end DUMP;

end DUMP_SAVE;

