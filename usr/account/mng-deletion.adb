separate (MNG)
package body DELETION is

  -- Number of deleted operations
  NB_DELETED : NATURAL := 0;

  -- Flag currently selected operation as deleted
  procedure FLAG_DELETED is
    SEL : SEL_REC;
  begin
    -- Flag selected operation as deleted
    SEL_LIST_MNG.READ(SEL_LIST, SEL, SEL_LIST_MNG.CURRENT);
    if not SEL.DELETED then
      SEL.DELETED := TRUE;
      SEL_LIST_MNG.MODIFY(SEL_LIST, SEL, SEL_LIST_MNG.CURRENT);
      NB_DELETED := NB_DELETED + 1;
    end if;
  end FLAG_DELETED;

  -- Flag currently selected operation as not deleted
  procedure FLAG_UNDELETED is
    SEL : SEL_REC;
  begin
    -- Flag selected operation as deleted
    SEL_LIST_MNG.READ(SEL_LIST, SEL, SEL_LIST_MNG.CURRENT);
    if SEL.DELETED then
      SEL.DELETED := FALSE;
      SEL_LIST_MNG.MODIFY(SEL_LIST, SEL, SEL_LIST_MNG.CURRENT);
      NB_DELETED := NB_DELETED - 1;
    end if;
  end FLAG_UNDELETED;

  -- Get number of flagged operations
  function GET_NB_DELETED return OPER_NB_RANGE is
  begin
    return NB_DELETED;
  end GET_NB_DELETED;

  -- Delete all flagged operation
  procedure COMMIT_DELETIONS is
    SEL : SEL_REC;
  begin
    if NB_DELETED = 0 then
      return;
    end if;

    -- Commit deletions from last to first (so REFs remain correct)
    SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.PREV, 0 , FALSE);
    loop
      SEL_LIST_MNG.READ(SEL_LIST, SEL, SEL_LIST_MNG.CURRENT);
      if SEL.DELETED then
        -- Remove current operation
        LIST_UTIL.MOVE_TO_CURRENT;
        if SEL.NO /= 1 then
          OPER_LIST_MNG.DELETE(OPER_LIST, OPER_LIST_MNG.PREV);
        else
          OPER_LIST_MNG.DELETE(OPER_LIST, OPER_LIST_MNG.NEXT);
        end if;
        -- Remove current selection
        if SEL_LIST_MNG.GET_POSITION(SEL_LIST) /= 1 then
          SEL_LIST_MNG.DELETE(SEL_LIST, SEL_LIST_MNG.PREV);
        else
          SEL_LIST_MNG.DELETE(SEL_LIST, SEL_LIST_MNG.NEXT);
        end if;
        -- Update counter
        NB_DELETED := NB_DELETED - 1;
        -- Either first item of selection is deleted here
        --  or previous selection records are unchanged
        exit when NB_DELETED = 0;
      else
        -- Update selection with accurate oper no
        SEL.NO := SEL.NO - NB_DELETED;
        SEL_LIST_MNG.MODIFY(SEL_LIST, SEL, SEL_LIST_MNG.CURRENT);
        -- Move to previous selection
        SEL_LIST_MNG.MOVE_TO(SEL_LIST, SEL_LIST_MNG.PREV);
      end if;
    end loop;
  end COMMIT_DELETIONS;

end DELETION;

