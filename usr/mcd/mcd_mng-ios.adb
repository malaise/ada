with TEXT_IO;
with INTE_IO, REAL_IO, BOOL_IO;
separate (MCD_MNG)

package body IOS is 

  procedure PUT (ITEM : in ITEM_REC) is
  begin
    case ITEM.KIND is
      when INTE =>
        INTE_IO.PUT(ITEM.VAL_INTE);
      when REAL =>
        REAL_IO.PUT(ITEM.VAL_REAL);
      when BOOL  =>
        BOOL_IO.PUT(ITEM.VAL_BOOL);
      when CHRS =>
        if ITEM.VAL_TEXT(1) = '"' then
          TEXT_IO.PUT (ITEM.VAL_TEXT(2 .. ITEM.VAL_LEN - 1));
        else
          TEXT_IO.PUT (ITEM.VAL_TEXT(1 .. ITEM.VAL_LEN));
        end if;
      when others =>
        raise INVALID_ARGUMENT;
    end case;
  end PUT;
    
  procedure PUT_LINE (ITEM : in ITEM_REC) is
  begin
    PUT(ITEM);
    NEW_LINE;
  end PUT_LINE;

  procedure NEW_LINE is
  begin
    TEXT_IO.NEW_LINE;
  end NEW_LINE;

end IOS;

