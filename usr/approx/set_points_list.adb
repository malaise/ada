with AFPX;
with POINTS, POINT_STR;
-- Set the AFPX_list from points
procedure SET_POINTS_LIST is
begin
  AFPX.LINE_LIST_MNG.DELETE_LIST(AFPX.LINE_LIST);
  if POINTS.P_EMPTY then
    return;
  end if;

  declare
    THE_POINTS : constant POINTS.P_T_THE_POINTS(1 .. POINTS.P_NB) := POINTS.P_THE_POINTS;
  begin
    for I in THE_POINTS'RANGE loop
      AFPX.LINE_LIST_MNG.INSERT (AFPX.LINE_LIST,
                                 POINT_STR.ENCODE_REC(THE_POINTS(I)));
    end loop;
  end;
  -- Rewind
  AFPX.LINE_LIST_MNG.MOVE_TO (AFPX.LINE_LIST, AFPX.LINE_LIST_MNG.NEXT, NUMBER => 0, FROM_CURRENT => FALSE);
  -- Go to top
  AFPX.UPDATE_LIST (AFPX.TOP);
end SET_POINTS_LIST;

