with Afpx;
with Points, Point_Str;
-- Set the Afpx_List from points
procedure Set_Points_List is
begin
  Afpx.Line_List_Mng.Delete_List(Afpx.Line_List);
  if Points.P_Empty then
    return;
  end if;

  declare
    The_Points : constant Points.P_T_The_Points(1 .. Points.P_Nb)
               := Points.P_The_Points;
  begin
    for I in The_Points'Range loop
      Afpx.Line_List_Mng.Insert (Afpx.Line_List,
                                 Point_Str.Encode_Rec(The_Points(I)));
    end loop;
  end;
  -- Rewind
  Afpx.Line_List_Mng.Rewind (Afpx.Line_List);
  -- Go to top
  Afpx.Update_List (Afpx.Top);
end Set_Points_List;

