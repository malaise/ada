with My_Math, Sorts;
package body Points is

  subtype T_Range is Positive range 1 .. Max_Number;

  -- Points storage
  type T_Storage is
    record
      Number     : Natural range 0 .. Max_Number := 0;
      The_Points : P_T_The_Points(T_Range);
      Saved      : Boolean := True;
    end record;
  Storage : T_Storage;

  function Lt (Pl, Pr : in P_T_One_Point) return Boolean is
    use My_Math;
  begin
    return Pl.X < Pr.X or else
         (Pl.X = Pr.X and then Pl.Y < Pr.Y);
  end Lt;

  package Points_Sort is new Sorts (
    Typ_Object => P_T_One_Point,
    Typ_Index  => Positive,
    "<"        => Lt,
    Typ_Array  => P_T_The_Points);

  function P_The_Points return P_T_The_Points is
  begin
    return Storage.The_Points(1 .. Storage.Number);
  end P_The_Points;

  function P_One_Point (Index : in Positive) return P_T_One_Point is
  begin
    if Index < 1 or else Index > Storage.Number then
      raise P_Index_Out;
    end if;
    return Storage.The_Points(Index);
  end P_One_Point;

  -- Store points
  procedure P_Store(The_Points : in P_T_The_Points) is
  begin
    -- Raise constraint error if the number of points is too big
    Storage.Number := The_Points'Length;
    if Storage.Number > 0 then
      Storage.The_Points(1 .. Storage.Number) :=
       The_Points(The_Points'Range);
    end if;
    Storage.Saved := True;
  exception
    when Constraint_Error =>
      raise P_Too_Many;
  end P_Store;

  -- Clear storage
  procedure P_Clear is
  begin
    Storage.Number := 0;
    Storage.Saved := True;
  end P_Clear;

  -- Sort
  procedure P_Sort is
  begin
    Points_Sort.Quick_Sort(Storage.The_Points(1 .. Storage.Number));
  end P_Sort;


  -- Take a point update into account
  procedure P_Upd_Point(Action : in P_T_Upd_Action;
                        Index  : in Positive := 1;
                        Point  : in P_T_One_Point := (X => 0.0, Y => 0.0)) is
  begin
    case Action is
      when Add =>
        -- Append new point
        begin
          Storage.Number := Storage.Number + 1;
        exception
          when Constraint_Error =>
            raise P_Too_Many;
        end;
        Storage.The_Points(Storage.Number) := Point;
      when Remove =>
        if Index < 1 or else Index > Storage.Number then
          raise P_Index_Out;
        end if;
        -- Shift points
        Storage.Number := Storage.Number - 1;
        Storage.The_Points(Index .. Storage.Number) :=
          Storage.The_Points(Index + 1 .. Storage.Number + 1);
      when Modify =>
        -- Store new content
        if Index < 1 or else Index > Storage.Number then
          raise P_Index_Out;
        end if;
        Storage.The_Points(Index) := Point;
    end case;
    -- The new set is not saved
    Storage.Saved := False;
  end P_Upd_Point;

  procedure P_Saved is
  begin
    Storage.Saved := True;
  end P_Saved;

  function P_Saved return Boolean is
  begin
    return Storage.Saved;
  end P_Saved;

  function P_Empty return Boolean is
  begin
    return Storage.Number = 0;
  end P_Empty;

  function P_Nb return Natural is
  begin
    return Storage.Number;
  end P_Nb;

end Points;
