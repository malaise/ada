package body Common is

  type Fix_Propal_State_Rec is record
    Propal_Color : Propal_Color_Array(1 .. Max_Level) := (others => 0);
    Try          : Try_List := Not_Set;
    Placed_Ok    : Natural := 0;
    Colors_Ok    : Natural := 0;
  end record;
  Init_Fix_Propal_State : constant Fix_Propal_State_Rec := (
    Propal_Color => (others => 0),
    Try          => Not_Set,
    Placed_Ok    => 0,
    Colors_Ok    => 0);

  State_Level_Stored : Last_Level_Range;
  State_Level : Last_Level_Range;
  Level_Stored : Boolean := False;
  Level_Set   : Boolean := False;
  State_Data  : array (Propal_Range) of Fix_Propal_State_Rec;

  -- Level of the game
  --  Store the one selected (may not be the one of current propal)
  procedure Store_Level (Level : in Last_Level_Range) is
  begin
    State_Level_Stored := Level;
    Level_Stored := True;
  end Store_Level;

  --  Set propal to level stored
  procedure Set_Level_To_Stored is
  begin
    if not Level_Stored then raise Constraint_Error; end if;
    State_Level := State_Level_Stored;
    Level_Set := True;
    Reset_State;
  end Set_Level_To_Stored;

  procedure Check_Level is
  begin
    if not Level_Set then raise Constraint_Error; end if;
  end Check_Level;

  function Get_Stored_Level return Last_Level_Range is
  begin
    if not Level_Stored then raise Constraint_Error; end if;
    return State_Level_Stored;
  end Get_Stored_Level;

  function Get_Level return Last_Level_Range is
  begin
    Check_Level;
    return State_Level;
  end Get_Level;

  function Get_Propal_State (Propal : Propal_Range) return Propal_State_Rec is
  begin
    Check_Level;
    return (Level        => State_Level,
            Propal_Color => State_Data(Propal).Propal_Color(1 .. State_Level),
            Try          => State_Data(Propal).Try );
  end Get_Propal_State;

  procedure Set_Propal_State (
   Propal : in Propal_Range;
   State  : in Propal_State_Rec) is
  begin
    Check_Level;
    if State.Level /= State_Level then raise Constraint_Error; end if;
    State_Data(Propal).Propal_Color(1 .. State_Level) := State.Propal_Color;
    State_Data(Propal).Try := State.Try;
  end Set_Propal_State;

  procedure Set_Color (
   Propal : in Propal_Range;
   Level  : in Level_Range;
   Color  : in Color_Range) is
  begin
    Check_Level;
    State_Data(Propal).Propal_Color(Level) := Color;
  end Set_Color;

  procedure Set_Try_State (
   Propal : in Propal_Range;
   Try    : in Try_List) is
  begin
    Check_Level;
    State_Data(Propal).Try := Try;
  end Set_Try_State;

  procedure Set_Answer (
   Propal : in Propal_Range;
   Placed_Ok, Colors_Ok : in Natural) is
  begin
    Check_Level;
    if State_Data(Propal).Try /= Answered then
      raise Constraint_Error;
    end if;
    State_Data(Propal).Placed_Ok := Placed_Ok;
    State_Data(Propal).Colors_Ok := Colors_Ok;
  end Set_Answer;

  procedure Get_Answer (
   Propal : in Propal_Range;
   Placed_Ok, Colors_Ok : out Natural) is
  begin
    Check_Level;
    if State_Data(Propal).Try /= Answered then
      raise Constraint_Error;
    end if;
    Placed_Ok := State_Data(Propal).Placed_Ok;
    Colors_Ok := State_Data(Propal).Colors_Ok;
  end Get_Answer;

  procedure Possible_Selections (Can_Try, Can_Propose : out Boolean) is
  begin
    Can_Try:= False;
    Can_Propose := False;
    for I in Propal_Range loop
      if State_Data(I).Try /= Answered then
        if not Can_Try then
          for J in 1 .. State_Level loop
            if State_Data(I).Propal_Color(J) /= 0 then
              -- This proposed color can me changed
              Can_Propose := True;
              exit;
            end if;
          end loop;
        end if;
        if State_Data(I).Try = Common.Can_Try then
          Can_Try := True;
        end if;
        exit when Can_Propose and then Can_Try;
      end if;
    end loop;
  end Possible_Selections;

  procedure Reset_State is
  begin
    Check_Level;
    State_Data := (others => Init_Fix_Propal_State);
  end Reset_State;

end Common;

