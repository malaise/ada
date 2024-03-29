package Common is

  ----------------
  -- DIMENSIONS --
  ----------------

  -- Maximum number of propal
  type Full_Propal_Range is new Natural range 0 .. 12;
  subtype Propal_Range is Full_Propal_Range range 1 .. Full_Propal_Range'Last;
  Max_Number_Propal : constant Propal_Range := Propal_Range'Last;

  -- Minimum and maximum level
  type Full_Level_Range is new Natural range 0 .. 5;
  subtype Level_Range is Full_Level_Range range 1 .. Full_Level_Range'Last;
  subtype Last_Level_Range is Level_Range range 3 .. Level_Range'Last;
  Min_Level : constant Last_Level_Range := Last_Level_Range'First;
  Max_Level : constant Last_Level_Range := Last_Level_Range'Last;

  -- Number of available colors
  type Color_Range is new Natural range 0 .. 8;
  Max_Number_Color : constant Color_Range := Color_Range'Last;
  subtype Eff_Color_Range is Color_Range range 1 .. Max_Number_Color;
  No_Color : constant Color_Range := Color_Range'First;

  -- Level of the game
  --  Store the one selected (may not be the one of current propal)
  procedure Store_Level (Level : in Last_Level_Range);
  --  Set propal to level stored
  procedure Set_Level_To_Stored;
  --  Current level stored
  function  Get_Stored_Level return Last_Level_Range;
  --  Current level set
  function  Get_Level return Last_Level_Range;

  -- Try state of a propal
  type Try_List is (Not_Set, Can_Try, Answered);

  -- State of a propal
  type Propal_Color_Array is array (Level_Range range <>) of Color_Range;
  type Propal_State_Rec (Level : Last_Level_Range := Min_Level) is record
    Propal_Color : Propal_Color_Array(1 .. Level) := (others => No_Color);
    Try          : Try_List := Not_Set;
  end record;

  function Get_Propal_State (Propal : Propal_Range) return Propal_State_Rec;
  procedure Set_Propal_State (
   Propal : in Propal_Range;
   State  : in Propal_State_Rec);

  procedure Set_Color (
   Propal : in Propal_Range;
   Level  : in Level_Range;
   Color  : in Color_Range);

  -- State of try area
  procedure Set_Try_State (
   Propal : in Propal_Range;
   Try    : in Try_List);

  -- Answer value
  procedure Set_Answer (
   Propal : in Propal_Range;
   Placed_Ok, Colors_Ok : in Natural);

  function Is_Answered (Propal : Propal_Range) return Boolean;

  procedure Get_Answer (
   Propal : in Propal_Range;
   Placed_Ok, Colors_Ok : out Natural);

  -- Reset all the states
  procedure Reset_State;

  -- Can the player select a try, a proposal to clear/copy
  procedure Possible_Selections (Can_Try, Can_Copy : out Boolean);

end Common;

