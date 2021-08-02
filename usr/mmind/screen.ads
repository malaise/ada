with Con_Io;
with Common;
package Screen is

  -------------------
  -- GLOBAL SCREEN --
  -------------------
  Console : aliased Con_Io.Console;
  -- Global init (of colors)
  procedure Init;
  -- (Re) init the screen or just refresh
  procedure Init (Start : in Boolean; Level  : in Common.Last_Level_Range);

  -- Clear and reset
  procedure Clear;

  ------------
  -- PROPAL --
  ------------
  procedure Put_Default_Pos (
   Propal : in Common.Propal_Range;
   Level  : in Common.Level_Range;
   Show   : in Boolean);

  type Put_Try_List is (Cannot_Try, Can_Try);
  procedure Put_Try (
   Propal    : in Common.Propal_Range;
   Try_State : in Put_Try_List;
   Selected  : in Boolean);

  procedure Put_Color (
   Propal : in Common.Propal_Range;
   Level  : in Common.Level_Range;
   Color  : in Common.Color_Range);

  procedure Put_Answer (
   Propal               : in Common.Propal_Range;
   Placed_Ok, Colors_Ok : in Natural;
   Selected             : in Boolean);

  ------------
  -- SECRET --
  ------------
  procedure Put_Secret_Color (
   Level  : in Common.Level_Range;
   Color  : in Common.Color_Range);

  ----------
  -- MENU --
  ----------
  procedure Put_Start_Giveup (Start : in Boolean; Selected : in Boolean);

  procedure Put_Exit (Selected : in Boolean);

  -----------
  -- LEVEL --
  -----------
  procedure Put_Level (Level_No : in Common.Last_Level_Range;
   Selected : in Boolean);

  procedure Put_Current_Level (Level_No : in Common.Last_Level_Range);

  -----------
  -- COLOR --
  -----------
  procedure Put_Selected_Color (
   Color    : in Common.Eff_Color_Range;
   Selected : in Boolean);

  ----------
  -- HELP --
  ----------
  type Help_State is (Play, Released_Color, Released_Propal, Stopped,
                      Clicked, Invalid);
  -- Can_Clear is meaningfull when released on a proposal
  procedure Put_Help (Help : in Help_State; Can_Clear : in Boolean := False);

  ---------------
  -- SELECTION --
  ---------------
  -- kind of selection
  type Selection_List is (Nothing, Color, Propal, Try, Menu, Level, Exit_Game);


  -- Selection data
  type Selection_Rec(Selection_Kind : Selection_List := Nothing) is record
    case Selection_Kind is
      when Nothing =>
        Selection : Selection_List := Nothing;
      when Color =>
        Color_No : Common.Eff_Color_Range;
      when Propal =>
        Propal_No : Common.Propal_Range;
        Column_No : Common.Level_Range;
      when Try=>
        Try_No : Common.Propal_Range;
      when Menu | Exit_Game =>
        null;
      when Level =>
        Level_No : Common.Last_Level_Range;
    end case;
  end record;


  function Get_Selected (Where : Con_Io.Square) return Selection_Rec;

end Screen;

