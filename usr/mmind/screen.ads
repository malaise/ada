with Con_Io;
with Common;
package Screen is

  -------------------
  -- GLOBAL SCREEN --
  -------------------
  -- init the screen
  procedure Init (Level  : in Common.Last_Level_Range);

  -- clear and reset
  procedure Clear;

  ------------
  -- PROPAL --
  ------------
  procedure Put_Default_Pos (
   Propal : in Common.Propal_Range;
   Level  : in Common.Level_Range;
   Show   : in Boolean);

  type Put_Try_List is (Cannot_Try, Can_Try, Selected);
  procedure Put_Try (
   Propal    : in Common.Propal_Range;
   Try_State : in Put_Try_List);

  procedure Put_Color (
   Propal : in Common.Propal_Range;
   Level  : in Common.Level_Range;
   Color  : in Common.Color_Range);

  procedure Put_Answer (
   Propal : in Common.Propal_Range;
   Placed_Ok, Colors_Ok : in Natural);


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
  type Help_State is (Released, Click_Color, Click_Propal, Click_Other,
                      Start, Discarded);
  procedure Put_Help (Help : Help_State);


  -----------
  -- MOUSE --
  -----------
  -- default behaviour of mouse : keep foreground
  procedure Set_Mouse_Default_Color;
  -- When color selected : set foreground
  procedure Set_Mouse_Color (Color : in Common.Eff_Color_Range);


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


  procedure Get_Selected (
   Where : in Con_Io.Square;
   What  : out Selection_Rec);

end Screen;

