with Afpx;
with Oper_Def, Unit_Format;
-- Manage the screen fields
package Screen is

  -- Allow oper edition buttons (edit/view, delete, clean_up, search)
  --  and Show_all button at next reset, confirm, ack
  -- False by default
  procedure Allow_Edit (Allow : in Boolean);
  procedure Sublist (Active : in Boolean);

  -- Set to default mode
  procedure Reset;

  -- Encode header fields
  procedure Encode_File_Name (File_Name : in String);
  procedure Encode_Nb_Oper (Oper : in Natural; Selected : in Natural);
  procedure Encode_Saved (Saved : in Boolean);

  -- Set the "To Francs/Euros" button according to current unit
  procedure Update_To_Unit;

  -- Encore summary
  type Amount_List is (Real, Account, Defered, Saved);
  type Amounts_Array is array (Amount_List) of Oper_Def.Amount_Rec;
  procedure Encode_Summary(Amounts : in Amounts_Array);

  -- Confirm
  type Action_List is (Overwrite_Account, Overwrite_File, Quit_Unsaved);
  function Confirm_Action (Action : Action_List) return Boolean;

  -- Ack an error
  type Error_List is (File_Access, File_Io, File_Read_Only, File_Name_Too_Long,
                      Account_Full, Not_Implemented, Internal_Error,
                      Capacity_Error);
  procedure Ack_Error (Error : in Error_List);

  -- Ring alarm / question bell
  procedure Ring (Alarm : in Boolean);

  -- Significant fields
  Account_Name_Fld  : constant Afpx.Field_Range :=  1;
  Oper_Nb_Fld       : constant Afpx.Field_Range :=  3;
  Operation_Fld     : constant Afpx.Field_Range :=  4;
  Account_Saved_Fld : constant Afpx.Field_Range :=  5;
  Nb_Selected_Fld   : constant Afpx.Field_Range :=  6;
  Selected_Fld      : constant Afpx.Field_Range :=  7;

  Real_Amnt_Fld     : constant Afpx.Field_Range := 10;
  Account_Amnt_Fld  : constant Afpx.Field_Range := 12;
  Defered_Amnt_Fld  : constant Afpx.Field_Range := 14;
  Saved_Amnt_Fld    : constant Afpx.Field_Range := 16;

  List_Top_Fld      : constant Afpx.Field_Range := 17;
  List_Pgup_Fld     : constant Afpx.Field_Range := 18;
  List_Up_Fld       : constant Afpx.Field_Range := 19;
  List_Center_Fld   : constant Afpx.Field_Range := 20;
  List_Down_Fld     : constant Afpx.Field_Range := 21;
  List_Pg_Down_Fld  : constant Afpx.Field_Range := 22;
  List_Bottom_Fld   : constant Afpx.Field_Range := 23;

  Title_Oper_Fld    : constant Afpx.Field_Range := 24;
  Add_Oper_Fld      : constant Afpx.Field_Range := 25;
  Copy_Oper_Fld     : constant Afpx.Field_Range := 26;
  Edit_Oper_Fld     : constant Afpx.Field_Range := 27;
  Delete_Oper_Fld   : constant Afpx.Field_Range := 28;
  Clean_Oper_Fld    : constant Afpx.Field_Range := 29;
  Search_Oper_Fld   : constant Afpx.Field_Range := 30;
  Show_Oper_Fld     : constant Afpx.Field_Range := 31;

  Title_Account_Fld : constant Afpx.Field_Range := 32;
  New_Account_Fld   : constant Afpx.Field_Range := 33;
  Load_Account_Fld  : constant Afpx.Field_Range := 34;
  Save_Account_Fld  : constant Afpx.Field_Range := 35;
  Print_Account_Fld : constant Afpx.Field_Range := 36;
  Franc_Account_Fld : constant Afpx.Field_Range := 37;
  Sdat_Account_Fld  : constant Afpx.Field_Range := 38;
  Samo_Account_Fld  : constant Afpx.Field_Range := 39;
  Exit_Account_Fld  : constant Afpx.Field_Range := 40;

  Message_Fld       : constant Afpx.Field_Range := 41;
  Yes_Fld           : constant Afpx.Field_Range := 42;
  No_Fld            : constant Afpx.Field_Range := 43;

end Screen;

