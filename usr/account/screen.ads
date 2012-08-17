with Afpx;
with Oper_Def, Afpx_Xref;
-- Manage the screen fields
package Screen is
  -- In default mode,
  -- Allow oper edition buttons (edit/view, delete, clean_up, search)
  --  and Show_all button at next reset, confirm, ack
  procedure Allow_Edit (Allow : in Boolean);
  -- In subblist, some actions are disabled or modified
  procedure Set_Sublist (Active : in Boolean);
  function Is_Sublist return Boolean;

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
  Account_Name_Fld  : constant Afpx.Field_Range := Afpx_Xref.Main.Name;
  Oper_Nb_Fld       : constant Afpx.Field_Range := Afpx_Xref.Main.Nb_Oper;
  Operation_Fld     : constant Afpx.Field_Range := Afpx_Xref.Main.Operations;
  Account_Saved_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Saved;
  Nb_Selected_Fld   : constant Afpx.Field_Range := Afpx_Xref.Main.Nb_Selected;
  Selected_Fld      : constant Afpx.Field_Range := Afpx_Xref.Main.Selected;

  Real_Amnt_Fld     : constant Afpx.Field_Range := Afpx_Xref.Main.Real;
  Account_Amnt_Fld  : constant Afpx.Field_Range := Afpx_Xref.Main.Account;
  Defered_Amnt_Fld  : constant Afpx.Field_Range := Afpx_Xref.Main.Defered;
  Saved_Amnt_Fld    : constant Afpx.Field_Range := Afpx_Xref.Main.Savings;

  List_Top_Fld      : constant Afpx.Field_Range := Afpx_Xref.Main.Top;
  List_Pgup_Fld     : constant Afpx.Field_Range := Afpx_Xref.Main.Pgup;
  List_Up_Fld       : constant Afpx.Field_Range := Afpx_Xref.Main.Up;
  List_Center_Fld   : constant Afpx.Field_Range := Afpx_Xref.Main.Center;
  List_Down_Fld     : constant Afpx.Field_Range := Afpx_Xref.Main.Down;
  List_Pg_Down_Fld  : constant Afpx.Field_Range := Afpx_Xref.Main.Pgdown;
  List_Bottom_Fld   : constant Afpx.Field_Range := Afpx_Xref.Main.Bottom;

  Title_Oper_Fld    : constant Afpx.Field_Range := Afpx_Xref.Main.Title_Oper;
  Add_Oper_Fld      : constant Afpx.Field_Range := Afpx_Xref.Main.Add;
  Copy_Oper_Fld     : constant Afpx.Field_Range := Afpx_Xref.Main.Copy;
  Edit_Oper_Fld     : constant Afpx.Field_Range := Afpx_Xref.Main.Edit;
  Delete_Oper_Fld   : constant Afpx.Field_Range := Afpx_Xref.Main.Delete;
  Clean_Oper_Fld    : constant Afpx.Field_Range := Afpx_Xref.Main.Cleanup;
  Search_Oper_Fld   : constant Afpx.Field_Range := Afpx_Xref.Main.Search;
  Show_Oper_Fld     : constant Afpx.Field_Range := Afpx_Xref.Main.Showall;

  Title_Account_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Title_Account;
  New_Account_Fld   : constant Afpx.Field_Range := Afpx_Xref.Main.New_Account;
  Load_Account_Fld  : constant Afpx.Field_Range := Afpx_Xref.Main.Load;
  Save_Account_Fld  : constant Afpx.Field_Range := Afpx_Xref.Main.Save;
  Print_Account_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.Print;
  Franc_Account_Fld : constant Afpx.Field_Range := Afpx_Xref.Main.To_Francs;
  Sdat_Account_Fld  : constant Afpx.Field_Range := Afpx_Xref.Main.Sort_Date;
  Samo_Account_Fld  : constant Afpx.Field_Range := Afpx_Xref.Main.Sort_Amount;
  Exit_Account_Fld  : constant Afpx.Field_Range := Afpx_Xref.Main.Quit;

  Message_Fld       : constant Afpx.Field_Range := Afpx_Xref.Main.Message;
  Yes_Fld           : constant Afpx.Field_Range := Afpx_Xref.Main.Yes;
  No_Fld            : constant Afpx.Field_Range := Afpx_Xref.Main.No;

end Screen;

