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

  -- Set the "TO FRANCS/EUROS" button according to current unit
  procedure Update_To_Unit;

  -- Encore summary
  type Amount_List is (Real, Account, Defered, Margin);
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

end Screen;

