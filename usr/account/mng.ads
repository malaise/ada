with Oper_Def;
-- Manage the whole acount status
package Mng is
  -- Nb of operations in current account
  Max_Nb_Oper : constant := 9999;
  subtype Oper_Nb_Range is Natural range 0 .. Max_Nb_Oper;
  subtype Oper_Range is Positive range 1 .. Max_Nb_Oper;

  -- Update current (selected) operation
  -- Does nothing with 0 (empty list)
  procedure Set_Current (No : in Oper_Nb_Range);

  -- Modify the account. Load and clear check it is saved.
  -- Load a file. If file name is "" then ask for it
  procedure Load (File_Name : in String);
  -- Save current file after confirmation or in rescue file
  procedure Save (Rescue : in Boolean := False);
  -- Clear current account (and file name)
  procedure Clear;
  -- Sort operation
  procedure Sort;
  -- Print listing
  procedure Print;
  -- Update the displayed amounts of opers, sums
  procedure Change_Unit;

  -- Modify operations
  procedure Update_State;
  procedure Add_Oper;
  procedure Edit_Oper;
  procedure View_Oper;
  procedure Del_Oper;
  procedure Garbage_Collect;
  procedure Search;
  procedure Show_All;

  -- Get data
  function Is_Saved return Boolean;

end Mng;
