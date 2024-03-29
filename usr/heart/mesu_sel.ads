with Afpx;
with Pers_Def, Mesu_Def, Mesu_Nam;

-- Mesure selection management
package Mesu_Sel is

  -- Criteria for adding to / removing from selection
  -- Date_Aft and Date_Bef are themselves included
  --  (they ar reather "From included" and "To included" criteria)
  type Criteria_Rec is record
    Name     : Pers_Def.Person_Name_Str;
    Activity : Pers_Def.Person_Activity_Str;
    Date_Aft : Mesu_Def.Date_Str;
    Date_Bef : Mesu_Def.Date_Str;
  end record;

  -- Add records to selection
  procedure Add_Selection (Criteria : in Criteria_Rec);

  -- Remove records from selection
  procedure Rem_Selection (Criteria : in Criteria_Rec);

  -- Clear all selection
  procedure Clear_Selection;

  -- Add a record to selection
  procedure Add_Selection (Name : in Mesu_Nam.File_Name_Str);

  -- Remove a record from selection
  procedure Rem_Selection (Name : in Mesu_Nam.File_Name_Str);
  -- This one to be used if the record file is already deleted
  procedure Rem_Selection (Line : in Afpx.Line_Rec);


  -- Load the selection from file
  procedure Load;

  -- Save the selection to file
  procedure Save;

  -- Undo (if possible) previous action on selection
  procedure Undo;

end Mesu_Sel;

