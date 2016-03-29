package body Event_Mng.Handling is

  -------------------------
  -- Low level operation --
  -------------------------
  function Handle (Event : Event_Rec) return Out_Event_List
                  renames Event_Mng.Handle;

end Event_Mng.Handling;

