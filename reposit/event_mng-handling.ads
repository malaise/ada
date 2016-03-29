package Event_Mng.Handling is

  -----------------------------
  -- Event internal handling --
 ------- ----------------------
  -- This low-level operation shall NOT be called by applications
  -- Handle an internal event
  function Handle (Event : Event_Rec) return Out_Event_List;

end Event_Mng.Handling;

