with Sok_Types;
with Sok_Movement;

-- salvage of movements for undo, save/restore ...
package Sok_Save is
  -- Max nbre of saved movements
  Nbre_Save : constant := 2000; -- 64 Kb

  -- when new frame, reset stack
  procedure Reset;

  -- circular buffer of saved movements for undo
  procedure Push (Man_Movement : in Sok_Movement.Saved_Data_Rec);
  function Pop  return Sok_Movement.Saved_Data_Rec;


  -- look first pushed or next pushed
  type Look_Ref_List is (First, Next);
  -- look first will return first pushed movemtny and so on ...
  function Look (Ref : Look_Ref_List) return Sok_Movement.Saved_Data_Rec;


  -- on pop if circular buffer is empty
  -- on look if no more movement to look
  No_More_Saved_Movements : exception;

end Sok_Save;