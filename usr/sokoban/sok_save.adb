with Queues;

-- salvage of movements for undo, save/restore ...
package body Sok_Save is

  package Movement_Lifo is new Queues.Lifo (
   Size => Nbre_Save,
   Item => Sok_Movement.Saved_Data_Rec);

  Index_Save : Positive range 1 .. Nbre_Save;

  -- when new frame, reset stack and photo
  procedure Reset is
    Movement : Sok_Movement.Saved_Data_Rec;
  begin
    loop
      begin
        Movement := Pop;
      exception
        when No_More_Saved_Movements =>
          exit;
      end;
    end loop;
  end Reset;


  -- circular buffer of saved movements for undo
  procedure Push (Man_Movement : in Sok_Movement.Saved_Data_Rec) is
  begin
    Movement_Lifo.Push (Man_Movement);
  exception
    when Movement_Lifo.Lifo_Full =>
      -- lifo is full : make a space and retry
      Movement_Lifo.Discard_Last;
      Movement_Lifo.Push (Man_Movement);
  end Push;



  function Pop return Sok_Movement.Saved_Data_Rec is
    Movement : Sok_Movement.Saved_Data_Rec;
  begin
    Movement_Lifo.Pop (Movement);
    return Movement;
  exception
    when Movement_Lifo.Lifo_Empty =>
      raise No_More_Saved_Movements;
  end Pop;

  -- look first pushed or next pushed
  -- type Look_Ref_List is (First, Next);
  function Look (Ref : Look_Ref_List) return Sok_Movement.Saved_Data_Rec is
    Movement : Sok_Movement.Saved_Data_Rec;
  begin
    if Ref = First then
      Index_Save := 1;
    elsif Index_Save = Nbre_Save then
      raise No_More_Saved_Movements;
    else
      Index_Save := Index_Save + 1;
    end if;

    Movement_Lifo.Look_Last (Movement, Index_Save);
    return Movement;
  exception
    when Movement_Lifo.Lifo_Empty | Movement_Lifo.Lifo_Not =>
      raise No_More_Saved_Movements;
  end Look;

end Sok_Save;

