package Event_Mng.Handling is

  -----------------------------
  -- Event internal handling --
  ------- ----------------------

  -- This low-level operation shall NOT be called by applications
  ---------------------------------------------------------------
  --  except if they implement their own waiting point instead of
  --  Event_Mng.Wait

  -- Internal event got by another waiting point
  type Event_Rec (Kind : In_Event_List := Fd_Event) is record
    case Kind is
      when Fd_Event =>
        Fd : File_Desc;
        Read : Boolean;
      when Signal_Event =>
        null;
      when Timeout =>
        null;
    end case;
  end record;

  -- Handle an internal event
  function Handle (Event : Event_Rec) return Out_Event_List;

end Event_Mng.Handling;

