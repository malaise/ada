with Unlimited_Pool, Mutex_Manager;
package body Init_Manager is

  -- The WR_Mutex is used for general protection
  -- Writers lock is used to accept new event, which ensures that several
  --  new events are accepted even if some events are to be delivered
  -- Readers lock is used to deliver an event to the handler (if there
  --  is not new event to accept)
  Wr_Mutex : Mutex_Manager.Mutex (Mutex_Manager.Write_Read, False);
  -- The Read_Mutex is used to protect the pool for event delivery
  --  because the WR_Mutext allows several simultaneous readers
  R_Mutex : Mutex_Manager.Simple_Mutex;

  -- The pool of pending events: Fifo
  package Event_Pool_Manager is new Unlimited_Pool (Event_Type, Lifo => False);
  package Event_Pool_Mng renames Event_Pool_Manager.Upool;
  Event_Pool : Event_Pool_Mng.Pool_Type;

  -- The event handler
  The_Handler : Event_Handler := null;

  -- Deliver the event to the handler
  procedure Deliver (Event : in Event_Type) is
  begin
    -- Call handler and mask exception
    begin
      The_Handler (Event);
    exception
      when others =>
        null;
    end;
  end Deliver;

  -- Declare the event handler (overwrites any previous),
  -- As long as there as no handler, new events are bufferized
  -- When being set, handler is called with bufferized events
  --  then with any new event when init stopped
  -- type Event_Handler is access procedure (Event : in Event_Type);
  procedure Set_Handler (Handler : Event_Handler) is
    Event : Event_Type;
  begin
    -- Acquire exclusive access if no event waiting to be accepted
    --  and set new handler
    Wr_Mutex.Get (Mutex_Manager.Read);
    R_Mutex.Get;
    The_Handler := Handler;
    -- Deliver pending events as long as no new event pending
    while The_Handler /= null and then not Event_Pool.Is_Empty loop
      -- Get pending event with exclusive access
      Event_Pool.Pop (Event);
      -- Release access for delivery
      R_Mutex.Release;
      Wr_Mutex.Release;
      Deliver (Event);
      -- Re-acquire exclusive access if no event waiting to be accepted
      Wr_Mutex.Get (Mutex_Manager.Read);
      R_Mutex.Get;
    end loop;
    -- Final release
    R_Mutex.Release;
    Wr_Mutex.Release;
  exception
    when others =>
      -- Always release
      if R_Mutex.Is_Owner then
        R_Mutex.Release;
      end if;
      if Wr_Mutex.Is_Owner then
        Wr_Mutex.Release;
      end if;
      raise;
  end Set_Handler;

  -- Accept a new event
  procedure New_Event (Event : in Event_Type) is
  begin
    -- Acquire exclusive access with high prio
    Wr_Mutex.Get (Mutex_Manager.Write);
    if The_Handler /= null and then Event_Pool.Is_Empty then
      -- Handler is set and no pending events => deliver directly
      Wr_Mutex.Release;
      Deliver (Event);
    else
      -- Either handler is null (then event will be flushed when setting it)
      -- Or pool is not empty (then flush is running and will flush event)
      -- Push the event and release
      Event_Pool.Push (Event);
      Wr_Mutex.Release;
    end if;
  exception
    when others =>
      -- Always release
      if Wr_Mutex.Is_Owner then
        Wr_Mutex.Release;
      end if;
      raise;
  end New_Event;

end Init_Manager;

