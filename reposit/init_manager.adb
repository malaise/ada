with Unlimited_Pool, Mutex_Manager;
package body Init_Manager is

  -- The mutex to protect pool and handler
  Mutex : Mutex_Manager.Simple_Mutex;

  -- The pool of pending events: Fifo
  package Event_Pool_Mng is new Unlimited_Pool (Event_Type, Lifo => False);
  Event_Pool : Event_Pool_Mng.Pool_Type;

  -- The event handler
  The_Handler : Event_Handler := null;

  -- Deliver the event to the handler, mutex must be acquired
  procedure Deliver (Event : in Event_Type) is
  begin
    -- No lock while calling handler
    Mutex.Release;
    -- Call handler and mask exception
    begin
      The_Handler (Event);
    exception
      when others =>
        null;
    end;
    -- Re-acquire mutex
    Mutex.Get;
  end Deliver;

  -- Flush pending events, mutex must be acquired and is released
  procedure Flush is
    Event : Event_Type;
  begin
    while The_Handler /= null and then not Event_Pool.Is_Empty loop
      -- Handler is set: deliver pending events
      Event_Pool.Pop (Event);
      Deliver (Event);
    end loop;
  end Flush;

  -- Clean mutex if acquired
  procedure Clean is
  begin
    if Mutex.Is_Owner then
      Mutex.Release;
    end if;
  end Clean;

  -- Declare the event handler (overwrites any previous),
  -- As long as there as no handler, new events are bufferized
  -- When being set, handler is called with bufferized events
  --  then with any new event when init stopped
  -- type Event_Handler is access procedure (Event : in Event_Type);
  procedure Set_Handler (Handler : Event_Handler) is
  begin
    Mutex.Get;
    The_Handler := Handler;
    Flush;
    -- Final release
    Mutex.Release;
  exception
    when others =>
      -- Always release
      Clean;
      raise;
  end Set_Handler;

  -- Inject a new event
  procedure New_Event (Event : in Event_Type) is
  begin
    Mutex.Get;
    if The_Handler /= null and then Event_Pool.Is_Empty then
      -- Handler set and no pending events => deliver
      Deliver (Event);
      Mutex.Release;
    else
      -- Either handler is null (then event will be flushed when setting it)
      -- Or pool is not empty (then flush is running and will flush event)
      -- Push the event and release
      Event_Pool.Push (Event);
      Mutex.Release;
    end if;
  exception
    when others =>
      -- Always release
      Clean;
      raise;
  end New_Event;

end Init_Manager;

