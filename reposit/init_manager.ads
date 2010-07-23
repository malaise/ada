-- Handles the initialisation of state while
--  events arrive (that aim to modify state)
generic

  type Event_Type is private;

package Init_Manager is

  -- Declare the event handler (overwrites any previous),
  -- As long as there is no handler, new events are bufferized
  -- When being set, handler is called with bufferized events
  --  then with any new event
  type Event_Handler is access procedure (Event : in Event_Type);
  procedure Set_Handler (Handler : Event_Handler);

  -- Inject a new event
  -- Events arriving while there is no handler or while there are already some
  --  pending events are bufferized
  procedure New_Event (Event : in Event_Type);

end Init_Manager;

