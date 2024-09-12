-- Handles the initialisation of a state, while
--  events arrive (events that aim to modify this state)
generic

  type Event_Type is private;

package Init_Mng is

  -- Declare the event handler (overwrites any previous),
  -- As long as there is no handler, new events are bufferized
  -- When being set, the handler is called with the bufferized events,
  --  then with any new event that was injected meanwhile,
  --  then this call returns
  -- Exceptions raised by Handler are caught and ignored
  type Event_Handler is access procedure (Event : in Event_Type);
  procedure Set_Handler (Handler : Event_Handler);

  -- Inject a new event
  -- Events arriving while there is no handler or while there are already some
  --  pending events are bufferized, otherwise they are processed synchronously
  --  (by calling the handler)
  procedure New_Event (Event : in Event_Type);

end Init_Mng;

