package Executor is

  -- Init the executor (arms timers and sets handlers)
  procedure Init;

  -- Has exit been requested
  function Exit_Requested return Boolean;

  -- Terminate actions (flush actions pending on latency)
  procedure Close;

end Executor;

