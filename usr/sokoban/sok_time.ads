package SOK_TIME is

  -- Just start and stop the clock
  procedure START_TIME;
  procedure STOP_TIME;

  procedure DISP_TIME;

  -- Resets or sets the clock
  procedure RESET_TIME;
  procedure SET_TIME (DAY : NATURAL; DUR : DURATION);
  -- Get current clock value
  procedure GET_TIME (DAY : out NATURAL; DUR : out DURATION);

end SOK_TIME;
