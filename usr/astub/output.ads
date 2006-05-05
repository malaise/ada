package Output is

  -- Put Str at correct indentation Level, optionaly in comment
  procedure Put (Str : in String;
                 Comment : in Boolean;
                 Level : in Natural := 0);

  Line_Feed_Error : exception;

  procedure Put_Line (Str : in String;
                      Comment : in Boolean;
                      Level : in Natural := 0);

  procedure New_Line;

end Output;

