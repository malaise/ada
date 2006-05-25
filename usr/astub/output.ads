package Output is

  -- Return the indentation of a given level
  function Get_Indent (Level : in Natural) return String;

  -- Put Str, optionaly at correct indentation level, optionaly in comment
  -- Str may contain Common.Line_Feed, so that it contains several lines
  --  each of them can be split n several parts if too long.
  -- If Indent is set, level applies to indent each line (and parts split)
  -- If Indent is not set, level is used only on split parts of lines
  procedure Put (Str : in String;
                 Comment : in Boolean;
                 Level : in Natural := 0;
                 Indent : in Boolean := False);

  Line_Feed_Error : exception;

  procedure Put_Line (Str : in String;
                      Comment : in Boolean;
                      Level : in Natural := 0;
                      Indent : in Boolean := False);

  procedure New_Line;

end Output;

