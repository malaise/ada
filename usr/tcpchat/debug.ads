package Debug is
  function Is_On return Boolean;
  procedure Log (Msg : in String; New_Line : in Boolean := True);
end Debug;

