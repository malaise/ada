with As.U; use As.U;
package Ios is

  -- Init the accepting of connections on port (name or num)
  procedure Init (Port : in Asu_Us);
  Init_Error : exception;

  -- Wait during Timemout (or up to disconnection)
  procedure Wait (Timeout_Ms : in Integer;
                  Disconnection : out Boolean);

  -- Read next sentence. Wait up to Timeout
  -- If Disconnection, other parameters are not significant
  -- Elsif Timeout, other parameters is not significant
  -- Else Text is significant
  procedure Read (Timeout_Ms : in Integer;
                  Disconnection : out Boolean;
                  Timeout : out Boolean;
                  Text : out Asu_Us);

  -- Send a sentence. Disconnection if error
  procedure Send (Text : in Asu_Us;
                  Disconnection : out Boolean);

  -- Close and re-open
  procedure Reset;

  -- Close (final)
  procedure Close;

end Ios;

