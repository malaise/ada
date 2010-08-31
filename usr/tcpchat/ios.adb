-- with As.U;
-- use As.U;

package body Ios is

  -- Init the accepting of connections on port (name or num)
  procedure Init (Port : in Asu_Us) is
  begin
    null;
  end Init;

  -- Wait during Timemout (or up to disconnection)
  procedure Wait (Timeout_Ms : in Integer;
                  Disconnection : out Boolean) is
  begin
    null;
  end Wait;

  -- Read next sentence. Wait up to Timeout
  -- If Disconnection, other parameters are not significant
  -- Elsif Timeout, other parameters is not significant
  -- Else Text is significant
  procedure Read (Timeout_Ms : in Integer;
                  Disconnection : out Boolean;
                  Timeout : out Boolean;
                  Text : out Asu_Us) is
  begin
    null;
  end Read;

  -- Send a sentence. Disconnection if error
  procedure Send (Text : in Asu_Us;
                  Disconnection : out Boolean) is
  begin
    null;
  end Send;

  -- Close and re-open
  procedure Reset is
  begin
    null;
  end Reset;

  -- Close (final)
  procedure Close is
  begin
    null;
  end Close;

end Ios;

