with As.U;
package Ios is

  -- Init the accepting of connections on port (name or num)
  procedure Init (Port : in As.U.Asu_Us);
  Init_Error : exception;

  -- Kind of event
  type Event_Kind_List is (
    Exit_Requested, -- User has hit Ctrl C
    Disconnection,  -- Tcp connection broken
    Global_Timeout, -- Global timeout has expired without sentence
    Local_Timeout,  -- Read timeout without sentence, or end of Wait
    Got_Sentence);  -- Got a sentence
  type Event_Type (Kind : Event_Kind_List := Got_Sentence) is record
    case Kind is
      when Got_Sentence =>
        Sentence : As.U.Asu_Us;
      when others =>
        null;
    end case;
  end record;

  -- Arm / cancel global timer
  procedure Start_Global_Timer (Timeout_Ms : Integer);
  procedure Stop_Global_Timer;

  -- Wait during Timeout (or up to global timeout or disconnection)
  -- Normaly returns Local_Timeout
  function Wait (Timeout_Ms : Integer) return Event_Type;

  -- Read next sentence. Wait up to Timeout
  function Read (Timeout_Ms : Integer) return Event_Type;

  -- Send a sentence. Disconnection if error
  procedure Send (Text : in As.U.Asu_Us;
                  Disconnection : out Boolean);

  -- Close and re-open
  procedure Reset;

  -- Close (final)
  procedure Close;

end Ios;

