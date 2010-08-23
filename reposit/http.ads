with As.U; use As.U;
package Http is

  -------------------------------
  -- Result of the Get request --
  -------------------------------
  Max_Msg_Len : constant := 1024 * 1024 * 1024;
  type Result_Kind_List is (Ok, Client_Error, Server_Error);
  type Client_Error_List is (
    Invalid_Url,    -- Not "http://<host>/<opt_content>
    Invalid_Answer, -- Not "HTTP/<vers> <code> <text>"
    Missing_Length, -- No "Content-Length:" in answer
    Msg_Too_Long,   -- Answer message is too long (above Max_Msg_Len)
    Wrong_Length,   -- Length received (before disconnection) /= Content-Length
    Timeout);       -- Timeout expired before completion
  subtype Server_Code_Range is Positive range 100 .. 999;
  type Result_Type (Kind : Result_Kind_List := Ok) is record
    case Kind is
      when Ok =>
        Content : Asu_Us;
      when Client_Error =>
        Error : Client_Error_List;
      when Server_Error =>
        Code : Server_Code_Range;
        Message : Asu_Us;
    end case;
  end record;

  ---------------------------------------------------------------------------
  -- Get the (file) content of an URL.                                     --
  -- Result can be Ok (and content), Client error (parsing, connection...) --
  --  or Server error.                                                     --
  -- Because it waits for replies from HTTp server, this function uses     --
  --  Event_Mng.Wait, which sets signal handlers. As a consequence:        --
  --  * Non interactive programs shall call                                --
  --    Event_Mng.Reset_Default_Signal_Policy after using this function    --
  --  * X11 programs shall Suspend ALL the X objects X_Line/Con_Io/Afpx    --
  --    before calling this function, then Resume the X objects.           --
  ---------------------------------------------------------------------------
  function Get (Url : String) return Result_Type;
end Http;

