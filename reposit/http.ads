with As.U;
package Http is

  -------------------------------
  -- Result of the Get request --
  -------------------------------
  Max_Msg_Len : constant := 1024 * 1024 * 1024;
  type Result_Kind_List is (Ok, Client_Error, Server_Error);
  type Client_Error_List is (
    Invalid_Url,    -- Not "http://<host>/<opt_content>
    Name_Error,     -- Host or port not found
    Invalid_Answer, -- Not "HTTP/<vers> <code> <text>"
    Missing_Length, -- No "Content-Length:" in answer
    Msg_Too_Long,   -- Answer message is too long (above Max_Msg_Len)
    Wrong_Length,   -- Length received (before disconnection) /= Content-Length
    No_Server,      -- Could not connect to server
    Timeout);       -- Timeout expired before completion
  subtype Server_Code_Range is Positive range 100 .. 999;
  type Result_Type (Kind : Result_Kind_List := Ok) is record
    case Kind is
      when Ok =>
        Content : As.U.Asu_Us;
      when Client_Error =>
        Error : Client_Error_List;
      when Server_Error =>
        Code : Server_Code_Range;
        Message : As.U.Asu_Us;
    end case;
  end record;

  ---------------------------------------------------------------------------
  -- Get the (file) content of an URL.                                     --
  -- URL must have the format "http://<host>[:<port>]/<path>               --
  -- Result can be Ok (and content), Client error (parsing, connection...) --
  --  or Server error.                                                     --
  --                                                                       --
  -- Set Environ variable HTTP_TIMEOUT_MS to the overall timeout (default  --
  --  is infinite, but other errors are detected).                         --
  -- Set Environ variable HTTP_CONNECT_TIMEOUT_MS to the connection        --
  --  timeout (one try, default is 3000).                                          --
  --                                                                       --
  -- Because it waits for replies from the HTTP server, this function uses --
  --  Event_Mng.Wait. As a consequence:        --
  --  * X11 programs shall Suspend ALL the X objects X_Line/Con_Io/Afpx    --
  --    before calling this function, then Resume the X objects.           --
  --  * This function is not re-entrant and is protected by a mutex.     --
  ---------------------------------------------------------------------------
  function Get (Url : String) return Result_Type;

end Http;

