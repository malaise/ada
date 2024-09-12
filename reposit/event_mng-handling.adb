with Timers.Expiration;
package body Event_Mng.Handling is

  function Handle (Event : Event_Rec) return Out_Event_List is
    Cb_Searched : Cb_Rec;
    Signal_Kind : Signal_Kind_List;
    Cb_Term_Sig, Cb_Child_Sig, Cb_Usr1_Sig, Cb_Usr2_Sig : Sig_Callback;
  begin
    Logger.Log_Debug ("Event_Mng.Handle event " & Event.Kind'Img);
    case Event.Kind is
      when Fd_Event =>
        -- A FD event
        -- Search and read callback
        Cb_Searched.Fd := Event.Fd;
        Cb_Searched.Read := Event.Read;
        if not Search_Cb (Cb_Searched) then
          Logger.Log_Debug ("**** Event_Mng.Handle: "
                   & File_Desc'Image(Event.Fd)
                   & " fd not found ****");
        else
          Logger.Log_Debug ("Event_Mng.Handle calling Cb on fd "
                   & Event.Fd'Img & " " & Event.Read'Img);
          -- Call it and propagate event if callback returns true
          if Cb_Searched.Cb /= null then
            begin
              if Cb_Searched.Cb (Cb_Searched.Fd, Cb_Searched.Read) then
                return Fd_Event;
              end if;
            exception
              when others =>
                raise Cb_Error;
            end;
          end if;
        end if;
      when Signal_Event =>
        Signal_Kind := Get_Signal_Kind;
        Cb_Term_Sig := Get_Term_Cb;
        Cb_Child_Sig := Get_Child_Cb;
        Cb_Usr1_Sig := Get_Usr1_Cb;
        Cb_Usr2_Sig := Get_Usr2_Cb;
        Logger.Log_Debug ("Event_Mng.Handle " & Signal_Kind'Img
                 & " with term cb: " & Boolean'Image(Cb_Term_Sig /= null)
                 & " and child cb: " & Boolean'Image(Cb_Child_Sig /= null)
                 & " and usr1 cb: " & Boolean'Image(Cb_Usr1_Sig /= null)
                 & " and usr2 cb: " & Boolean'Image(Cb_Usr2_Sig /= null));
        case Signal_Kind is
          when Unknown_Sig | No_Sig =>
            -- No_Event
            null;
          when Dummy_Sig =>
            -- Dummy signal: never call Cb but always generate event
            return Signal_Event;
          when Terminate_Sig =>
            if Cb_Term_Sig /= null then
              begin
                Cb_Term_Sig.all;
              exception
                when others =>
                  raise Cb_Error;
              end;
              return Signal_Event;
            end if;
          when Child_Sig =>
            if Cb_Child_Sig /= null then
              begin
                Cb_Child_Sig.all;
              exception
                when others =>
                  raise Cb_Error;
              end;
              return Signal_Event;
            end if;
          when Usr1_Sig =>
            if Cb_Usr1_Sig /= null then
              begin
                Cb_Usr1_Sig.all;
              exception
                when others =>
                  raise Cb_Error;
              end;
              return Signal_Event;
            end if;
          when Usr2_Sig =>
            if Cb_Usr2_Sig /= null then
              begin
                Cb_Usr2_Sig.all;
              exception
                when others =>
                  raise Cb_Error;
              end;
              return Signal_Event;
            end if;
            -- else No_Event
        end case;
      when Timeout =>
        -- Nothing. Expire timers or return timeout
        if Timers.Expiration.Expire then
          Logger.Log_Debug ("Event_Mng.Handle: No_Event -> Timer_Event");
          return Timer_Event;
        end if;
    end case;
    return Timeout;

  end Handle;

end Event_Mng.Handling;

