with Ada.Exceptions;
with As.U, Socket, Event_Mng, Dynamic_List, Basic_Proc, Tcp_Util;
with Debug;
package body Connection is

  Server : Boolean := True;
  Own_Color : Space.Color_List := Space.White;

  Soc : Socket.Socket_Dscr;
  Fd  : Event_Mng.File_Desc;
  Server_Host : Socket_Util.Remote_Host;
  Local_Port : Socket_Util.Local_Port;
  Remote_Port : Socket_Util.Remote_Port;

  type Message_Kind_List is (Init, Move, Error);
  type Error_List is (Busy, Color, Protocol);
  type Message_Type (Kind : Message_Kind_List := Init) is record
    case Kind is
      when Init =>
        -- Send own color
        Color : Space.Color_List;
      when Move =>
        -- Send movement
        Action : Players.Action_Rec;
      when Error =>
        Error : Error_List;
    end case;
  end record;

  package Action_Dyn_List_Mng is new Dynamic_List (Players.Action_Rec);
  package Action_List_Mng renames Action_Dyn_List_Mng.Dyn_List;
  Action_List : Action_List_Mng.List_Type;

  -- Has an action been received and get it
  function Action_Received return Boolean is (not Action_List.Is_Empty);

  function Receive return Players.Action_Rec is
    Action : Players.Action_Rec;
  begin
    Action_List.Rewind (Action_List_Mng.Prev);
    Action_List.Get (Action, Action_List_Mng.Prev);
    return Action;
  exception
    when Action_List_Mng.Empty_List =>
      raise No_Action;
  end Receive;

  -- Init completed?
  We_Are_Ready : Boolean := False;

  -- Have we received a move (to discard instead of reject
  --  client init retries
  We_Have_Moved : Boolean := False;

  procedure Chess_Read is new Socket.Receive (Message_Type);
  procedure Chess_Send is new Socket.Send    (Message_Type);

  function Rec_Call_Back (Unused_Fd   : in Event_Mng.File_Desc;
                          Unused_Read : in Boolean) return Boolean;

  procedure Raise_Error (Error : in Error_List) is
  begin
    case Error is
      when Color =>
        raise Color_Error;
      when Protocol =>
        raise Protocol_Error;
      when Busy =>
        raise Busy_Error;
    end case;
  end Raise_Error;

  procedure Close is
  begin
    if Soc.Is_Open then
      Event_Mng.Del_Fd_Callback (Fd, True);
      Soc.Close;
    end if;
  end Close;

  -- May handle Lost_conn/Reconn via a timer
  procedure My_Send (Message : in Message_Type) is
  begin
    if Soc.Is_Open then
      Chess_Send (Soc, Message);
    end if;
  exception
    when Socket.Soc_Conn_Lost =>
      null;
  end My_Send;


  procedure Con_Call_Back (Unused_Remote_Host_Id  : in Socket_Util.Host_Id;
                           Unused_Remote_Port_Num : in Socket_Util.Port_Num;
                           Unused_Connected       : in Boolean;
                           Dscr                   : in Socket.Socket_Dscr) is
  begin
    if Debug.Get (Debug.Connection) then
      Basic_Proc.Put_Line_Output ("Connect callback");
    end if;
    Soc := Dscr;
    Fd := Soc.Get_Fd;
    Event_Mng.Add_Fd_Callback (Fd, True, Rec_Call_Back'Access);
    My_Send ((Init, Own_Color));
  end Con_Call_Back;

  procedure Connect_Server is
    Dummy : Boolean;
  begin
    Close;
    Dummy := Tcp_Util.Connect_To (Socket.Tcp_Header,
                                  Server_Host,
                                  Remote_Port,
                                  Con_Call_Back'Access,
                                  1.0, 0);
  exception
    when Error: others =>
      if Debug.Get (Debug.Connection) then
        Basic_Proc.Put_Line_Output ("Connect exception: "
           & Ada.Exceptions.Exception_Name(Error));
      end if;
      raise;
  end Connect_Server;

  procedure Acc_Call_Back (Local_Port_Num  : in Socket_Util.Port_Num;
                           Unused_Local_Dscr      : in Socket.Socket_Dscr;
                           Unused_Remote_Host_Id  : in Socket_Util.Host_Id;
                           Unused_Remote_Port_Num : in Socket_Util.Port_Num;
                           New_Dscr        : in Socket.Socket_Dscr) is
  begin
    if Debug.Get (Debug.Connection) then
      Basic_Proc.Put_Line_Output ("Accept callback");
    end if;
    Tcp_Util.Abort_Accept (Socket.Tcp_Header, Local_Port_Num);
    Soc := New_Dscr;
    Fd := Soc.Get_Fd;
    Event_Mng.Add_Fd_Callback (Fd, True, Rec_Call_Back'Access);
  end Acc_Call_Back;

  procedure Accept_Client is
    Acc_Dscr : Socket.Socket_Dscr;
    Port_Num : Socket_Util.Port_Num;
  begin
    Close;
    Tcp_Util.Accept_From (Socket.Tcp_Header,
                          Local_Port,
                          Acc_Call_Back'Access,
                          Acc_Dscr,
                          Port_Num);
  end Accept_Client;

  function Rec_Call_Back (Unused_Fd : in Event_Mng.File_Desc;
                          Unused_Read : in Boolean)
           return Boolean is
    Default_Action : Players.Valid_Action_Rec;
    -- Largest message
    Message : Message_Type := (Kind => Move, Action => Default_Action);
    Len : Natural;
    use type Space.Color_List;
  begin
    if Debug.Get (Debug.Connection) then
      Basic_Proc.Put_Line_Output ("In receive callback : ");
    end if;
    begin
      Chess_Read (Soc, Message, Len, False);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        if Debug.Get (Debug.Connection) then
          Basic_Proc.Put_Line_Output ("Lost connection - Discard");
        end if;
        if Server then
          Accept_Client;
        else
          Connect_Server;
        end if;
        return False;
      when Error: others =>
        if Debug.Get (Debug.Connection) then
          Basic_Proc.Put_Line_Output ("Exception: "
             & Ada.Exceptions.Exception_Name(Error));
        end if;
        raise;
    end;
    if Debug.Get (Debug.Connection) then
      Basic_Proc.Put_Line_Output ("Message read " & Integer'Image (Len) & " bytes");
    end if;
    case We_Are_Ready is

      when False =>
        -- Check message kind is Init and check colors
        if Server then
          -- We receive a client message
          case Message.Kind is
            when Init =>
              if Message.Color /= Own_Color then
                -- Reply by our color and accept client
                My_Send ((Init, Own_Color));
                We_Are_Ready := True;
                We_Have_Moved := False;
              else
                -- We have same color
                My_Send ((Error, Color));
              end if;
            when Move | Error =>
              -- Not an Init request. Reject.
              My_Send ((Error, Protocol));
          end case;

        else
          -- Client
          if Message.Kind = Init and then Message.Color /= Own_Color then
            We_Are_Ready := True;
            We_Have_Moved := False;
          elsif Message.Kind = Error then
            Raise_Error (Message.Error);
          else
            raise Protocol_Error;
          end if;
        end if;

      when True =>
        -- We are ready
        if Server then
          if Message.Kind /= Move then
            if We_Have_Moved then
              -- Not a retry
              My_Send ((Error, Protocol));
            end if;
            return True;
          end if;
        else
          -- Client
          if Message.Kind = Error then
            Raise_Error (Message.Error);
          elsif Message.Kind /= Move then
            raise Protocol_Error;
          end if;
        end if;

        if Debug.Get (Debug.Connection) then
          Basic_Proc.Put_Line_Output ("In rec callback : Saving action");
        end if;

        -- Insert action
        Action_List.Rewind (Check_Empty => False);
        Action_List.Insert (Message.Action, Action_List_Mng.Prev);
        We_Have_Moved := True;

    end case;

    if Debug.Get (Debug.Connection) then
      Basic_Proc.Put_Line_Output ("In rec callback : End");
    end if;
    return True;
  end Rec_Call_Back;

  -- Initialise connection
  -- If server name is empty, we are server
  procedure Init (Server_Name : in String;
                  Port : Socket_Util.Remote_Port;
                  Color : in Space.Color_List) is
    use type Socket_Util.Local_Port_List;
  begin
    -- Init parameters and socket
    Own_Color := Color;
    if Server_Name /= "" then
      Server := False;
      Server_Host.Name := As.U.Tus (Server_Name);
      Remote_Port := Port;
      Connect_Server;
    else
      if Port.Kind = Socket_Util.Port_Name_Spec then
        Local_Port := (Socket_Util.Port_Name_Spec, Port.Name);
      else
        Local_Port := (Socket_Util.Port_Num_Spec, Port.Num);
      end if;
      loop
        begin
          Accept_Client;
          exit;
        exception
          when Socket.Soc_Addr_In_Use =>
            if Debug.Get (Debug.Connection) then
              Basic_Proc.Put_Line_Output ("Address in use: Waiting");
            end if;
            Event_Mng.Wait (10_000);
        end;
      end loop;
    end if;

    if Debug.Get (Debug.Connection) then
      Basic_Proc.Put_Line_Output ("Connection: init done");
    end if;
  exception
    when others =>
      raise Connection_Error;
  end Init;

  procedure Wait_Ready is
  begin
    if Debug.Get (Debug.Connection) then
      Basic_Proc.Put_Line_Output ("Connection: waiting");
    end if;
    loop
      -- Wait
      Event_Mng.Wait (-1);
      exit when We_Are_Ready;
    end loop;
    if Debug.Get (Debug.Connection) then
      Basic_Proc.Put_Line_Output ("Connection: established");
    end if;
  end Wait_Ready;

  procedure Send (Action : in Players.Action_Rec) is
    Message : constant Message_Type := (Move, Action);
  begin
    My_Send (Message);
    if Debug.Get (Debug.Connection) then
      Basic_Proc.Put_Line_Output ("Connection: sent "
         & Integer'Image (Message'Size / 8) & " bytes");
    end if;
  end Send;

end Connection;

