with Ada.Text_Io, Ada.Exceptions;
with As.U; use As.U;
with Socket, Event_Mng, Debug, Dynamic_List;
package body Connection is

  Server : Boolean := True;
  Own_Color : Space.Color_List := Space.White;

  Soc : Socket.Socket_Dscr;
  Fd  : Event_Mng.File_Desc;
  Server_Host : Tcp_Util.Remote_Host;
  Local_Port : Tcp_Util.Local_Port;
  Remote_Port : Tcp_Util.Remote_Port;

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
  function Action_Received return Boolean is
  begin
    return not Action_List.Is_Empty;
  end;

  function Receive return Players.Action_Rec is
    Action : Players.Action_Rec;
  begin
    Action_List.Rewind (True, Action_List_Mng.Prev);
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

  function Rec_Call_Back (Fd : in Event_Mng.File_Desc; Read : in Boolean)
  return Boolean;

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


  procedure Con_Call_Back (Remote_Port_Num : in Tcp_Util.Port_Num;
                           Remote_Host_Id  : in Tcp_Util.Host_Id;
                           Connected       : in Boolean;
                           Dscr            : in Socket.Socket_Dscr) is
    pragma Unreferenced (Remote_Port_Num, Remote_Host_Id, Connected);
  begin
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Connect callback");
    end if;
    Soc := Dscr;
    Fd := Soc.Get_Fd;
    Event_Mng.Add_Fd_Callback (Fd, True, Rec_Call_Back'Access);
    My_Send ((Init, Own_Color));
  end Con_Call_Back;

  procedure Connect_Server is
    Ok : Boolean;
    pragma Unreferenced (Ok);
  begin
    Close;
    Ok := Tcp_Util.Connect_To (Socket.Tcp_Header,
                               Server_Host,
                               Remote_Port,
                               1.0, 0,
                               Con_Call_Back'Access);
  exception
    when Error: others =>
      if Debug.Get (Debug.Connection) then
        Ada.Text_Io.Put_Line ("Connect exception: "
           & Ada.Exceptions.Exception_Name(Error));
      end if;
      raise;
  end Connect_Server;

  procedure Acc_Call_Back (Local_Port_Num  : in Tcp_Util.Port_Num;
                           Local_Dscr      : in Socket.Socket_Dscr;
                           Remote_Port_Num : in Tcp_Util.Port_Num;
                           Remote_Host_Id  : in Tcp_Util.Host_Id;
                           New_Dscr        : in Socket.Socket_Dscr) is
    pragma Unreferenced (Local_Dscr, Remote_Port_Num, Remote_Host_Id);
  begin
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Accept callback");
    end if;
    Tcp_Util.Abort_Accept (Socket.Tcp_Header, Local_Port_Num);
    Soc := New_Dscr;
    Fd := Soc.Get_Fd;
    Event_Mng.Add_Fd_Callback (Fd, True, Rec_Call_Back'Access);
  end Acc_Call_Back;

  procedure Accept_Client is
    Acc_Dscr : Socket.Socket_Dscr;
    Port_Num : Tcp_Util.Port_Num;
  begin
    Close;
    Tcp_Util.Accept_From (Socket.Tcp_Header,
                          Local_Port,
                          Acc_Call_Back'Access,
                          Acc_Dscr,
                          Port_Num);
  end Accept_Client;

  function Rec_Call_Back (Fd : in Event_Mng.File_Desc; Read : in Boolean)
           return Boolean is
    pragma Unreferenced (Fd, Read);
    Default_Action : Players.Valid_Action_Rec;
    -- Largest message
    Message : Message_Type := (Kind => Move, Action => Default_Action);
    Len : Natural;
    use type Space.Color_List;
    use type Socket.Host_Id, Socket.Port_Num;
  begin
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("In receive callback : ");
    end if;
    begin
      Chess_Read (Soc, Message, Len, False);
    exception
      when Socket.Soc_Conn_Lost | Socket.Soc_Read_0 =>
        if Debug.Get (Debug.Connection) then
          Ada.Text_Io.Put_Line ("Lost connection - Discard");
        end if;
        if Server then
          Accept_Client;
        else
          Connect_Server;
        end if;
        return False;
      when Error: others =>
        if Debug.Get (Debug.Connection) then
          Ada.Text_Io.Put_Line ("Exception: "
             & Ada.Exceptions.Exception_Name(Error));
        end if;
        raise;
    end;
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Message read " & Integer'Image (Len) & " bytes");
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
          Ada.Text_Io.Put_Line ("In rec callback : Saving action");
        end if;

        -- Insert action
        Action_List.Rewind (False);
        Action_List.Insert (Message.Action, Action_List_Mng.Prev);
        We_Have_Moved := True;

    end case;

    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("In rec callback : End");
    end if;
    return True;
  end Rec_Call_Back;

  -- Initialise connection
  -- If server name is empty, we are server
  procedure Init (Server_Name : in String;
                  Port : Tcp_Util.Remote_Port;
                  Color : in Space.Color_List) is
    use type Tcp_Util.Local_Port_List;
  begin
    -- Init parameters and socket
    if Server_Name /= "" then
      Server := False;
      Server_Host.Name := Asu_Tus (Server_Name);
      Remote_Port := Port;
      Connect_Server;
    else
      if Port.Kind = Tcp_Util.Port_Name_Spec then
        Local_Port := (Tcp_Util.Port_Name_Spec, Port.Name);
      else
        Local_Port := (Tcp_Util.Port_Num_Spec, Port.Num);
      end if;
      loop
        begin
          Accept_Client;
          exit;
        exception
          when Socket.Soc_Addr_In_Use =>
            if Debug.Get (Debug.Connection) then
              Ada.Text_Io.Put_Line ("Address in use: Waiting");
            end if;
            Event_Mng.Wait (10_000);
        end;
      end loop;
    end if;
    Own_Color := Color;

    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Connection: init done");
    end if;
  exception
    when others =>
      raise Connection_Error;
  end Init;

  procedure Wait_Ready is
  begin
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Connection: waiting");
    end if;
    loop
      -- Wait
      Event_Mng.Wait (-1);
      exit when We_Are_Ready;
    end loop;
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Connection: established");
    end if;
  end Wait_Ready;

  procedure Send (Action : in Players.Action_Rec) is
    Message : constant Message_Type := (Move, Action);
  begin
    My_Send (Message);
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Connection: sent "
         & Integer'Image (Message'Size / 8) & " bytes");
    end if;
  end Send;

end Connection;

