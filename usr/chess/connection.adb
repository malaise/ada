with Ada.Text_Io;
with Socket, X_Mng, Debug, Dynamic_List;
package body Connection is

  Server : Boolean := True;
  Own_Color : Space.Color_List := Space.White;

  Soc : Socket.Socket_Dscr;
  Fd  : X_Mng.File_Desc;
  Server_Port_Num : constant := 50000;
  Client_Host : Socket.Host_Id;
  Client_Port : Socket.Port_Num;

  Type Message_Kind_List is (Init, Move, Error);
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

  package Action_List_Mng is new Dynamic_List (Players.Action_Rec);
  Action_List : Action_List_Mng.List_Type;

  -- Has an action been received and get it
  function Action_Received return Boolean is
  begin
    return not Action_List_Mng.Is_Empty (Action_list);
  end;

  function Receive return Players.Action_Rec is
    ACtion : Players.Action_Rec;
  begin
    Action_List_Mng.Move_To (Action_list, Action_List_Mng.Prev, 0, False);
    Action_List_Mng.Get (Action_List, Action, Action_List_Mng.Prev);
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

  function Call_Back (Fd : in X_Mng.File_Desc; Read : in Boolean) return Boolean is
    Message : Message_Type;
    Len : Natural;
    use type Space.Color_List;
    use type Socket.Host_Id, Socket.Port_Num;
  begin
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put ("In callback : ");
    end if;
    begin
      Chess_Read (Soc, Message, Len, Server);
    exception
      when Socket.Soc_Conn_Lost =>
        Ada.Text_Io.Put_Line ("Lost connection - Discard");
        return False;
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
                Chess_Send (Soc, (Init, Own_Color));
                Client_Host := Socket.Get_Destination_Host (Soc);
                Client_Port := Socket.Get_Destination_Port (Soc);
                We_Are_Ready := True;
                We_Have_Moved := False;
              else
                -- We have same color
                Chess_Send (Soc, (Error, Color));
              end if;
            when Move | Error =>
              -- Not an Init request. Reject.
              Chess_Send (Soc, (Error, Protocol));
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
          -- Reject new client
          if      Socket.Get_Destination_Host (Soc) /= Client_Host
          or else Socket.Get_Destination_Port (Soc) /= Client_Port then
            -- New client
            if Message.Kind = Init then
              Chess_Send (Soc, (Error, Busy));
            else
              Chess_Send (Soc, (Error, Protocol));
            end if;
            -- Restore
            Socket.Set_Destination_Host_And_Port (Soc, Client_Host, Client_Port);
            return True;
          elsif Message.Kind /= Move then
            if We_Have_Moved then
              -- Not a retry
              Chess_Send (Soc, (Error, Protocol));
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
          Ada.Text_Io.Put_Line ("In callback : Saving action");
        end if;

        -- Insert action
        if not Action_List_Mng.Is_Empty (Action_list) then
          Action_List_Mng.Move_To (Action_list, Action_List_Mng.Next, 0, False);
        end if;
        Action_List_Mng.Insert (Action_List, Message.Action,
                                Action_List_Mng.Prev);
        We_Have_Moved := True;

    end case;

    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("In callback : End");
    end if;
    return True;
  end Call_Back;

  -- Initialise connection
  -- If server name is empty, we are server
  procedure Init (Server_Name : in String;
                  Color : in Space.Color_List) is
  begin
    if Server_Name /= "" then
      Server := False;
    end if;
    Own_Color := Color;

    -- Init socket
    Socket.Open (Soc, Socket.Udp);
    Fd := Socket.Fd_Of (Soc);
    X_Mng.X_Add_Callback (Fd, True, Call_Back'Access);
    if Server then
      Socket.Link_Port (Soc, Server_Port_Num);
    else
      Socket.Link_Dynamic (Soc);
      Socket.Set_Destination_Name_And_Port (Soc,
           False, Server_Name, Server_Port_Num);
    end if;

    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Connection: init done");
    end if;
  exception
    when others =>
      raise Connection_Error;
  end Init;

  procedure Close is
  begin
    X_Mng.X_Del_Callback (Fd, True);
    Socket.Close (Soc);
  end Close;

  procedure Wait_Ready is
    Select_Got_Fd : Boolean;
  begin
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Connection: waiting");
    end if;
    loop
      if not Server then
        -- Client sends its color to server
        Chess_Send (Soc, (Init, Own_Color));
        if Debug.Get (Debug.Connection) then
          Ada.Text_Io.Put_Line ("Connection: pinging");
        end if;
        -- Retry a bit later
        Select_Got_Fd := X_Mng.Select_No_X (1000);
      else
        -- Server waits
        Select_Got_Fd := X_Mng.Select_No_X (-1);
      end if;
      exit when We_Are_Ready;
    end loop;
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Connection: established");
    end if;
  end Wait_Ready;

  procedure Send (Action : in Players.Action_Rec) is
    Message : constant Message_Type := (Move, Action);
  begin
    Chess_Send (Soc, Message);
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Connection: sent "
         & Integer'Image (Message'Size / 8) & " bytes");
    end if;
  end Send;

end Connection;

