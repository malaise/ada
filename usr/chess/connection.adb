with Ada.Text_Io;
with Udp, X_Mng, Debug;
package body Connection is

  Server : Boolean := True;
  Own_Color : Space.Color_List := Space.White;

  Soc : Udp.Socket_Dscr;
  Fd  : X_Mng.File_Desc;
  Server_Port_Num : constant := 50000;
  Client_Host : Udp.Host_Id;
  Client_Port : Udp.Port_Num;

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

  Action_Set : Boolean := False;
  Action_To_Get : Players.Action_Rec;

  -- Has an action been received and get it
  function Action_Received return Boolean is
  begin
    return Action_Set;
  end;

  function Receive return Players.Action_Rec is
  begin
    if not Action_Set then
      raise No_Action;
    end if;
    Action_Set := False;
    return Action_To_Get;
  end Receive;

  -- Init completed?
  We_Are_Ready : Boolean := False;

  -- Have we received a move (to discard instead of reject
  --  client init retries
  We_Have_Moved : Boolean := False;

  procedure Chess_Read is new Udp.Receive (Message_Type);
  procedure Chess_Send is new Udp.Send    (Message_Type);

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

  procedure Call_Back (Fd : in X_Mng.File_Desc) is
    Message : Message_Type;
    Len : Natural;
    Receive : Boolean;
    use type Space.Color_List;
    use type Udp.Host_Id, Udp.Port_Num;
  begin
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put ("In callback : ");
    end if;
    Chess_Read (Soc, Message, Len, Receive, Server);
    if not Receive then
      if Debug.Get (Debug.Connection) then
        Ada.Text_Io.Put_Line ("No message");
      end if;
      return;
    else
      if Debug.Get (Debug.Connection) then
        Ada.Text_Io.Put_Line ("Message read " & Integer'Image (Len) & " bytes");
      end if;
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
                Client_Host := Udp.Get_Destination_Host (Soc);
                Client_Port := Udp.Get_Destination_Port (Soc);
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
          if      Udp.Get_Destination_Host (Soc) /= Client_Host
          or else Udp.Get_Destination_Port (Soc) /= Client_Port then
            -- New client
            if Message.Kind = Init then
              Chess_Send (Soc, (Error, Busy));
            else
              Chess_Send (Soc, (Error, Protocol));
            end if;
            -- Restore
            Udp.Set_Destination_Host_And_Port (Soc, Client_Host, Client_Port);
            return;
          elsif Message.Kind /= Move then
            if We_Have_Moved then
              -- Not a retry
              Chess_Send (Soc, (Error, Protocol));
            end if;
            return;
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
        if Action_Set then
          -- Oooops, overwriting an action!!!
          Ada.Text_Io.Put_Line ("In callback : Overwrting action");
          raise Protocol_Error;
        end if;
        Action_To_Get := Message.Action;
        Action_Set := True;
        We_Have_Moved := True;

    end case;

    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("In callback : End");
    end if;
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
    Udp.Open (Soc);
    Fd := Udp.Fd_Of (Soc);
    X_Mng.X_Add_Callback (Fd, Call_Back'Access);
    if Server then
      Udp.Link_Port (Soc, Server_Port_Num);
    else
      Udp.Link_Dynamic (Soc);
      Udp.Set_Destination_Name_And_Port (Soc,
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
    X_Mng.X_Del_Callback (Fd);
    Udp.Close (Soc);
  end Close;

  procedure Wait_Ready is
  begin
    if Debug.Get (Debug.Connection) then
      Ada.Text_Io.Put_Line ("Connection: waiting");
    end if;
    loop
      if not Server then
        -- Client sends its color to server
        Chess_Send (Soc, (Init, Own_Color));
        X_Mng.Select_No_X (100);
      else
        -- Server waits
        X_Mng.Select_No_X (100);
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

