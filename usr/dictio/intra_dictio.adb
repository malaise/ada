with Ada.Exceptions;
with As.U, Address_Ops, Socket, Autobus, Mixed_Str, Text_Line;
with Dictio_Debug, Parse, Local_Host_Name;
package body Intra_Dictio is

  Lf : constant Character := Text_Line.Line_Feed_Char;

  type Header_Rec is record
    Stat : Character;
    Sync : Character;
    From : Local_Host_Name.Host_Name;
    Kind : Character;
    Prio : Args.Prio_Str;
  end record;

  type Message_Rec is record
    Head : Header_Rec;
    Item : Data_Base.Item_Rec;
  end record;

  Bus : aliased Autobus.Bus_Type;
  Local_Name : Tcp_Util.Host_Name;

  -- Call bac to invoke on message reception
  Client_Cb : Read_Cb_Access := null;
  procedure Set_Read_Cb (Read_Cb : Read_Cb_Access) is
  begin
    Client_Cb := Read_Cb;
  end Set_Read_Cb;

  -- Observer receiver of messages
  Subscriber : aliased Autobus.Subscriber_Type;
  type Observer_Type is new Autobus.Observer_Type with null record;
  procedure Receive (Unused_Observer   : in out Observer_Type;
                     Unused_Subscriber : in Autobus.Subscriber_Access_Type;
                     Message           : in String) is
  begin
    if Client_Cb = null then
      return;
    end if;
    -- @@@ parse and call Client_Cb
  end Receive;
  Receiver : aliased Observer_Type;


  procedure Init is
  begin
    Local_Host_Name.Set(Socket.Local_Host_Name);
    Local_Name := As.U.Tus (Local_Host_Name.Get);

    Dictio_Debug.Put (Dictio_Debug.Intra, "Init bus");
    begin
      Bus.Init (Args.Get_Bus, Autobus.Multicast);
    exception
      when Error: others =>
        Dictio_Debug.Put_Error (Dictio_Debug.Intra,
            "Cannot init bus " & Args.Get_Bus
          & Lf & "Exception: " & Ada.Exceptions.Exception_Name(Error));
        Args.Usage;
    end;

    Subscriber.Init (Bus'Access, Receiver'Access);

    Dictio_Debug.Put (Dictio_Debug.Intra, "Init succeeded");
  end Init;

  procedure Quit is
  begin
    Dictio_Debug.Put (Dictio_Debug.Intra, "Quit");
    Bus.Reset;
  end Quit;


  function Kind_Image (C : Character) return String is
  begin
    if C = Stat_Kind then
      return "Stat";
    elsif C = Data_Kind then
      return "Data";
    elsif C = Sync_Kind then
      return "Sync";
    else
      return "Unkn";
    end if;
  end Kind_Image;

  function Stat_Image (C : Character) return String is
    Stat : Status.Status_List;
  begin
    Stat := Status.Status_List'Val(Character'Pos(C));
    return Mixed_Str (Stat'Img);
  end Stat_Image;

  function Sync_Image (C : Character) return String is
  begin
    if Character'Pos (C) = Boolean'Pos (True) then
      return "True";
    else
      return "False";
    end if;
  end Sync_Image;

  procedure Read_Cb (Message       : in Message_Rec;
                     Unused_Length : in Natural;
                     Diffused      : in Boolean) is
  begin
    -- Discard own message
    if Local_Name.Image = Parse (Message.Head.From) then
      return;
    end if;
    Dictio_Debug.Put (Dictio_Debug.Intra,
               "Receive Kind: " & Kind_Image (Message.Head.Kind)
             & "  Diff: " & Mixed_Str (Diffused'Img)
             & "  Stat: " & Stat_Image (Message.Head.Stat)
             & "  Sync: " & Sync_Image (Message.Head.Sync)
             & "  Prio: " & Message.Head.Prio
             & "  From: " & Parse (Message.Head.From));
    -- Call dispatcher
    if Client_Cb /= null then
      Client_Cb (Diffused,
                 Status.Status_List'Val(Character'Pos(Message.Head.Stat)),
                 Boolean'Val(Character'Pos(Message.Head.Sync)),
                 Message.Head.Prio,
                 As.U.Tus (Parse (Message.Head.From)),
                 Message.Head.Kind,
                 Message.Item);
    end if;
  end Read_Cb;

  procedure Send (To      : in String;
                  Message : in out Message_Rec;
                  Result  : out Reply_Result_List;
                  Get_Status : in Boolean := True) is
    -- @@@
    Msg : String (1 .. Autobus.Message_Max_Length);
    Len : Natural := 0;
    use Address_Ops;
    use type Data_Base.Item_Rec;
  begin
    if Get_Status then
      Message.Head.Stat := Character'Val(Status.Status_List'Pos(Status.Get));
    end if;
    Message.Head.Sync := Character'Val(Boolean'Pos(Status.Sync));
    Message.Head.Prio := Args.Get_Prio;
    Local_Host_Name.Get (Message.Head.From);

    if Message.Item.Data_Len = 0 then
      -- Item without data
      Len := Integer(Message.Item.Data(1)'Address
                     - Message'Address + 4 - 1);
    else
      Len := Integer(Message.Item.Data(Message.Item.Data_Len)'Address
                     - Message'Address + 4);
    end if;
    if To = "*" then
      Dictio_Debug.Put (Dictio_Debug.Intra, "Bcast Kind: "
                 & Kind_Image(Message.Head.Kind)
                 & "  Stat: " & Stat_Image(Message.Head.Stat)
                 & "  Len: " & Len'Img);
    elsif To = "" then
      Dictio_Debug.Put (Dictio_Debug.Intra, "Reply Kind: "
                 & Kind_Image(Message.Head.Kind)
                 & "  Stat: " & Stat_Image(Message.Head.Stat)
                 & "  Len: " & Len'Img);
    else
      Dictio_Debug.Put (Dictio_Debug.Intra, "Send to: " & To
                 & "  Kind: " & Kind_Image(Message.Head.Kind)
                 & "  Stat: " & Stat_Image(Message.Head.Stat)
                 & "  Len: " & Len'Img);
    end if;

    Result := Error;
    if To = "*" then
      Bus.Send (Msg (1 .. Len));
      Result := Ok;
    elsif To = "" then
      begin
        Bus.Reply (Msg (1 .. Len));
        Result := Ok;
      exception
        when others =>
          Result := Error;
      end;
    else
      begin
        -- @@@ To
        Bus.Send_To (To, Msg (1 .. Len));
        Result := Ok;
      exception
        when others =>
          Result := Error;
      end;
    end if;
  end Send;

  procedure Send (To : in String;
                  Message : in out Message_Rec;
                  Get_Status : in Boolean := True) is
    Result : Reply_Result_List;
  begin
    Send (To, Message, Result, Get_Status);
  end Send;

  procedure Send_Status (Extra : in String := "") is
    Msg : Message_Rec;
  begin
    Msg.Head.Kind := Stat_Kind;
    if Extra = "" then
      Msg.Item.Data_Len := 0;
    else
      Msg.Item.Data_Len := Extra'Length;
      Msg.Item.Data (1 .. Msg.Item.Data_Len) := Extra;
    end if;
    Send ("*", Msg);
  end Send_Status;

  procedure Send_Status (Stat  : in Status.Status_List;
                         Extra : in String := "") is
    Msg : Message_Rec;
  begin
    Msg.Head.Kind := Stat_Kind;
    Msg.Head.Stat := Character'Val(Status.Status_List'Pos(Stat));
    if Extra = "" then
      Msg.Item.Data_Len := 0;
    else
      Msg.Item.Data_Len := Extra'Length;
      Msg.Item.Data (1 .. Msg.Item.Data_Len) := Extra;
    end if;
    Send ("*", Msg, Get_Status => False);
  end Send_Status;


  procedure Reply_Status (Extra : in String := "") is
    Msg : Message_Rec;
  begin
    Msg.Head.Kind := Stat_Kind;
    if Extra = "" then
      Msg.Item.Data_Len := 0;
    else
      Msg.Item.Data_Len := Extra'Length;
      Msg.Item.Data (1 .. Msg.Item.Data_Len) := Extra;
    end if;
    Send ("", Msg);
  end Reply_Status;

  procedure Send_Data (Item : in Data_Base.Item_Rec) is
    Msg : Message_Rec;
  begin
    Msg.Head.Kind := Data_Kind;
    Msg.Item := Item;
    Send ("*", Msg);
  end Send_Data;

  function Send_Sync_Data (To : in String;
                           Item : in Data_Base.Item_Rec)
           return Reply_Result_List is
    Msg : Message_Rec;
    Result : Reply_Result_List;
  begin
    Msg.Head.Kind := Sync_Kind;
    Msg.Item := Item;
    if To /= "" then
      Send (To, Msg, Result);
    else
      Send ("*", Msg, Result);
    end if;
    Dictio_Debug.Put (Dictio_Debug.Intra, "Sync reply failed on " & Result'Img);
    return Result;
  end Send_Sync_Data;

  function Extra_Of (Str : String; Key : Character) return String is
    First : Natural;
  begin
    First := 0;
    for I in Str'Range loop
      if Str(I) = Key and then I /= Str'Last then
        First := I + 1;
        exit;
      end if;
    end loop;

    if First = 0 then
      return "";
    end if;

    for I in First + 1 .. Str'Last loop
      if Str(I) not in '0' .. '9' then
        return Str(First .. I - 1);
      end if;
    end loop;
    return Str(First .. Str'Last);
  end Extra_Of;

end Intra_Dictio;

