with Ada.Exceptions;
with As.U, Socket, Autobus, Mixed_Str, Text_Line, Normal;
with Dictio_Debug, Parse, Local_Host_Name;
package body Intra_Dictio is

  Lf : constant Character := Text_Line.Line_Feed_Char;

  Bus : aliased Autobus.Bus_Type;

  Msg_Max_Len : constant Positive := 367;

  -- Call back to invoke on message reception
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
                     Message           : in String);
  Receiver : aliased Observer_Type;


  procedure Init is
  begin
    Local_Host_Name.Set(Socket.Local_Host_Name);

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

  function Sync_Image (Sync : Boolean) return String is
  begin
    return Mixed_Str (Sync'Img);
  end Sync_Image;


  procedure Receive (Unused_Observer   : in out Observer_Type;
                     Unused_Subscriber : in Autobus.Subscriber_Access_Type;
                     Message           : in String) is
    Msg : String (1 .. Msg_Max_Len);
    Diff : Boolean;
    Stat : Status.Status_List;
    Sync : Boolean;
    Prio : Args.Prio_Str;
    From : Tcp_Util.Host_Name;
    Kind : Character;
    Item : Data_Base.Item_Rec;
    Step : As.U.Asu_Us;
    Invalid_Data : exception;
  begin
    if Client_Cb = null then
      return;
    end if;
    -- Copy Message -> 1 .. Len
    if Message'Length > Msg'Length then
      Dictio_Debug.Put_Error (Dictio_Debug.Intra, "Message too long: >"
                            & Message & "<");
      return;
    end if;
    Msg (1 .. Message'Length) := Message;

    -- Parse Message
    Step := As.U.Tus ("Diff");
    case Msg(1) is
      when 'M' => Diff := True;
      when 'R' => Diff := False;
      when 'T' => Diff := False;
      when others => raise Invalid_Data;
    end case;
    Step := As.U.Tus ("Stat");
    Stat := Status.Status_List'Val (Character'Pos (Msg(2)));
    Step := As.U.Tus ("Sync");
    Sync := Boolean'Val (Character'Pos (Msg(3)));
    Step := As.U.Tus ("Prio");
    Prio := Msg(4 .. 6);
    Step := As.U.Tus ("From");
    From := As.U.Tus (Parse (Msg (7 .. 70)));
    Step := As.U.Tus ("Kind");
    Kind := Msg(71);
    if Message'Length > 71 then
      Step := As.U.Tus ("Item len");
      Item.Data_Len := Integer'Value (Msg(72 .. 74));
      Step := As.U.Tus ("Item name");
      Item.Name := Msg(75 .. 106);
      Step := As.U.Tus ("Item kind");
      Item.Kind := Msg(107 .. 107);
      Step := As.U.Tus ("Item CRC");
      Item.Crc := Msg(108 .. 111);
      Step := As.U.Tus ("Item data");
      Item.Data(1 .. Item.Data_Len) := Msg(112 .. 112 + Item.Data_Len - 1);
   end if;

    Dictio_Debug.Put (Dictio_Debug.Intra,
               "Receive Kind: " & Kind_Image (Kind)
             & "  Diff: " & Mixed_Str (Diff'Img)
             & "  Stat: " & Mixed_Str (Stat'Img)
             & "  Sync: " & Mixed_Str (Sync'Img)
             & "  Prio: " & Prio
             & "  From: " & From.Image);
    -- Call Client_Cb
    Step := As.U.Tus ("Call_Cb data");
    Client_Cb (Diff, Stat, Sync, Prio, From, Kind, Item);
  exception
    when Constraint_Error | Invalid_Data =>
      Dictio_Debug.Put_Error (Dictio_Debug.Intra, "Invalid_Message: >"
                            & Message & "<. Error decoding " & Step.Image);
  end Receive;

  type Header_Rec is record
    Stat : Character := '?';
    Sync : Character := '?';
    Prio : Args.Prio_Str := "999";
    From : Local_Host_Name.Host_Name := (others => '?');
    Kind : Character := '?';
  end record;

  type Message_Rec is record
    Head : Header_Rec;
    Item : Data_Base.Item_Rec;
  end record;

  procedure Send (To      : in String;
                  Message : in out Message_Rec;
                  Result  : out Reply_Result_List;
                  Get_Status : in Boolean := True) is
    Msg : String(1 .. Msg_Max_Len);
    Len : Natural := 0;
    use type Data_Base.Item_Rec;
  begin
    if Get_Status then
      Message.Head.Stat := Character'Val(Status.Status_List'Pos(Status.Get));
    end if;
    Msg(2) := Message.Head.Stat;
    Msg(3) := Character'Val(Boolean'Pos(Status.Sync));
    Msg(4 .. 6) := Args.Get_Prio;
    Local_Host_Name.Get (Msg(7 ..  70));
    Msg(71) := Message.Head.Kind;

    if Message.Item.Data_Len = 0 then
      -- No item
      Len := 71;
    else
      -- An item
      Msg(72 .. 74) := Normal (Message.Item.Data_Len, 3, Gap => '0');
      Msg(75 .. 106) := Message.Item.Name;
      Msg(107 .. 107) := Message.Item.Kind;
      Msg(108 .. 111) := Message.Item.Crc;
      Msg(112 .. 112 + Message.Item.Data_Len - 1) :=
          Message.Item.Data(1 .. Message.Item.Data_Len);
      Len := 111 + Message.Item.Data_Len;
    end if;
    Dictio_Debug.Put (Dictio_Debug.Intra,
                      (if To = "*" then "Bcast"
                       elsif To = "" then  "Reply"
                       else "Send to: " & To)
                 & "  Kind: " & Kind_Image(Message.Head.Kind)
                 & "  Stat: " & Stat_Image(Message.Head.Stat)
                 & "  Sync: " & Sync_Image(Status.Sync)
                 & "  Len: "  & Len'Img);

    if To = "*" then
      Msg(1) := 'M';
      Bus.Send (Msg (1 .. Len));
    elsif To = "" then
      Msg(1) := 'R';
      Bus.Reply (Msg (1 .. Len));
    else
      Msg(1) := 'T';
      Bus.Send_To (To & ":", Msg (1 .. Len));
    end if;
    Result := Ok;
  exception
    when others =>
      Result := Error;
      Dictio_Debug.Put_Error (Dictio_Debug.Intra,
                              "Exception when sending message");
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
    if Result /= Ok then
      Dictio_Debug.Put (Dictio_Debug.Intra,
                        "Sync reply failed on " & Result'Img);
    end if;
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

