with Ada.Exceptions;
with As.U, Address_Ops, Socket, Channels, Mixed_Str, Text_Line;
with Dictio_Debug, Parse, Local_Host_Name;
pragma Elaborate_All (Channels);
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

  procedure Read_Cb (Message  : in Message_Rec;
                     Unused_Length   : in Channels.Message_Length;
                     Diffused : in Boolean);

  package Dictio_Channel is new Channels.Channel ("Dummy",
                                                  Message_Rec, Read_Cb);
  package Dictio_Bus is new Channels.Bus ("Dummy", "Dummy",
                                                  Message_Rec, Read_Cb);
  type Channel_Mode_List is new Args.Channel_Mode_List;
  Mode : Channel_Mode_List;

  Local_Name : Tcp_Util.Host_Name;

  procedure Init is
    use type Args.Channel_Mode_List;
  begin
    Local_Host_Name.Set(Socket.Local_Host_Name);
    Local_Name := As.U.Tus (Local_Host_Name.Get);

    Mode := Channel_Mode_List(Args.Get_Mode);

    if Mode = Channel then

      Dictio_Debug.Put (Dictio_Debug.Intra, "Init channel");
      begin
        Dictio_Channel.Change_Channel_Name (Args.Get_Name);
        Dictio_Channel.Subscribe;
      exception
        when Error: others =>
          Dictio_Debug.Put_Error (Dictio_Debug.Intra,
              "Cannot use channel " & Args.Get_Name
            & Lf & "Exception: " & Ada.Exceptions.Exception_Name(Error));
          Args.Usage;
      end;

      begin
        Dictio_Channel.Add_Destinations (Args.Get_Dest);
      exception
        when Error: others =>
          Dictio_Debug.Put_Error (Dictio_Debug.Intra,
              "Cannot set destinations of " & Args.Get_Name
            & Lf & "Exception: " & Ada.Exceptions.Exception_Name(Error));
          Args.Usage;
      end;

      begin
        Dictio_Channel.Del_Destination (Local_Name.Image);
      exception
        when Error: others =>
          Dictio_Debug.Put_Error (Dictio_Debug.Intra,
              "Cannot remove local host from destinations of " & Args.Get_Name
            & Lf & "Exception: " & Ada.Exceptions.Exception_Name(Error));
          Args.Usage;
      end;

    else

      Dictio_Debug.Put (Dictio_Debug.Intra, "Init bus");
      begin
        Dictio_Bus.Change_Names (Args.Get_Name, Args.Get_Dest);
      exception
        when Error: others =>
          Dictio_Debug.Put_Error (Dictio_Debug.Intra,
              "Cannot use bus " & Args.Get_Name & " with destination "
            & Args.Get_Dest
            & Lf & "Exception: " & Ada.Exceptions.Exception_Name(Error));
          Args.Usage;
      end;

      begin
        Dictio_Bus.Subscribe;
        Dictio_Bus.Join;
      exception
        when Error: others =>
          Dictio_Debug.Put_Error (Dictio_Debug.Intra,
              "Cannot subscribe or join bus " & Args.Get_Name
            & " with destination " & Args.Get_Dest
            & Lf & "Exception: " & Ada.Exceptions.Exception_Name(Error));
          Args.Usage;
      end;
    end if;
    Dictio_Debug.Put (Dictio_Debug.Intra, "Init succeeded");
  end Init;

  procedure Quit is
  begin
    Dictio_Debug.Put (Dictio_Debug.Intra, "Quit");
    if Mode = Channel then
      begin
        Dictio_Channel.Unsubscribe;
      exception
        when Channels.Not_Subscribed =>
          null;
      end;
      Dictio_Channel.Del_All_Destinations;
    else
      Dictio_Bus.Unsubscribe;
      Dictio_Bus.Leave;
    end if;
  end Quit;


  procedure Add_Host (Host : String) is
  begin
    if Mode = Bus then
      return;
    end if;
    Dictio_Channel.Add_Destination (Host);
    Dictio_Debug.Put (Dictio_Debug.Intra, "Add_Host: " & Host & " done");
  exception
    when Error : others =>
      Dictio_Debug.Put (Dictio_Debug.Intra, "Add_Host: " & Host
                                          & " not done cause "
                                       & Ada.Exceptions.Exception_Name(Error));
  end Add_Host;

  procedure Del_Host (Host : String) is
  begin
    if Mode = Bus then
      return;
    end if;
    Dictio_Channel.Del_Destination (Host);
    Dictio_Debug.Put (Dictio_Debug.Intra, "Del_Host: " & Host & " done");
  exception
    when Error : others =>
        Dictio_Debug.Put (Dictio_Debug.Intra, "Del_Host: " & Host
                                            & " not done cause "
                                       & Ada.Exceptions.Exception_Name(Error));
  end Del_Host;

  Client_Cb : Read_Cb_Access := null;

  procedure Set_Read_Cb (Read_Cb : Read_Cb_Access) is
  begin
    Client_Cb := Read_Cb;
  end Set_Read_Cb;

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
                     Unused_Length : in Channels.Message_Length;
                     Diffused      : in Boolean) is
  begin
    -- Discard own message
    if Mode = Bus and then Local_Name.Image = Parse (Message.Head.From) then
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
    Len : Natural;
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
      if Mode = Channel then
        Dictio_Channel.Write (Message, Len);
      else
        Dictio_Bus.Write (Message, Len);
      end if;
      Result := Ok;
    elsif To = "" then
      begin
        if Mode = Channel then
          Dictio_Channel.Reply (Message, Len);
        else
          Dictio_Bus.Reply (Message, Len);
        end if;
        Result := Ok;
      exception
        when Channels.Reply_Overflow =>
          -- Channel only
          Result := Overflow;
        when Channels.Reply_Failed =>
          Result := Error;
      end;
    else
      begin
        if Mode = Channel then
          Dictio_Channel.Send (To, Message, Len);
        else
          Dictio_Bus.Send (To, Message, Len);
        end if;
        Result := Ok;
      exception
        when Channels.Send_Overflow =>
          -- Channel only
          Result := Overflow;
        when Channels.Send_Failed =>
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

