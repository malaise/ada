with Ada.Exceptions, System;
with Sys_Calls, Address_Ops, Socket, Tcp_Util, Channels;
with Args, Debug, Parse, Local_Host_Name, Status, Errors;
package body Intra_Dictio is

  Byte_Size : constant := System.Storage_Unit;

  type Header_Rec is record
    From : Tcp_Util.Host_Name;
    Kind : Character;
    Stat : Status.Status_List;
    Sync : Boolean;
  end record;

  type Message_Rec is record
    Head : Header_Rec;
    Item : Data_Base.Item_Rec;
  end record;

  procedure Read_Cb (Message  : in Message_Rec;
                     Length   : in Channels.Message_Length;
                     Diffused : in Boolean);

  package Dictio_Channel is new Channels.Channel ("Dummy",
                                                  Message_Rec, Read_Cb);

  procedure Init is
  begin
    Local_Host_Name.Set(Socket.Local_Host_Name);

    begin
      Dictio_Channel.Change_Channel_Name (Args.Get_Channel_Name);
      Dictio_Channel.Subscribe;
    exception
      when Error: others =>
        Debug.Put_Error ("Cannot use channel "
                       & Args.Get_Channel_Name);
        Debug.Put_Error ("Exception: "
                       & Ada.Exceptions.Exception_Name(Error));
        Args.Usage;
    end;

    begin
      Dictio_Channel.Add_Destinations (Args.Get_Dest_File);
      Dictio_Channel.Del_Destination (Local_Host_Name.Get);
    exception
      when Error: others =>
        Debug.Put_Error ("Cannot set destinations on "
                       & Args.Get_Channel_Name);
        Debug.Put_Error ("Exception: "
                       & Ada.Exceptions.Exception_Name(Error));
        Args.Usage;
    end;
    if Debug.Level_Array(Debug.Intra) then
      Debug.Put ("Intra: init succeeded");
    end if;
  end Init;

  procedure Quit is
  begin
    if Debug.Level_Array(Debug.Intra) then
      Debug.Put ("Intra: quit");
    end if;
    begin
      Dictio_Channel.Unsubscribe;
    exception
      when Channels.Not_Subscribed =>
        null;
    end;
    Dictio_Channel.Del_All_Destinations;
  end Quit;
    

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

  procedure Read_Cb (Message  : in Message_Rec;
                     Length   : in Channels.Message_Length;
                     Diffused : in Boolean) is
  begin
    if Debug.Level_Array(Debug.Intra) then
      Debug.Put ("Intra: receive Kind: " & Kind_Image(Message.Head.Kind)
               & "  Stat: " & Message.Head.Stat'Img
               & "  From: " & Parse (Message.Head.From));
    end if;
    -- Call dispatcher
    if Client_Cb /= null then
      Client_Cb (Diffused, Message.Head.Stat, 
                           Message.Head.Sync,
                           Message.Head.From,
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
      Message.Head.Stat := Status.Get;
    end if;
    Message.Head.Sync := Status.Sync;
    Local_Host_Name.Get (Message.Head.From);
     
    if Message.Item = Data_Base.No_Item then
      -- Header size
      Len := 80;
    else
      Len := Integer(Message.Item.Data(Message.Item.Data_Len)'Address
                     - Message'Address + 4);
    end if;
    if Debug.Level_Array(Debug.Intra) then
      if To = "*" then
        Debug.Put ("Intra: bcast Kind: " & Kind_Image(Message.Head.Kind)
                 & "  Stat: " & Message.Head.Stat'Img
                 & "  Len: " & Len'Img);
      elsif To = "" then
        Debug.Put ("Intra: reply Kind: " & Kind_Image(Message.Head.Kind)
                 & "  Stat: " & Message.Head.Stat'Img
                 & "  Len: " & Len'Img);
      else
        Debug.Put ("Intra: send to: " & To
                 & "  Kind: " & Kind_Image(Message.Head.Kind)
                 & "  Stat: " & Message.Head.Stat'Img
                 & "  Len: " & Len'Img);
      end if;
    end if;
    if To = "*" then
      Dictio_Channel.Write (Message, Len);
      Result := Ok;
    elsif To = "" then
      begin
        Dictio_Channel.Reply (Message, Len);
      exception
        when Channels.Reply_Overflow =>
          Result := Overflow;
        when Channels.Reply_Failed =>
          Result := Error;
      end;
    else
      begin
        Dictio_Channel.Send (To, Message, Len);
        Result := Ok;
      exception
        when Channels.Send_Overflow =>
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

  procedure Send_Status (Stat : in Status.Status_List) is
    Msg : Message_Rec;
  begin
    Msg.Head.Kind := Stat_Kind;
    Msg.Head.Stat := Stat;
    Msg.Item.Data_Len := 0;
    Send ("*", Msg, get_Status => False);
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
    Send (To, Msg, Result);
    if Debug.Level_Array(Debug.Intra) and then Result /= Ok then
      Debug.Put ("Intra: sync reply failed on " & Result'Img);
    end if;
    return Result;
  end Send_Sync_Data;

end Intra_Dictio;

