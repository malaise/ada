-- Usage: relay <channel>
-- The channel destinations file is <channel>.chn

with Ada.Text_Io, System;

with Text_Handler, Argument, Sys_Calls, Event_Mng,
     Socket, Tcp_Util, Channels, Async_Stdin;
procedure Relay is

  -- Message type
  Max_Data_Len : constant := 1024;
  subtype Data_Len_Range is Positive range 1 .. Max_Data_Len;
  subtype Data_Type is String (1 .. Max_Data_Len);
  type Message_Type is record
    Id : Socket.Host_Id;
    Data : Data_Type;
  end record;

  Len_Offset : constant Positive
             := (Message_Type'Size - Data_Type'Size) / System.Storage_Unit + 8;
  My_Host_Id : constant Socket.Host_Id := Socket.Local_Host_Id;

  Message : Message_Type;

  -- Name of the Channel
  Channel_Name : Text_Handler.Text(Tcp_Util.Max_Port_Name_Len);
  -- Suffix to build destinations file name
  File_Name_Suffix : constant String := ".chn";

  -- End of processing
  Done : Boolean := False;

  -- Are stdin and stdout a tty or not
  Stdin_Is_A_Tty : Boolean;
  Stdout_Is_A_Tty : Boolean;

  -- Sig callback
  procedure Sig_Callback is
  begin
    Done := True;
  end Sig_Callback;

  procedure Usage is
  begin
    Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                            & " <channel>");
    Sys_Calls.Set_Error_Exit_Code;
  end Usage;

  -- Subscriber reads with this
  procedure Channel_Read_Cb (Message  : in Message_Type;
                             Length   : in Channels.Message_Length;
                             Diffused : in Boolean) is
    Len : Channels.Message_Length;
    use type Socket.Host_Id;
  begin
    -- Discard own messages
    if Message.Id = My_Host_Id then
      return;
    end if;
    -- Get string len
    Len := Length - Len_Offset;
    -- Replace Cr by a New_Line
    if Len = 1 and then Message.Data(1) = Ascii.Cr then
      Len := 0;
    end if;
    -- Put
    if Stdout_Is_A_Tty then
      Ada.Text_Io.Put ("-> ");
    end if;
    Ada.Text_Io.Put_Line (Message.Data(1 .. Len));
    Ada.Text_Io.Flush;
  end Channel_Read_Cb;

  package My_Channel is new Channels.Channel ("dummy", Message_Type,
                                              Channel_Read_Cb);

  -- Sender may read stdin with this
  function Stdin_Cb (Str : in String) return Boolean is
    Len : Natural := Str'Length;
  begin
    if Len = 0 then
      Done := True;
      return True;
    end if;
    if Len >= 1 and then Str(Str'Length) = Ascii.Eot then
      Len := Len - 1;
      Done := True;
    end if;
    if Len > 1 and then (Str(Len) = Ascii.Lf
                 or else Str(Len) = Ascii.Cr) then
      Len := Len - 1;
    end if;
    if Len = 1 and then Str(Len) = Ascii.Lf then
      Message.Data(1) := Ascii.Cr;
    else
      Message.Data(1 .. Len) := Str (1 .. Len);
    end if;
    if Len > 0 then
      Message.Id := My_Host_Id;
      My_Channel.Write (Message, Len + Len_Offset);
    end if;
    return True;
  end Stdin_Cb;

  -- Read data from not a tty (file?)
  procedure Get_No_Tty is
    Len : Natural;
  begin
    begin
      Ada.Text_Io.Get_Line (Message.Data, Len);
    exception
      when Ada.Text_Io.End_Error =>
        Done := True;
        return;
    end;

    if Len = 0 then
      Message.Data(1) := Ascii.Cr;
      Len := 1;
    end if;
    Message.Id := My_Host_Id;
    My_Channel.Write (Message, Len + Len_Offset);
  end Get_No_Tty;

begin

  -- 1 argument
  if Argument.Get_Nbre_Arg /= 1 then
    Usage;
    return;
  end if;

  -- Store channel_name
  begin
    Argument.Get_Parameter (Channel_Name);
  exception
   when others =>
     Sys_Calls.Put_Line_Error ("Invalid channel_name "
                             & Argument.Get_Parameter);
     Usage;
     return;
  end;

  My_Channel.Change_Channel_Name (Text_Handler.Value(Channel_Name));
  My_Channel.Subscribe;
  begin
    My_Channel.Add_Destinations (Text_Handler.Value(Channel_Name) &
                                   File_Name_Suffix);
  exception
    when Channels.Unknown_Channel =>
      Sys_Calls.Put_Line_Error ("Unknown channel "
                              & Text_Handler.Value(Channel_Name));
      Sys_Calls.Set_Error_Exit_Code;
      return;
    when Channels.File_Error =>
      Sys_Calls.Put_Line_Error ("Error processing destinations file "
                              & Text_Handler.Value(Channel_Name)
                              & File_Name_Suffix);
      Sys_Calls.Set_Error_Exit_Code;
      return;
  end;
  declare
    use type Sys_Calls.File_Desc_Kind_List;
  begin
    Stdin_Is_A_Tty :=
        Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin) = Sys_Calls.Tty;
    Stdout_Is_A_Tty :=
        Sys_Calls.File_Desc_Kind (Sys_Calls.Stdout) = Sys_Calls.Tty;
  end;
  if Stdin_Is_A_Tty then
    Async_Stdin.Set_Async (Stdin_Cb'Unrestricted_Access, Max_Data_Len);
  else
    Event_Mng.Wait (500);
  end if;


  Event_Mng.Set_Sig_Term_Callback (Sig_Callback'Unrestricted_Access);

  -- Main loop
  loop
    if Stdin_Is_A_Tty then
      Event_Mng.Wait (-1);
    else
      Event_Mng.Wait (0);
      Get_No_Tty;
    end if;

    exit when Done;

  end loop;

  My_Channel.Del_All_Destinations;
  My_Channel.Unsubscribe;
  if Stdin_Is_A_Tty then
    Async_Stdin.Set_Async;
  end if;

end Relay;

