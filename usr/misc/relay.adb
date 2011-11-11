-- Relay stdin/stdout <-> a channel
-- Usage: relay <channel>
-- The channel destinations file is <channel>.chn

with System;
with Ada.Characters.Latin_1;
with As.U, Argument, Sys_Calls, Event_Mng, Socket, Channels, Async_Stdin,
     String_Mng, Basic_Proc;
procedure Relay is

  -- Message type
  Max_Data_Len : constant := 1024;
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
  Channel_Name : As.U.Asu_Us;
  -- Suffix to build destinations file name
  File_Name_Suffix : constant String := ".xml";

  -- End of processing
  Done : Boolean := False;

  -- Are stdin and stdout a tty or not
  Stdin_Is_A_Tty : Boolean := False;
  Stdout_Is_A_Tty : Boolean := False;

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
    pragma Unreferenced (Diffused);
    Len : Channels.Message_Length;
    use type Socket.Host_Id;
  begin
    -- Discard own messages
    if Message.Id = My_Host_Id then
      -- Normally, this should not occure because local host name
      --  has been deleted from destinations, but does it have the
      --  same name in the destination list file?
      return;
    end if;
    -- Get string len
    Len := Length - Len_Offset;
    -- Replace Cr by a New_Line
    if Len = 1 and then Message.Data(1) = Ada.Characters.Latin_1.Cr then
      Len := 0;
    end if;
    -- Put
    if Stdout_Is_A_Tty then
      Async_Stdin.Put_Out ("-> ");
    end if;
    Async_Stdin.Put_Line_Out (Message.Data(1 .. Len));
    Basic_Proc.Flush_Output;
  end Channel_Read_Cb;

  package My_Channel is new Channels.Channel ("dummy", Message_Type,
                                              Channel_Read_Cb);

  -- Sender may read stdin with this
  function Stdin_Cb (Str : in String) return Boolean is
    Len : Natural := Str'Length;
    Last : Natural := Str'Last;
  begin
    if Len = 0 then
      Done := True;
      return True;
    end if;
    if Len >= 1 and then Str(Last) = Ada.Characters.Latin_1.Eot then
      Len := Len - 1;
      Last := Last - 1;
      Done := True;
    end if;
    if Len > 1 and then (Str(Last) = Ada.Characters.Latin_1.Lf
                 or else Str(Last) = Ada.Characters.Latin_1.Cr) then
      Len := Len - 1;
      Last := Last - 1;
    end if;
    if Len = 1 and then Str(Last) = Ada.Characters.Latin_1.Lf then
      Message.Data(Message.Data'First) := Ada.Characters.Latin_1.Cr;
    else
      String_Mng.Copy (Str (Str'First .. Last), Message.Data);
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
    Basic_Proc.Get_Line (Message.Data, Len);
    if Len = 0 then
      Message.Data(1) := Ada.Characters.Latin_1.Cr;
      Len := 1;
    end if;
    Message.Id := My_Host_Id;
    My_Channel.Write (Message, Len + Len_Offset);
  exception
    when Basic_Proc.End_Error =>
      Done := True;
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

  -- Subscribe and add destinations from file except ourself
  My_Channel.Change_Channel_Name (Channel_Name.Image);
  My_Channel.Subscribe;
  begin
    My_Channel.Add_Destinations (Channel_Name.Image & File_Name_Suffix);
  exception
    when Channels.Unknown_Channel =>
      Sys_Calls.Put_Line_Error ("Unknown channel " & Channel_Name.Image);
      Sys_Calls.Set_Error_Exit_Code;
      return;
    when Channels.File_Error =>
      Sys_Calls.Put_Line_Error ("Error processing destinations file "
                              & Channel_Name.Image
                              & File_Name_Suffix);
      Sys_Calls.Set_Error_Exit_Code;
      return;
  end;
  begin
    My_Channel.Del_Destination(Socket.Local_Host_Name);
  exception
    when Channels.Unknown_Destination =>
      -- Local host is not a destination of this destinations file
      null;
  end;

  -- Initialize ttys
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

  -- Hook Cb for sig term
  Event_Mng.Set_Sig_Term_Callback (Sig_Callback'Unrestricted_Access);

  -- Wait a little bit for some connections to establish
  if not Stdin_Is_A_Tty then
    Event_Mng.Pause (100);
  end if;

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

  -- Cleanup / reset before exit
  My_Channel.Del_All_Destinations;
  My_Channel.Unsubscribe;
  if Stdin_Is_A_Tty then
    Async_Stdin.Set_Async;
  end if;

exception
  when others =>
    if Stdin_Is_A_Tty then
      Async_Stdin.Set_Async;
    end if;
    raise;
end Relay;

