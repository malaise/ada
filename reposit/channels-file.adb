with Get_Line, Lower_Str;

separate (Channels)

package body File is

  package Channel_File is new Get_Line (
     Max_Word_Len => Tcp_Util.Max_Host_Name_Len,
     Max_Word_Nb  => 60,
     Max_Line_Len => 132,
     Comment      => "#");

  type Section_List is (Out_Channel, In_Channel, In_Matching_Channel);

  Channel_Name : Text_Handler.Text (Tcp_Util.Max_Port_Name_Len);
  Section : Section_List;
  Curr_Word : Channel_File.Word_Count;
  Line : Channel_File.Line_Array;

  procedure Open (File_Name : in String; Channel_Name : in String) is
  begin
    begin
      Text_Handler.Set (File.Channel_Name, Channel_Name);
    exception
      when others =>
        raise Name_Too_Long;
    end;
    begin
      Channel_File.Open (File_Name);
    exception
       when Channel_File.No_More_Line =>
         null;
       when others =>
        raise File_Error;
    end;
    Curr_Word := 0;
    Section := Out_Channel;
  end Open;

  procedure Close is
  begin
    Channel_File.Close;
    Text_Handler.Empty (Channel_Name);
  exception
     when others =>
      raise File_Error;
  end Close;

  function Next_Host return Tcp_Util.Remote_Host is
    Host : Tcp_Util.Remote_Host;
  begin
    loop
      -- Load a new line?
      if Curr_Word = Channel_File.Get_Word_Number then
        begin
          Channel_File.Read_Next_Line;
        exception
          when Channel_File.No_More_Line =>
            raise End_Error;
        end;
        Curr_Word := 0;
      end if;

      if Curr_Word = 0 then
        Channel_File.Get_Words (Line);

        if Lower_Str (Channel_File.Get_First_Word) = "channel" then
          -- Channel { <channel_name> }
          if Section /= Out_Channel
          or else Channel_File.Get_Word_Number < 2 then
            raise File_Error;
          end if;
          -- Locate Channel_Name in list
          Section := In_Channel;
          for I in 2 .. Channel_File.Get_Word_Number loop
            if Text_Handler."=" (Channel_Name, Line(I)) then
              Section := in_Matching_Channel;
              exit;
            end if;
          end loop;
          Curr_Word := Channel_File.Get_Word_Number;
        elsif Lower_Str (Channel_File.Get_First_Word) = "end_channel" then
          if Section = Out_Channel
          or else Channel_File.Get_Word_Number /= 1 then
            raise File_Error;
          end if;
          Section := Out_Channel;
          Curr_Word := 1;
        elsif Lower_Str (Channel_File.Get_First_Word) = "host" then
          case Section is
            when Out_Channel =>
              raise File_Error;
            when In_Channel =>
              -- Skip
              Curr_Word := Channel_File.Get_Word_Number;
            when In_Matching_Channel =>
              Curr_Word := 1;
          end case;
        else
          raise File_Error;
        end if;
      else
        Curr_Word := Curr_Word + 1;
        if Section = in_Matching_Channel then
          Host.Name(1 .. Text_Handler.Length (Line(Curr_Word)) ) :=
                  Text_Handler.Value (Line(Curr_Word));
          exit;
        else
          -- Unexpected. Bug in algo.
          raise File_Error;
        end if;
      end if;

    end loop;

    return Host;

  exception
    when End_Error =>
      raise;
    when others =>
      Section := Out_Channel;
      Curr_Word := Channel_File.Get_Word_Number;
      raise File_Error; 
  end Next_Host;


end File;

