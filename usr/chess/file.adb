with Ada.Text_Io, Ada.Direct_Io;
with Text_Handler, Get_Line;

with Space;

package body File is

  File_Name_Txt : Text_Handler.Text (Max_File_Name_Len);

  -- Have we finished reading
  type State_List is (Closed, Reading, Empty, Writting);
  State : State_List := Closed;

  package Chess_Io is new Ada.Direct_Io (Character);

  The_File : Chess_Io.File_Type;
  Move_Num : Natural;
  Overwrite : Boolean;

  package My_Get_Line is new Get_Line (
      Max_Word_Len => Image.Move_Str'Length,
      Max_Word_Nb  => 2,
      Max_Line_Len => 132,
      Comment      => '#');
  Line : My_Get_Line.Line_Txt;
  Line_Array : My_Get_Line.Line_Array;
  Line_No : Ada.Text_Io.Count;

  -- Delete a file if it exists
  procedure Delete (File_Name : in String) is
  begin
    if State /= Closed then
      raise File_Error;
    end if;
    Chess_Io.Open (The_File, Chess_Io.In_File, File_Name);
    Chess_Io.Delete (The_File);
  exception
    when Chess_Io.Name_Error =>
      null;
  end Delete;
 
  -- Open / Create a file for reading then saving actions
  -- File_Error if file exists and cannot be open
  -- or file cannot be created
  procedure Open (File_Name : in String) is
  begin
    if State /= Closed then
      raise File_Error;
    end if;
    -- Try to open
    begin
      My_Get_Line.Open (File_Name);
      Line_No := 0;
      State := Reading;
    exception
      when Ada.Text_Io.Name_Error =>
        Chess_Io.Create (The_File, Chess_Io.Out_File, File_Name);
        State := Empty;
        Overwrite := False;
      when My_Get_Line.Line_Too_Long =>
        Ada.Text_Io.Put_Line ("Format error at beginning of file");
        raise Format_Error;
    end;
    Move_Num := 0;
    Text_Handler.Set (File_Name_Txt, File_Name);
  exception
    when others =>
      raise File_Error;
  end Open;

  -- Log a format error
  procedure Log_Error (Read : in Boolean) is
  begin
    if not Read then
      -- Error during reading
      Ada.Text_Io.Put_Line ("Format error after line" & Ada.Text_Io.Count'Image (Line_No));
    else
      -- Line has been read
      Ada.Text_Io.Put_Line ("Format error at line" & Ada.Text_Io.Count'Image (Line_No)
         & ": " & Text_Handler.Value(Line));
    end if;
  end Log_Error;

  -- Read one movement, returns not valid at end of file
  function Read_One return Players.Action_Rec is
    Str : Image.Move_Str;
    Word_Num : My_Get_Line.Word_Range;
    Color  : Space.Color_List;
    Action : Game.Valid_Action_Rec;
    Result : Game.Move_Status_List;

  begin

    -- Read a new line each 2 moves (except first) and parse it
    if Move_Num rem 2 = 0 then
      if Move_Num /= 0 then
        begin
          My_Get_Line.Read_Next_Line;
          Line_No := My_Get_Line.Get_Line_No;
          My_Get_Line.Get_Whole_Line (Line);
        exception
          when My_Get_Line.No_More_Line =>
            return (Valid => False);
          when My_Get_Line.Line_Too_Long =>
            Log_Error (False);
            raise Format_Error;
        end;
      end if;

      -- Get 1 or 2 words
      if My_Get_Line.Get_Word_Number = 1 then
        begin
          -- There should not be any more line
          My_Get_Line.Read_Next_Line;
          Log_Error (True);
          raise Format_Error;
        exception
          when My_Get_Line.No_More_Line =>
            null;
        end;
      elsif My_Get_Line.Get_Word_Number /= 2 then
        Log_Error (True);
        raise Format_Error;
      end if;

      My_Get_Line.Get_Words (Line_Array);
      Word_Num := 1;
      Color := Space.White;
    else
      Word_Num := 2;
      Color := Space.Black;
      if  My_Get_Line.Get_Word_Number = 1 then
        return (Valid => False);
      end if;
    end if;

    -- Parse then word
    begin
      Str := (others => ' ');
      Str(1 .. Text_Handler.Length(Line_Array(Word_Num))) := 
        Text_Handler.Value(Line_Array(Word_Num));
    exception
      when others =>
        Log_Error (True);
        raise Format_Error;
    end;

    -- Convert
    begin
      Image.Move_Value (Str, Color, Action, Result);
    exception
      when Image.Value_Error=>
        Log_Error (True);
        raise Format_Error;
    end;

    return Action;

  exception
    when My_Get_Line.Too_Many_Words | My_Get_Line.Word_Too_Long =>
      Log_Error (True);
      raise Format_Error;
  end;
     

  -- Read next move (white then black then white...)
  -- Returns a not valid action at end of file
  -- File_error on IO error;
  -- Value_Error if decoding of an action fails
  function Read return Players.Action_Rec is
    Action : Players.Action_Rec;
  begin
    if State = Reading then
      Action := Read_One;
      if not Action.Valid then
        -- End of read
        My_Get_Line.Close;
        Chess_Io.Open (The_File, Chess_Io.InOut_File,
                          Text_Handler.Value(File_Name_Txt));
        -- Ready to Overwrite last Lf
        Chess_Io.Set_Index (The_File, Chess_Io.Size(The_File));
        Overwrite := True;
        State := Writting;
        return (Valid => False);
      else
        Move_Num := Move_Num + 1;
        return Action;
      end if;
    elsif State = Empty then
      State := Writting;
      return (Valid => False);
    else
      raise File_Error;
    end if;
  exception
    when Format_Error =>
      raise;
    when others =>
      raise File_Error;
  end Read;

  -- After all movements have been read (otherwise File_Error)
  -- Append new movement
  procedure Write (Action : in Game.Valid_Action_Rec;
                   Result : in Game.Move_Status_List) is
  begin
    if State /= Writting then
      raise File_Error;
    end if;
    declare
      Str : constant Image.Move_Str := Image.Move_Image(Action, Result);
    begin
      if Overwrite then
        if Move_Num rem 2 = 1 then
          Chess_Io.Write (The_File, ' ');
        end if;
        Overwrite := False;
      end if;
      for I in Str'Range loop
        -- First write to the file: overwrite last Lf
        Chess_Io.Write (The_File, Str(I));
      end loop;
    end;
    Move_Num := Move_Num + 1;
    if Move_Num rem 2 = 0 then
      Chess_Io.Write (The_File, Ascii.Lf);
    else
      Chess_Io.Write (The_File, ' ');
    end if;
  exception
    when others =>
      raise File_Error;
  end Write;

  -- At the end
  procedure Close is
  begin
    if State = Closed then
      raise File_Error;
    end if;
    Chess_Io.Write (The_file, Ascii.Lf);
    Chess_Io.Close (The_File);
    State := Closed;
  exception
    when others =>
      raise File_Error;
  end Close;

end File;
 
