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
  -- Last move num read/written
  Move_Num : Natural;
  -- Have we written
  Written : Boolean; 

  package My_Get_Line is new Get_Line (
      Max_Word_Len => Image.Move_Str'Length,
      Max_Word_Nb  => 2,
      Max_Line_Len => 132,
      Comment      => '#');
  Line : My_Get_Line.Line_Txt;
  Line_Array : My_Get_Line.Line_Array;
  Line_No : Ada.Text_Io.Count;
  End_Reached : Boolean;

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
      End_Reached := False;
      State := Reading;
    exception
      when Ada.Text_Io.Name_Error =>
        Chess_Io.Create (The_File, Chess_Io.Out_File, File_Name);
        State := Empty;
      when My_Get_Line.No_More_Line =>
        My_Get_Line.Close;
        Chess_Io.Open (The_File, Chess_Io.Out_File, File_Name);
        State := Empty;
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
  procedure Log_Error (Read : in Boolean; Msg : in String := "") is
  begin
    if not Read then
      -- Error during reading
      Ada.Text_Io.Put_Line ("Format error after line" & Ada.Text_Io.Count'Image (Line_No));
    else
      -- Line has been read
      Ada.Text_Io.Put_Line ("Format error at line" & Ada.Text_Io.Count'Image (Line_No)
         & ": " & Text_Handler.Value(Line));
    end if;
    if Msg /= "" then
       Ada.Text_Io.Put_Line (Msg & '.');
    end if;
  end Log_Error;

  -- Read one movement, returns not valid at end of file
  procedure Read_One (Action : out Players.Action_Rec) is
    Str : Image.Move_Str;
    Word_Num : My_Get_Line.Word_Range;
    Color  : Space.Color_List;
    Result : Game.Move_Status_List;
    Check_Eof : Boolean;

  begin
    Action := (Valid => False);
    if End_Reached then
      return;
    end if;

    -- Read a new line each 2 moves (except first) and parse it
    if Move_Num rem 2 = 0 then
      if Move_Num /= 0 then
        begin
          My_Get_Line.Read_Next_Line;
        exception
          when My_Get_Line.No_More_Line =>
            Action := (Valid => False);
            return;
          when My_Get_Line.Line_Too_Long =>
            Log_Error (False, "Line too long");
            raise Format_Error;
        end;
      end if;
      Line_No := My_Get_Line.Get_Line_No;
      My_Get_Line.Get_Whole_Line (Line);

      -- Get 1 or 2 words
      Check_Eof := False;
      if My_Get_Line.Get_Word_Number = 1 then
        Check_Eof := True;
      elsif My_Get_Line.Get_Word_Number /= 2 then
        Log_Error (True, "There should be one or two words");
        raise Format_Error;
      end if;

      My_Get_Line.Get_Words (Line_Array);
      Word_Num := 1;
      Color := Space.White;
    else
      Check_Eof := False;
      Word_Num := 2;
      Color := Space.Black;
      if  My_Get_Line.Get_Word_Number = 1 then
        Action := (Valid => False);
        return;
      end if;
    end if;

    -- Parse the word
    begin
      Str := (others => ' ');
      Str(1 .. Text_Handler.Length(Line_Array(Word_Num))) := 
        Text_Handler.Value(Line_Array(Word_Num));
    exception
      when others =>
        Log_Error (True, "Wrong word value");
        raise Format_Error;
    end;

    -- Convert
    -- Bug in Gnat? Constraint_Error is raised at call if Action is not valid
    --  here.
    --  This was not the case when kind was not an arg of Move_Value.
    declare
      Tmp_Action : Players.Valid_Action_Rec;
    begin
      Image.Move_Value (Str, Color, Tmp_Action, Result);
      Action := Tmp_Action;
    exception
      when Image.Value_Error =>
        Log_Error (True, "Invalid format");
        raise Format_Error;
    end;

    if Check_Eof then
      begin
        -- There should not be any more line
        My_Get_Line.Read_Next_Line;
        Log_Error (True, "End of file expected");
        raise Format_Error;
      exception
        when My_Get_Line.No_More_Line =>
          End_Reached := True;
      end;
    end if;

  exception
    when My_Get_Line.Too_Many_Words =>
      Log_Error (True, "Too many words");
      raise Format_Error;
    when  My_Get_Line.Word_Too_Long =>
      Log_Error (True, "Word too long");
      raise Format_Error;
  end Read_One;


  -- Prepare file for append
  -- Move to first (if white) or second (if black)
  -- of last sequence of Lf
  procedure Prepare_To_Append is
    Index : Chess_Io.Count;
    Char : Character;
    use type Chess_Io.Count;
  begin
    -- Last char is necessary a Lf
    Index := Chess_Io.Size(The_File);
    if Index = 0 then
      Chess_Io.Write (The_File, Ascii.Lf, 1);
      Index := 1;
    end if;

    -- Locate First Lf of the Lf sequence
    if Index = 1 then
      Chess_Io.Set_Index (The_File, Index);
    else
      Index := Index - 1;
      while Index > 1 loop
        Chess_Io.Read (The_File, Char, Index);
        if Char /= Ascii.Lf then
          exit;
        end if;
        Index := Index - 1;
      end loop;
    end if;

    -- File index is now set to first Lf
    if Move_Num /= 0 and then Move_Num rem 2 = 0 then
      -- White move to write. Leave this Lf.
      Chess_Io.Read (The_File, Char);
    elsif Move_Num rem 2 /= 0 then
      if Char /= ' ' then
        -- We have white move then Lf
        Chess_Io.Write (The_File, ' ');
      end if;
    end if;
    Written := False;
  end Prepare_To_Append;
     

  -- Read next move (white then black then white...)
  -- Returns a not valid action at end of file
  -- File_error on IO error;
  -- Value_Error if decoding of an action fails
  function Read return Players.Action_Rec is
    Action : Players.Action_Rec;
  begin
    Action := (Valid => False);
    if State = Reading then
      Read_One (Action);
      if not Action.Valid then
        -- End of read
        My_Get_Line.Close;
        Chess_Io.Open (The_File, Chess_Io.InOut_File,
                          Text_Handler.Value(File_Name_Txt));
        Prepare_To_Append;
        State := Writting;
        return (Valid => False);
      else
        -- Ok
        Move_Num := Move_Num + 1;
        return Action;
      end if;
    elsif State = Empty then
      Prepare_To_Append;
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
  procedure Write (Action : in Players.Action_Rec;
                   Result : in Game.Move_Status_List) is
  begin
    if State /= Writting then
      raise File_Error;
    end if;
    if Action.Valid then
      declare
        Str : constant Image.Move_Str := Image.Move_Image(Action, Result);
      begin
        for I in Str'Range loop
          Chess_Io.Write (The_File, Str(I));
        end loop;
      end;
    else
      declare
        Str : constant Image.Move_Str := "   ----   ";
      begin
        for I in Str'Range loop
          Chess_Io.Write (The_File, Str(I));
        end loop;
      end;
    end if;
    Move_Num := Move_Num + 1;
    if Move_Num rem 2 = 0 then
      Chess_Io.Write (The_File, Ascii.Lf);
    else
      Chess_Io.Write (The_File, ' ');
    end if;
    Written := True;
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
    if Written then
      Chess_Io.Write (The_file, Ascii.Lf);
    end if;
    begin
      My_Get_Line.Close;
    exception
      when others =>
        null;
    end;
    begin
      Chess_Io.Close (The_File);
    exception
      when others =>
        null;
    end;
    State := Closed;
  exception
    when others =>
      raise File_Error;
  end Close;

end File;
 
