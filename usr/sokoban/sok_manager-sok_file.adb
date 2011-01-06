with Ada.Direct_Io, Ada.Sequential_Io;
with As.B, Sys_Calls, Directory;

separate (Sok_Manager)
-- Sokoban frames reading.
package body Sok_File is

  -- frame as it is on disk : no mutant
  type File_Square_Rec is record
    Pattern : Sok_Types.Pattern_List;
    Content : Sok_Types.Content_List;
  end record;
  type File_Frame_Tab is array (Sok_Types.Row_Range, Sok_Types.Col_Range)
   of File_Square_Rec;

  -- internal state of a frame
  type File_State_Rec is record
    Dur          : Ada.Calendar.Day_Duration;
    Day          : Natural;
    Frame        : File_Frame_Tab;
    No_Frame     : Sok_Types.Frame_Range;
    Position     : Sok_Types.Coordinate_Rec;
    Nbre_Targets : Natural;
    Box_Ok       : Natural;
    Moves        : Natural;
    Pushes       : Natural;
  end record;

  -- for read frame
  package Sok_File_Mng is new Ada.Direct_Io (File_Frame_Tab);


  -- for save and restore sate
  package Sok_State_Mng is new Ada.Sequential_Io (File_State_Rec);
  -- for save and restore saved movements
  package Sok_Saved_Mng is new Ada.Sequential_Io (Sok_Movement.Saved_Data_Rec);

  -- for save and restore scores
  package Sok_Score_Mng is new Ada.Direct_Io (Sok_Types.Score_Rec);

  -- for setting frame file directory
  Sok_File_Dir_Env_Name : constant String := "SOKOBAN_DIR";
  Sok_File_Dir : As.B.Asb_Bs(Directory.Max_Dir_Name_Len + 1);

  -- for read frame
  Sok_File_Name : constant String := "SOKOBAN.DAT";

  -- for save and restore frame and movements
  Sok_State_Name : constant String := "STATE.DAT";
  Sok_Saved_Name : constant String := "SAVED.DAT";

  -- for scores
  Sok_Score_Name : constant String := "SCORES.DAT";

  -- to convert from a frame on file to a frame
  procedure From_File_To_Frame (File  : in  File_Frame_Tab;
                                Frame : out Sok_Types.Frame_Tab) is
    use Sok_Types;
  begin
    for I in Sok_Types.Row_Range loop
      for J in Sok_Types.Col_Range loop
        case File(I,J).Pattern is
          when Sok_Types.Wall =>
            Frame(I,J) := (Pattern => Sok_Types.Wall);
          when Sok_Types.Free =>
            Frame(I,J) := (Pattern => Sok_Types.Free,
                           Content => File(I, J).Content);
          when Sok_Types.Target =>
            Frame(I,J) := (Pattern => Sok_Types.Target,
                           Content => File(I, J).Content);

        end case;
      end loop;
    end loop;
  end From_File_To_Frame;

  -- to convert from a frame to a frame on file
  procedure From_Frame_To_File (Frame : in  Sok_Types.Frame_Tab;
                                File  : out File_Frame_Tab) is
    use Sok_Types;
  begin
    for I in Sok_Types.Row_Range loop
      for J in Sok_Types.Col_Range loop
        case Frame(I,J).Pattern is
          when Sok_Types.Wall =>
            File(I,J) := (Pattern => Sok_Types.Wall,
                          Content => Sok_Types.Nothing);
          when Sok_Types.Free =>
            File(I,J) := (Pattern => Sok_Types.Free,
                          Content => Frame(I,J).Content);
          when Sok_Types.Target =>
            File(I,J) := (Pattern => Sok_Types.Target,
                          Content => Frame(I,J).Content);

        end case;
      end loop;
    end loop;
  end From_Frame_To_File;


  -- to read a new frame
  procedure Read (No_Frame : in  Sok_Types.Frame_Range;
                  Frame    : out Sok_Types.Frame_Tab) is
    Sok_File : Sok_File_Mng.File_Type;
    File_Frame : File_Frame_Tab;
  begin
    if Sok_File_Dir.Is_Null then
      declare
        Env_Set   : Boolean;
        Env_Trunc : Boolean;
        Env_Value : String(1 .. Directory.Max_Dir_Name_Len);
        Env_Len   : Natural;
      begin
        Sys_Calls.Getenv (Sok_File_Dir_Env_Name, Env_Set, Env_Trunc,
                          Env_Value, Env_Len);
        if Env_Set and then not Env_Trunc then
          Sok_File_Dir.Set (Env_Value(1 .. Env_Len));
        else
          Sok_File_Dir.Set (".");
        end if;
        Sok_File_Dir.Append ("/");
      end;
    end if;

    begin
      Sok_File_Mng.Open (Sok_File, Sok_File_Mng.In_File,
        Sok_File_Dir.Image & Sok_File_Name);
    exception
      when Sok_File_Mng.Name_Error =>
        raise Data_File_Not_Found;
    end;

    begin
      Sok_File_Mng.Read (Sok_File, File_Frame, Sok_File_Mng.Count(No_Frame));
      From_File_To_Frame (File_Frame, Frame);
    exception
      when others =>
        begin
          Sok_File_Mng.Close (Sok_File);
        exception
          when others => null;
        end;
        raise Error_Reading_Data;
    end;

    begin
      Sok_File_Mng.Close (Sok_File);
    exception
      when others => null;
    end;
  end Read;

  -- closes frame and saved file
  procedure Close_Files (
   Sok_State_File : in out Sok_State_Mng.File_Type;
   Sok_Saved_File : in out Sok_Saved_Mng.File_Type) is
  begin
    begin
      -- close state file
      Sok_State_Mng.Close (Sok_State_File);
    exception
      when others => null;
    end;

    -- close saved file
    begin
      Sok_Saved_Mng.Close (Sok_Saved_File);
    exception
      when others => null;
    end;
  end Close_Files;

  --save a frame with saved movements
  procedure Save (State : in State_Rec) is
    Sok_State_File : Sok_State_Mng.File_Type;
    Sok_Saved_File : Sok_Saved_Mng.File_Type;
    File_State : File_State_Rec;
  begin
    -- be sure that there is no file
    begin
      Sok_State_Mng.Open (Sok_State_File, Sok_State_Mng.In_File,
       Sok_State_Name);
      Sok_State_Mng.Delete (Sok_State_File);
    exception
      when Sok_State_Mng.Name_Error => null;
    end;
    begin
      Sok_Saved_Mng.Open (Sok_Saved_File, Sok_Saved_Mng.In_File,
       Sok_Saved_Name);
      Sok_Saved_Mng.Delete (Sok_Saved_File);
    exception
      when Sok_Saved_Mng.Name_Error => null;
    end;

    -- now create new files
    Sok_State_Mng.Create (Sok_State_File, Sok_State_Mng.Out_File,
     Sok_State_Name);
    Sok_Saved_Mng.Create (Sok_Saved_File, Sok_Saved_Mng.Out_File,
     Sok_Saved_Name);

    -- fill state to be saved
    Sok_Time.Get_Time (File_State.Day, File_State.Dur);
    From_Frame_To_File (State.Frame, File_State.Frame);
    File_State.No_Frame     := State.No_Frame;
    File_State.Position     := State.Position;
    File_State.Nbre_Targets := State.Nbre_Targets;
    File_State.Box_Ok       := State.Box_Ok;
    File_State.Moves        := State.Moves;
    File_State.Pushes       := State.Pushes;

    -- save state
    Sok_State_Mng.Write (Sok_State_File, File_State);

    -- save saved movements
    begin
      Sok_Saved_Mng.Write (Sok_Saved_File, Sok_Save.Look (Sok_Save.First));
      loop
        Sok_Saved_Mng.Write (Sok_Saved_File, Sok_Save.Look (Sok_Save.Next));
      end loop;
    exception
      when Sok_Save.No_More_Saved_Movements => null;
    end;

    Close_Files (Sok_State_File, Sok_Saved_File);
  exception
    when others =>
      Close_Files (Sok_State_File, Sok_Saved_File);
      raise Error_Writing_Frame;
  end Save;

  --restore a frame without saved movements
  procedure Restore (State : out State_Rec) is
    Sok_State_File : Sok_State_Mng.File_Type;
    Sok_Saved_File : Sok_Saved_Mng.File_Type;
    File_State : File_State_Rec;
  begin
    begin
      Sok_State_Mng.Open (Sok_State_File, Sok_State_Mng.In_File,
       Sok_State_Name);
      Sok_Saved_Mng.Open (Sok_Saved_File, Sok_Saved_Mng.In_File,
       Sok_Saved_Name);
    exception
      when -- Sok_State_Mng.Name_Error |
           Sok_Saved_Mng.Name_Error =>
        raise Frame_File_Not_Found;
    end;

    -- read state
    Sok_State_Mng.Read (Sok_State_File, File_State);

    -- fill returned state
    Sok_Time.Set_Time (File_State.Day, File_State.Dur);
    From_File_To_Frame (File_State.Frame, State.Frame);
    State.No_Frame     := File_State.No_Frame;
    State.Position     := File_State.Position;
    State.Nbre_Targets := File_State.Nbre_Targets;
    State.Box_Ok       := File_State.Box_Ok;
    State.Moves        := File_State.Moves;
    State.Pushes       := File_State.Pushes;

    -- read saved movements
    Sok_Save.Reset;
    loop
      declare
        Saved_Movement : Sok_Movement.Saved_Data_Rec;
      begin
        Sok_Saved_Mng.Read (Sok_Saved_File, Saved_Movement);
        Sok_Save.Push (Saved_Movement);
      exception
        when Sok_Saved_Mng.End_Error => exit;
      end;
    end loop;
    Close_Files (Sok_State_File, Sok_Saved_File);
  exception
    when Frame_File_Not_Found =>
      Close_Files (Sok_State_File, Sok_Saved_File);
      raise;
    when others =>
      Close_Files (Sok_State_File, Sok_Saved_File);
      raise Error_Reading_Frame;
  end Restore;

  function Read_Score (No : Sok_Types.Frame_Range) return Sok_Types.Score_Rec is
    Sok_Score_File : Sok_Score_Mng.File_Type;
    Score : Sok_Types.Score_Rec;
  begin
    Sok_Score_Mng.Open (Sok_Score_File, Sok_Score_Mng.In_File,
     Sok_Score_Name);
    Sok_Score_Mng.Read (Sok_Score_File, Score,
      Sok_Score_Mng.Positive_Count(No));
    Sok_Score_Mng.Close (Sok_Score_File);
    return Score;
  exception
    when others =>
      raise Score_Io_Error;
  end Read_Score;

  procedure Write_Score (No : in Sok_Types.Frame_Range;
                         Score : in Sok_Types.Score_Rec) is
    Sok_Score_File : Sok_Score_Mng.File_Type;
  begin
    Sok_Score_Mng.Open (Sok_Score_File, Sok_Score_Mng.Out_File,
     Sok_Score_Name);
    Sok_Score_Mng.Write (Sok_Score_File, Score,
      Sok_Score_Mng.Positive_Count(No));
    Sok_Score_Mng.Close (Sok_Score_File);
  exception
    when others =>
      raise Score_Io_Error;
  end Write_Score;

end Sok_File;

