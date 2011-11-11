with Ada.Direct_Io;
with Argument, Text_Line, Basic_Proc, As.U;

with Sok_Types;
use Sok_Types;

procedure Trans is

  Frame : Sok_Types.Frame_Tab;

  type File_Frame_Rec is record
    Pattern : Sok_Types.Pattern_List;
    Content : Sok_Types.Content_List;
  end record;
  type File_Frame_Tab is array (Sok_Types.Row_Range, Sok_Types.Col_Range)
    of File_Frame_Rec;
  File_Frame : File_Frame_Tab;
  package D is new Ada.Direct_Io (File_Frame_Tab);
  D_File_Name : constant String := "SOKOBAN.DAT";
  D_File : D.File_Type;


  A_File_Name : constant String := "SOKOBAN.ASC";
  A_File : Text_Line.File_Type;
  Char : Character;

  Dat_To_Asc : Boolean;

  Buff : As.U.Asu_Us;
  End_Error : exception;
  procedure Get (C : out Character) is
  begin
    if Buff.Is_Null then
      Buff := A_File.Get;
      if Buff.Is_Null then
        raise End_Error;
      end if;
    end if;
    C := Buff.Element (1);
    Buff.Delete (1, 1);
  end Get;

  -- to convert from a frame to a frame on file
  procedure From_Frame_To_File (Frame : in  Sok_Types.Frame_Tab;
                                File  : out File_Frame_Tab) is
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

  -- to convert from a frame on file to a frame
  procedure From_File_To_Frame (File  : in  File_Frame_Tab;
                                Frame : out Sok_Types.Frame_Tab) is
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


begin -- trans

  if Argument.Get_Nbre_Arg /= 1 then
    Basic_Proc.Put_Line_Error ("Wrong arg");
    return;
  end if;
  if Argument.Get_Parameter = "asc2dat" then
    Dat_To_Asc := False;
  elsif Argument.Get_Parameter = "dat2asc" then
    Dat_To_Asc := True;
  else
    Basic_Proc.Put_Line_Error ("Wrong arg");
    return;
  end if;

  if Dat_To_Asc then
    D.Open (D_File, D.In_File, D_File_Name);
    begin
      A_File.Open_All (Text_Line.Out_File, A_File_Name);
      Basic_Proc.Put_Line_Error ("File exists");
      return;
    exception
      when Text_Line.Name_Error =>
        A_File.Create_All (A_File_Name);
    end;

    for F in Sok_Types.Frame_Range loop
      D.Read (D_File, File_Frame);
      From_File_To_Frame (File_Frame, Frame);
      for R in Sok_Types.Row_Range loop
        for C in Sok_Types.Col_Range loop
          case Frame(R, C).Pattern is
            when Sok_Types.Wall =>
              A_File.Put ('w' & "");
            when Free =>
              case Frame(R, C).Content is
                when Sok_Types.Man =>
                  A_File.Put ('m' & "");
                when Sok_Types.Box =>
                  A_File.Put ('b' & "");
                when Sok_Types.Nothing =>
                  A_File.Put ('n' & "");
              end case;
            when Target =>
              case Frame(R, C).Content is
                when Sok_Types.Man =>
                  A_File.Put ('M' & "");
                when Sok_Types.Box =>
                  A_File.Put ('B' & "");
                when Sok_Types.Nothing =>
                  A_File.Put ('N' & "");
              end case;
          end case;
        end loop;
      end loop;
    end loop;
  else

    A_File.Open_All (Text_Line.In_File, A_File_Name);
    begin
      D.Open (D_File, D.Out_File, D_File_Name);
      Basic_Proc.Put_Line_Error ("File exists");
      return;
    exception
      when D.Name_Error =>
        D.Create (D_File, D.Out_File, D_File_Name);
    end;

    for F in Sok_Types.Frame_Range loop
      for R in Sok_Types.Row_Range loop
        for C in Sok_Types.Col_Range loop
          Get (Char);
          case Char is
            when 'w' =>
              Frame(R, C) := (Pattern => Sok_Types.Wall);
            when 'm' =>
              Frame(R, C) := (Pattern => Sok_Types.Free,
                              Content => Sok_Types.Man);
            when 'b' =>
              Frame(R, C) := (Pattern => Sok_Types.Free,
                              Content => Sok_Types.Box);
            when 'n' =>
              Frame(R, C) := (Pattern => Sok_Types.Free,
                              Content => Sok_Types.Nothing);
            when 'M' =>
              Frame(R, C) := (Pattern => Sok_Types.Target,
                              Content => Sok_Types.Man);
            when 'B' =>
              Frame(R, C) := (Pattern => Sok_Types.Target,
                              Content => Sok_Types.Box);
            when 'N' =>
              Frame(R, C) := (Pattern => Sok_Types.Target,
                              Content => Sok_Types.Nothing);
            when others =>
              Basic_Proc.Put_Line_Error ("Invalid character");
          end case;
        end loop;
      end loop;
      From_Frame_To_File (Frame, File_Frame);
      D.Write (D_File, File_Frame);
    end loop;

  end if;

  A_File.Close_All;
  D.Close (D_File);
  Basic_Proc.Put_Line_Output ("Done");

end Trans;

