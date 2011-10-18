with Ada.Text_Io, Ada.Direct_Io;

with Normal, Argument;

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
  A_File : Ada.Text_Io.File_Type;
  Char : Character;

  Dat_To_Asc : Boolean;

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
    Ada.Text_Io.Put_Line ("Wrong arg");
    return;
  end if;
  if Argument.Get_Parameter = "asc2dat" then
    Dat_To_Asc := False;
  elsif Argument.Get_Parameter = "dat2asc" then
    Dat_To_Asc := True;
  else
    Ada.Text_Io.Put_Line ("Wrong arg");
    return;
  end if;

  if Dat_To_Asc then
    D.Open (D_File, D.In_File, D_File_Name);
    begin
      Ada.Text_Io.Open (A_File, Ada.Text_Io.Out_File, A_File_Name);
      Ada.Text_Io.Put_Line ("File exists");
      return;
    exception
      when Ada.Text_Io.Name_Error =>
        Ada.Text_Io.Create (A_File, Ada.Text_Io.Out_File, A_File_Name);
    end;



    for F in Sok_Types.Frame_Range loop
      D.Read (D_File, File_Frame);
      From_File_To_Frame (File_Frame, Frame);
      for R in Sok_Types.Row_Range loop
        for C in Sok_Types.Col_Range loop
          case Frame(R, C).Pattern is
            when Sok_Types.Wall =>
              Ada.Text_Io.Put (A_File, 'w');
            when Free =>
              case Frame(R, C).Content is
                when Sok_Types.Man =>
                  Ada.Text_Io.Put (A_File, 'm');
                when Sok_Types.Box =>
                  Ada.Text_Io.Put (A_File, 'b');
                when Sok_Types.Nothing =>
                  Ada.Text_Io.Put (A_File, 'n');
              end case;
            when Target =>
              case Frame(R, C).Content is
                when Sok_Types.Man =>
                  Ada.Text_Io.Put (A_File, 'M');
                when Sok_Types.Box =>
                  Ada.Text_Io.Put (A_File, 'B');
                when Sok_Types.Nothing =>
                  Ada.Text_Io.Put (A_File, 'N');
              end case;
          end case;
        end loop;
      end loop;
    end loop;
  else

    Ada.Text_Io.Open (A_File, Ada.Text_Io.In_File, A_File_Name);
    begin
      D.Open (D_File, D.Out_File, D_File_Name);
      Ada.Text_Io.Put_Line ("File exists");
      return;
    exception
      when D.Name_Error =>
        D.Create (D_File, D.Out_File, D_File_Name);
    end;

    for F in Sok_Types.Frame_Range loop
      for R in Sok_Types.Row_Range loop
        for C in Sok_Types.Col_Range loop
          Ada.Text_Io.Get (A_File, Char);
          case Char is
            when 'w' =>
              Frame(R, C) := (Pattern => Sok_Types.Wall);
            when 'm' =>
              Frame(R, C) := (Pattern => Sok_Types.Free, Content => Sok_Types.Man);
            when 'b' =>
              Frame(R, C) := (Pattern => Sok_Types.Free, Content => Sok_Types.Box);
            when 'n' =>
              Frame(R, C) := (Pattern => Sok_Types.Free, Content => Sok_Types.Nothing);
            when 'M' =>
              Frame(R, C) := (Pattern => Sok_Types.Target, Content => Sok_Types.Man);
            when 'B' =>
              Frame(R, C) := (Pattern => Sok_Types.Target, Content => Sok_Types.Box);
            when 'N' =>
              Frame(R, C) := (Pattern => Sok_Types.Target, Content => Sok_Types.Nothing);
            when others =>
              Ada.Text_Io.Put_Line ("Invalid character");
          end case;
        end loop;
      end loop;
      From_Frame_To_File (Frame, File_Frame);
      D.Write (D_File, File_Frame);
    end loop;

  end if;

  Ada.Text_Io.Close (A_File);
  D.Close (D_File);
  Ada.Text_Io.Put_Line ("Done");

end Trans;

