with Ada.Direct_Io, Ada.Exceptions;
with Pers_Def, Str_Mng, Basic_Proc, Text_Line, Sys_Calls;
use Pers_Def;
package body Pers_Fil is

  -- Direct_io of persons (old format)
  package Person_Io is new Ada.Direct_Io (Element_Type => Pers_Def.Person_Rec);
  Person_File : Person_Io.File_Type;
  Person_File_Name : constant String := "PERSONS.LST";

  -- 20 for name, 10 for activity, 3 for Pid, 6x3 for Bmps, 3 for Sampling
  subtype File_Txt is String (1 .. 54);
  Txt_File : Text_Line.File_Type;

  procedure Open (Create : in Boolean) is
  begin
    if Create then
      -- Try to open or create text file
      begin
        Txt_File.Open_All (Text_Line.Out_File, Person_File_Name,
                           Text_Line.Trunc);
      exception
        when Text_Line.Name_Error =>
          Txt_File.Create_All (Person_File_Name);
      end;
      return;
    end if;

    -- Try to open existing Txt file for reading
    begin
      Txt_File.Open_All (Text_Line.In_File, Person_File_Name);
      declare
        Str : constant String := Text_Line.Trim (Txt_File.Get);
      begin
        if Str'Length = File_Txt'Length
        and then (Str(31) = '0' or else Str(31) = '1') then
          -- Looks as the start of a Pid
          Txt_File.Close_All;
          Txt_File.Open_All (Text_Line.In_File, Person_File_Name);
          return;
        end if;
        Txt_File.Close_All;
      exception
        when others =>
          Txt_File.Close_All;
      end;
    exception
      when others =>
        null;
    end;

    -- Try to open existing binary file
    Person_Io.Open (Person_File, Person_Io.Inout_File, Person_File_Name);
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception on Open "
          & Ada.Exceptions.Exception_Name (Error) & " " & Person_File_Name);
      raise Io_Error;
  end Open;

  procedure Close is
  begin
    if Person_Io.Is_Open (Person_File) then
      Person_Io.Close (Person_File);
    else
      Txt_File.Close_All;
    end if;
  end Close;

  -- Load the list from the file. (Erasing the current list)
  procedure Load is
    Person : Pers_Def.Person_Rec;
    Txt : File_Txt;
    Start : Positive;
  begin

    -- Open file
    Open (False);
    -- Clear list
    The_Persons.Delete_List;

    -- Read persons from file and insert them in list
    loop
      if Person_Io.Is_Open (Person_File) then
        exit when Person_Io.End_Of_File (Person_File);
        Person_Io.Read (Person_File, Person);
      else
        declare
          Str : constant String := Txt_File.Get;
        begin
          exit when Str = "";
          Txt := Text_Line.Trim (Str);
        end;
        Person.Name := Txt (1 .. 20);
        Person.Activity := Txt (21 .. 30);
        Person.Pid := Pers_Def.Pid_Range'Value (Txt (31 .. 33));
        Start := 34;
        for I in Pers_Def.Person_Tz_Array'Range loop
          Person.Tz(I) := Str_Mng.To_Bpm (Txt(Start .. Start + 2));
          Start := Start + 3;
        end loop;
        Person.Sampling_Delta := Str_Mng.To_Sampling (Txt(Start .. Start + 2));
      end if;
      The_Persons.Insert (Person, Pers_Def.Person_List_Mng.Next);
    end loop;

    -- Close file
    Close;

    -- Move to begining of list
    The_Persons.Rewind (Check_Empty => False);

  exception
    when Pers_Def.Person_List_Mng.Full_List =>
      raise Full_List_Error;
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception on Load "
          & Ada.Exceptions.Exception_Name (Error) & " " & Person_File_Name);
      raise Io_Error;
  end Load;

  -- Save the list to file. (List not affected)
  procedure Save is
    Person : Pers_Def.Person_Rec;
    List_Length : constant Natural := The_Persons.List_Length;
    Init_Pos    : Natural;
    Txt : File_Txt;
    Start : Positive;
  begin

    -- Delete file and create a new empty one
    Sys_Calls.Unlink (Person_File_Name);
    Open (True);

    -- Scan list only if not empty
    if List_Length /= 0 then
      -- Save current position
      Init_Pos := The_Persons.Get_Position;
      -- Move to beginning of list
      The_Persons.Rewind;

      -- Read persons from list and write them to file
      for I in 1 .. List_Length loop
        if I /= List_Length then
          The_Persons.Read (Person);
        else
          -- Do not move after reading last person
          The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
        end if;

        Txt (1 .. 20) := Person.Name;
        Txt (21 .. 30) := Person.Activity;
        Txt (31 .. 33) := Str_Mng.Pid_Str (Person.Pid);
        Start := 34;
        for I in Pers_Def.Person_Tz_Array'Range loop
          Txt(Start .. Start + 2) := Str_Mng.To_Str (Person.Tz(I));
          Start := Start + 3;
        end loop;
        Txt(Start .. Start + 2) := Str_Mng.To_Str (Person.Sampling_Delta);
        Txt_File.Put_Line (Txt);
      end loop;

      -- Move to initial position in list
      The_Persons.Move_At (Init_Pos);
    end if;

    -- Close file
    Close;

  exception
    when Pers_Def.Person_List_Mng.Empty_List |
         Pers_Def.Person_List_Mng.Not_In_List =>
      raise Pers_Fil_Internal_Error;
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception on Save "
          & Ada.Exceptions.Exception_Name (Error) & " " & Person_File_Name);
      raise Io_Error;
  end Save;

end Pers_Fil;

