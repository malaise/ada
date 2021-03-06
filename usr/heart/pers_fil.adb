with Ada.Direct_Io;
with Pers_Def;
use Pers_Def;
package body Pers_Fil is

  -- Direct_io of persons
  package Person_Io is new Ada.Direct_Io (Element_Type => Pers_Def.Person_Rec);
  Person_File : Person_Io.File_Type;
  Person_File_Name : constant String := "PERSONS.LST";


  procedure Open is
  begin
    -- Try to open existing file
    begin
      Person_Io.Open (Person_File, Person_Io.Inout_File, Person_File_Name);
    exception
      when Person_Io.Name_Error =>
        Person_Io.Create (Person_File, Person_Io.Inout_File, Person_File_Name);
    end;
  exception
    when others =>
      raise Io_Error;
  end Open;

  -- Load the list from the file. (Erasing the current list)
  procedure Load is
    Person : Pers_Def.Person_Rec;
  begin

    -- Open file
    Open;
    -- Clear list
    The_Persons.Delete_List;

    -- Read persons from file and insert them in list
    while not Person_Io.End_Of_File (Person_File) loop
      Person_Io.Read (Person_File, Person);
      The_Persons.Insert (Person, Pers_Def.Person_List_Mng.Next);
    end loop;

    -- Close file
    Person_Io.Close (Person_File);
    -- Move to begining of list
    The_Persons.Rewind (Check_Empty => False);

  exception
    when Pers_Def.Person_List_Mng.Full_List =>
      raise Full_List_Error;
    when others =>
      raise Io_Error;
  end Load;


  -- Save the list to file. (List not affected)
  procedure Save is
    Person : Pers_Def.Person_Rec;
    List_Length : constant Natural := The_Persons.List_Length;
    Init_Pos    : Natural;
  begin

    -- Delete file and create a new empty one
    Open;
    Person_Io.Delete (Person_File);
    Open;

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
        Person_Io.Write (Person_File, Person);
      end loop;

      -- Move to initial position in list
      The_Persons.Move_At (Init_Pos);
    end if;

    -- Close file
    Person_Io.Close (Person_File);

  exception
    when Pers_Def.Person_List_Mng.Empty_List |
         Pers_Def.Person_List_Mng.Not_In_List =>
      raise Pers_Fil_Internal_Error;
    when others =>
      raise Io_Error;
  end Save;

end Pers_Fil;

