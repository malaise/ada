with Ada.Text_Io, Ada.Sequential_Io, Ada.Exceptions;
with Directory, Text_Handler;

with Oper_Def;
package body File_Mng is

  package Read_Oper_Io is new Ada.Sequential_Io(Oper_Def.read_Oper_Rec);
  package Oper_Io is new Ada.Sequential_Io(Oper_Def.Oper_Rec);


  -- First record in file as to be this
  --  except amount which is the account amount
  Magic_Oper : Oper_Def.Oper_Rec;


  -- Overwrites the list from file content
  procedure Load (File_Name : in String;
                  Oper_List : in out Oper_List_Mng.List_Type;
                  Can_Write : out Boolean) is
    File : Read_Oper_Io.File_Type;
    -- A record to read
    Loc_Read_Oper : Oper_Def.Read_Oper_Rec;
    Loc_Oper : Oper_Def.Oper_Rec;
    -- A list for tempo read
    Loc_List : Oper_List_Mng.List_Type;
    use Oper_Def;
  begin
    -- First test we can access file to read
    begin
      Read_Oper_Io.Open (File, Read_Oper_Io.In_File, File_Name);
    exception
      when others => raise F_Access_Error;
    end;
    Read_Oper_Io.Close (File);

    -- Then test we can write file
    Test_Write:
    declare
      Write_File : Read_Oper_Io.File_Type;
    begin
      Read_Oper_Io.Open (Write_File, Read_Oper_Io.Append_File, File_Name);
      Can_Write := True;
      Read_Oper_Io.Close (Write_File);
    exception
      when others =>
        -- Cannot be opened for writting
        Can_Write := False;
    end Test_Write;

    -- Then open file for work
    Read_Oper_Io.Open (File, Read_Oper_Io.In_File, File_Name);
    Read_Oper_Io.Reset (File);

    -- Read magic record
    begin
      Read_Oper_Io.Read (File, Loc_Read_Oper);
    exception
      when others =>
        -- There should be at least the magic record
        Read_Oper_Io.Close (File);
        raise F_Access_Error;
    end;
    Loc_Read_Oper.Amount := Magic_Oper.Amount;
    if Loc_Oper /= Magic_Oper then
      -- Bad magic record
      Read_Oper_Io.Close (File);
      raise F_Access_Error;
    end if;

    -- Read records from file to list
    Read_Oper_Io.Reset (File);
    loop
      begin
        Read_Oper_Io.Read (File, Loc_Read_Oper);
      exception
        when Read_Oper_Io.End_Error =>
          exit;
      end;
      Oper_Def.Convert (Loc_Read_Oper, Loc_Oper);
      Oper_List_Mng.Insert (Loc_List, Loc_Oper);
    end loop;

    Read_Oper_Io.Close (File);

    -- Everything is Ok. Overwrite the existing list. Go to end.
    Oper_List_Mng.Delete_List (Oper_List);
    Oper_List_Mng.Assign (Oper_List, Loc_List);
    Oper_List_Mng.Rewind (Oper_List, Oper_List_Mng.Prev);

  exception
    when F_Access_Error =>
      raise;
    when File_Error : others =>
      Ada.Text_Io.Put_Line ("Exception "
                          & Ada.Exceptions.Exception_Name(File_Error)
                          & " raised while loading file " & File_Name);
      begin
        Read_Oper_Io.Close (File);
      exception
        when others => null;
      end;
      -- Clean the garbage
      Oper_List_Mng.Delete_List (Loc_List);
      raise F_Io_Error;
  end Load;

  -- Save the list in file
  procedure Save (File_Name : in String;
                  Oper_List : in Oper_List_Mng.List_Type) is
    -- Position in list
    Loc_Pos : Natural;
    File : Oper_Io.File_Type;
    -- A record to write
    Loc_Oper, Loc_Oper_1 : Oper_Def.Oper_Rec;
    -- A list for tempo move
    Loc_List : Oper_List_Mng.List_Type;
  begin
    Oper_List_Mng.Assign (Loc_List, Oper_List);

    -- Save current position
    Loc_Pos := Oper_List_Mng.Get_Position (Loc_List);

    -- Create / erase file
    declare
      Read_File : Read_Oper_Io.File_Type;
    begin
      Read_Oper_Io.Open(Read_File, Read_Oper_Io.Out_File, File_Name);
      Read_Oper_Io.Delete(Read_File);
      Oper_Io.Create(File, Oper_Io.Out_File, File_Name);
    exception
      when Read_Oper_Io.Name_Error =>
        -- New file
        begin
          Oper_Io.Create (File, Oper_Io.Out_File, File_Name);
        exception
          when others =>
            -- Cannot create
            raise F_Access_Error;
        end;
      when others =>
        -- Other error on open
        raise F_Access_Error;
    end;

    -- Rewind, write magic record with amount of first record
    Oper_List_Mng.Rewind (Loc_List);
    Oper_List_Mng.Read (Loc_List, Loc_Oper, Oper_List_Mng.Current);
    Loc_Oper_1 := Magic_Oper;
    Loc_Oper_1.Amount := Loc_Oper.Amount;
    Oper_Io.Write (File, Loc_Oper_1);

    -- Write other records
    loop
      exit when not Oper_List_Mng.Check_Move (Loc_List);
      Oper_List_Mng.Move_To (Loc_List);
      Oper_List_Mng.Read (Loc_List, Loc_Oper, Oper_List_Mng.Current);
      Oper_Io.Write (File, Loc_Oper);
    end loop;

    -- Done. Close file, move to saved position
    Oper_Io.Close (File);
    Oper_List_Mng.Move_To (Loc_List, Oper_List_Mng.Next, Loc_Pos-1, False);

  exception
    when F_Access_Error =>
      raise;
    when File_Error : others =>
      Ada.Text_Io.Put_Line ("Exception "
                          & Ada.Exceptions.Exception_Name(File_Error)
                          & " raised while closing file " & File_Name);
      begin
        Oper_Io.Close (File);
      exception
        when others => null;
      end;
      raise F_Io_Error;
  end Save;

end File_Mng;

