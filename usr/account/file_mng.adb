with Text_Io, Sequential_Io;
with Ada.Exceptions;
with Directory, Text_Handler;

with Oper_Def;
package body File_Mng is

  package Oper_Io is new Sequential_Io(Oper_Def.Oper_Rec);


  -- First record in file as to be this
  --  except amount which is the account amount
  Magic_Oper : Oper_Def.Oper_Rec;


  -- Overwrites the list from file content
  procedure Load (File_Name : in String;
                  Oper_List : in out Oper_List_Mng.List_Type;
                  Can_Write : out Boolean) is
    File : Oper_Io.File_Type;
    -- A record to read
    Loc_Oper : Oper_Def.Oper_Rec; 
    -- A list for tempo read
    Loc_List : Oper_List_Mng.List_Type;
    use Oper_Def;
  begin
    -- First test we can access file
    begin
      Oper_Io.Open (File, Oper_Io.In_File, File_Name);
    exception
      when others => raise F_Access_Error;
    end;
    Oper_Io.Close (File);

    -- Then test we can write file
    Test_Write:
    declare
      Write_File : Oper_Io.File_Type;
    begin
      Oper_Io.Open (Write_File, Oper_Io.Append_File, File_Name);
      Can_Write := True;
      Oper_Io.Close (Write_File);
    exception
      when others =>
        -- Cannot be opened for writting 
        Can_Write := False;
    end Test_Write;

    -- Then open file for work 
    Oper_Io.Open (File, Oper_Io.In_File, File_Name);
    Oper_Io.Reset (File);

    -- Read magic record
    begin
      Oper_Io.Read (File, Loc_Oper);
    exception
      when others =>
        -- There should be at least the magic record
        Oper_Io.Close (File);
        raise F_Access_Error;
    end;
    Loc_Oper.Amount := Magic_Oper.Amount;
    if Loc_Oper /= Magic_Oper then
      -- Bad magic record
      Oper_Io.Close (File);
      raise F_Access_Error;
    end if;

    -- Read records from file to list
    Oper_Io.Reset (File);
    loop
      begin
        Oper_Io.Read (File, Loc_Oper);
      exception
        when Oper_Io.End_Error =>
          exit;
      end;
      Oper_List_Mng.Insert (Loc_List, Loc_Oper);
    end loop;

    Oper_Io.Close (File);

    -- Everything is Ok. Overwrite the existing list. Go to end.
    Oper_List_Mng.Delete_List (Oper_List);
    Oper_List_Mng.Assign (Oper_List, Loc_List);
    Oper_List_Mng.Move_To (Oper_List, Oper_List_Mng.Prev, 0, False);
    
  exception
    when F_Access_Error =>
      raise;
    when File_Error : others =>
      Text_Io.Put_Line ("Exception " & Ada.Exceptions.Exception_Name(File_Error)
                      & " raised while loading file " & File_Name);
      begin
        Oper_Io.Close (File);
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
    begin
      Oper_Io.Open(File, Oper_Io.Out_File, File_Name);
      Oper_Io.Delete(File);
      Oper_Io.Create(File, Oper_Io.Out_File, File_Name);
    exception
      when Oper_Io.Name_Error =>
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
    Oper_List_Mng.Move_To (Loc_List, Oper_List_Mng.Next, 0, False);
    Oper_List_Mng.Read (Loc_List, Loc_Oper, Oper_List_Mng.Current);
    Loc_Oper_1 := Magic_Oper;
    Loc_Oper_1.Amount := Loc_Oper.Amount;
    Oper_Io.Write (File, Loc_Oper_1);

    -- Write other records
    loop
      begin
        Oper_List_Mng.Move_To (Loc_List, Oper_List_Mng.Next);
      exception
        when Oper_List_Mng.Not_In_List =>
          exit;
      end;
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
      Text_Io.Put_Line ("Exception " & Ada.Exceptions.Exception_Name(File_Error)
                      & " raised while closing file " & File_Name);
      begin
        Oper_Io.Close (File);
      exception
        when others => null;
      end;
      raise F_Io_Error;
  end Save;

end File_Mng;

