with Sys_Calls, Text_Line;
package body Database is

  -- For working on a database
  type Db_Access is access all As.U.Utils.Asu_Dyn_List_Mng.List_Type;

  -- For save
  Words_Name, Nouns_Name : As.U.Asu_Us;
  Suffix : constant String := ".sav";

  -- Local: init a data base
  procedure Init_Db (File_Name : in String;
                     Db : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type) is
    Fd : Sys_Calls.File_Desc;
    File : Text_Line.File_Type;
  begin
    Logger.Init ("Xwords");
    -- Clear
    Db.Delete_List;
    -- Load file
    Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
    File.Open (Text_Line.In_File, Fd);
    loop
      declare
        Word : constant String := File.Get;
        Len : Natural := Word'Length;
      begin
        -- End of file
        exit when Len = 0;
        if Word(Len) = Text_Line.Line_Feed_Char then
          Len := Len - 1;
        end if;
        Db.Insert (As.U.Tus (Word(1 .. Len)));
      end;
    end loop;

    -- Done
    File.Close;
    Sys_Calls.Close (Fd);

  exception
    when Sys_Calls.Name_Error | Sys_Calls.System_Error =>
      raise Init_Error;
  end Init_Db;

  -- Init database from a dictionary (file with one word per line)
  -- Reset it if already init
  procedure Init (Words_File, Nouns_File : in String) is
  begin
    Words_Name.Set (Words_File);
    Nouns_Name.Set (Nouns_File);
    Init_Db (Words_File, Words_Db);
    Init_Db (Nouns_File, Nouns_Db);
  end Init;

  -- Add a word in the database if does not exist
  procedure Add (Word : in As.U.Asu_Us; Noun : in Boolean) is
    Uword : As.U.Asu_Us;
    Dba : Db_Access;
    Found : Boolean;
    use type As.U.Asu_Us;
  begin
    Dba := (if Noun then Nouns_Db'Access  else Words_Db'Access);
    if Dba.Is_Empty then
      return;
    end if;

    -- Iterate on all words
    Dba.Rewind;
    Found := False;
    loop
      -- Find Uword after Word
      Dba.Read (Uword, Move => As.U.Utils.Asu_Dyn_List_Mng.Current);
      if Uword > Word then
        -- Insert word before current
        Logger.Log_Debug ("Inserting " & Word.Image & " " & Noun'Img);
        Dba.Insert (Word, As.U.Utils.Asu_Dyn_List_Mng.Prev);
        Dba.Move_To;
        Found := True;
        exit;
      elsif Uword = Word then
        Found := True;
        exit;
      end if;
      exit when not Dba.Check_Move;
      Dba.Move_To;
    end loop;
    if not Found then
      -- End reached without finding Uword > Word: append
      Dba.Insert (Word);
    end if;
    Dba.Rewind;
  end Add;

  -- Delete a word from the database if it exists
  procedure Del (Word : in As.U.Asu_Us; Noun : in Boolean) is
    Moved : Boolean;
    Uword : As.U.Asu_Us;
    Dba : Db_Access;
    use type As.U.Asu_Us;
  begin
    Dba := (if Noun then Nouns_Db'Access  else Words_Db'Access);
    if Dba.Is_Empty then
      return;
    end if;
    -- Find the word if it exists
    Dba.Rewind;
    loop
      -- Get the word
      Dba.Read (Uword, As.U.Utils.Asu_Dyn_List_Mng.Current);
      if Word = Uword then
        -- The word exists
        Logger.Log_Debug ("Deleting " & Word.Image & " " & Noun'Img);
        Dba.Delete (Moved => Moved);
        exit when not Moved;
      else
        exit when not Dba.Check_Move;
        Dba.Move_To;
      end if;
    end loop;
    if not Dba.Is_Empty then
      Dba.Rewind;
   end if;
  end Del;

  procedure Save (Noun : in Boolean) is
    Name : As.U.Asu_Us;
    Dba : Db_Access;
    File : Text_Line.File_Type;
  begin
    -- save propoer database
    if Noun then
      Name := Nouns_Name;
      Dba :=  Nouns_Db'Access;
    else
      Name := Words_Name;
      Dba := Words_Db'Access;
    end if;
    -- Save previous file
    if not Sys_Calls.Rename (Name.Image, Name.Image & Suffix) then
      raise Backup_Error;
    end if;

    -- Create file
    File.Create_All (Name.Image);
    if Dba.Is_Empty then
      File.Close_All;
      return;
    end if;

    -- Write database
    Dba.Rewind;
    loop
      File.Put_Line (Dba.Access_Current.all.Image);
      exit when not Dba.Check_Move;
      Dba.Move_To;
    end loop;

    -- Done
    File.Close_All;

  exception
    when Backup_Error =>
      Logger.Log_Debug ("Bakup error on " & Name.Image);
      raise;
    when others =>
      Logger.Log_Debug ("Save error on " & Name.Image);
      if File.Is_Open then
        File.Close_All;
      end if;
      raise Save_Error;
  end Save;

end Database;

