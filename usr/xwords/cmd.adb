with Ada.Exceptions;
with Str_Util, Reg_Exp, Basic_Proc, Upper_Str;
with Database;
package body Cmd is

  -- Path to Words and files
  Words_File : As.U.Asu_Us;
  Nouns_File : As.U.Asu_Us;

  -- Non regex pattern
  Non_Regex_Crit : constant String := "[a-z?:*.]+";
  Non_Regex_Patt : Reg_Exp.Compiled_Pattern;

  -- Init links to files (for Add/Del)
  procedure Init (Words_File, Nouns_File : in String) is
    Ok : Boolean;
  begin
    Cmd.Words_File.Set (Words_File);
    Cmd.Nouns_File.Set (Nouns_File);
    -- Compile non-regex pattern
    Non_Regex_Patt.Compile (Ok, Non_Regex_Crit);
    if not Ok then
      Basic_Proc.Put_Line_Error ("Error compiling non regexp pattern");
      raise Init_Error;
    end if;
  end Init;

  -- Word (add or del) pattern
  Word_Pattern : constant String := "[a-z]+";

  -- One of the Db
  type Db_Access is access all As.U.Utils.Asu_Dyn_List_Mng.List_Type;

  -- subtype Line_Type is Asu_Us;
  -- package Res_Mng is new Dynamic_List (Line_Type);
  -- subtype Res_List is Res_Mng.Dyn_List.List_Type;
  procedure Exec (Comd  : in Cmd_List;
                  Regex : in Boolean;
                  Noun  : in Boolean;
                  Arg   : in String;
                  Ok    : out Boolean;
                  Res   : in out Res_List) is

    -- Set error Msg in Res
    procedure Error (Msg : in String) is
    begin
      Res.Delete_List;
      Res.Insert (As.U.Tus (Msg));
      Ok := False;
    end Error;

    Txt : As.U.Asu_Us;
    Index : Natural;
    Search_Patt : Reg_Exp.Compiled_Pattern;
    Found : Boolean;
    Db : Db_Access;
  begin

    Database.Logger.Log_Debug ("Command " & Comd'Img & " " & Regex'Img
                      & " " & Noun'Img & " >" & Arg & "<");
    Ok := True;
    if Comd = Search then
      -- Build search string
      Txt.Set (Arg);

      if not Regex then
        -- Check and transalte non-regex
        -- Match non-regex syntax
        if not Non_Regex_Patt.Match (Txt.Image, True) then
          Database.Logger.Log_Debug ("KO");
          Error ("Invalid pattern");
          return;
        end if;
        -- '*' if any must be the last char
        Index := Txt.Locate ("*");
        if Index /= 0 and then Index /= Txt.Length then
          Error ("Invalid pattern");
          return;
        end if;
        -- Replace '?' and ':' by  '.'
        Txt.Set (Str_Util.Substit (Txt.Image, "?", "."));
        Txt.Set (Str_Util.Substit (Txt.Image, ":", "."));
        -- Replace '*' and ':' by  '.*'
        Txt.Set (Str_Util.Substit (Txt.Image, "*", ".*"));
      end if;

      -- Compile
      Search_Patt.Compile (Ok, Txt.Image);
      if not Ok then
        Error ("Invalid pattern");
        return;
      end if;
      Database.Logger.Log_Debug ("Pattern >" & Txt.Image & "< compiled");

      -- Search in Db
      Db := (if Noun then Database.Nouns_Db'Access
             else Database.Words_Db'Access);
      if Db.Is_Empty then
        return;
      end if;
      Db.Rewind;
      loop
        Db.Read (Txt, Moved => Found);
        if Search_Patt.Match (Txt.Image, True) then
          -- Insert matching words (uppercase if noun)
          if Noun then
           Txt.Set (Upper_Str (Txt.Image));
          end if;
          Res.Insert (Txt);
        end if;
        exit when not Found;
      end loop;
    return;
    end if;

    -- Add or Del: Check Word
    if not Reg_Exp.Match (Word_Pattern, Arg, True) then
      Error ("Invalid word");
      return;
    end if;

    if Comd = Add then
      Database.Add (As.U.Tus (Arg), Noun);
    else
      Database.Del (As.U.Tus (Arg), Noun);
    end if;
    Database.Save (Noun);

  exception
    when Database.Backup_Error =>
      Error ("Cannot create backup file");
    when Database.Save_Error =>
      Error ("Cannot save database in file");
    when Err:others =>
      Error ("Exception  " & Ada.Exceptions.Exception_Name (Err));
  end Exec;

end Cmd;

