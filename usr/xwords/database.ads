with As.U.Utils, Trace.Loggers;
package Database is
  -- A common logger
  Logger : Trace.Loggers.Logger;

  -- The databases
  Words_Db, Nouns_Db : aliased As.U.Utils.Asu_Dyn_List_Mng.List_Type;

  -- Init database from 2 dictionaries (files with one word per line)
  -- One with common words and one with nouns
  -- Reset it if already init
  Init_Error : exception;
  procedure Init (Words_File, Nouns_File : in String);

  -- Add a word in the database if it does not exist
  procedure Add (Word : in As.U.Asu_Us; Noun : in Boolean);
  -- Delete a word from the database if it exists
  procedure Del (Word : in As.U.Asu_Us; Noun : in Boolean);

  -- Save a database
  Backup_Error, Save_Error : exception;
  procedure Save (Noun : in Boolean);

end Database;
