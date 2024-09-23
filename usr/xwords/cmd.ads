with As.U.Utils;
package Cmd is

  -- Init links to files (for Add/Del), regexps...
  Init_Error : exception;
  procedure Init (Words_File, Nouns_File : in String);

  -- Search result or error
  package Res_Mng renames As.U.Utils.Asu_Dyn_List_Mng;
  subtype Res_List is Res_Mng.List_Type;

  -- Execute command
  -- Noun is not significant for Search, which looks in both lists
  -- Regex is not significant for Add and Del
  -- If search OK then append result to Res
  -- If error then Set result to output/error data
  type Cmd_List is (Search, Add, Del);
  Terminate_Request : exception;
  procedure Exec (Comd  : in Cmd_List;
                  Regex : in Boolean;
                  Noun  : in Boolean;
                  Arg   : in String;
                  Ok    : out Boolean;
                  Res   : in out Res_List);

end Cmd;

