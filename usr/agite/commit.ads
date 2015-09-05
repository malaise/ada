with Git_If;
package Commit is

  -- Handle the commit of modifications
  -- Show button Quit instead of Push
  -- Init comment from the one of the provided Hash
  procedure Handle (Root : in String;
                    Quit_Io_Push : in Boolean := False;
                    Hash_For_Comment : in Git_If.Git_Hash := Git_If.No_Hash);

end Commit;

