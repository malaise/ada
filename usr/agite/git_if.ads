with As.U, Dynamic_List, Sys_Calls;
package Git_If is

  -- Git version
  -- No_Git is raised if "git --version" fails
  -- Calls Command
  type Version_Rec is record
    Major, Medium, Minor : Natural;
  end record;
  No_Git : exception;
  function Get_Version return Version_Rec;


  -- Current Root and relative path to git, empty or "/" appended
  procedure Get_Root_And_Path (Root, Path : out As.U.Asu_Us);

  -- LIST OF FILES AND STATUS
  -- A file entry
  type File_Entry_Rec is record
    S2 : Character;
    S3 : Character;
    Name : As.U.Asu_Us;
    Kind : Character;
  end record;
  package File_Mng is new Dynamic_List (File_Entry_Rec);
  subtype File_List is File_Mng.Dyn_List.List_Type;

  -- List the files and status
  procedure List_Files (Current_Path : in String;
                        Files : in out File_List);

  -- The character Kins associated to a kind
  function Char_Of (Kind : Sys_Calls.File_Kind_List) return Character;

  -- A comment of commit
  type Comment_Array is array (Positive range <>) of As.U.Asu_Us;
  subtype Comment_1 is Comment_Array (1 .. 1);

  -- LOG HISTORY
  -- Git hashing number
  subtype Git_Hash is String (1 .. 40);
  No_Hash : constant Git_Hash := (others => ' ');
  -- A date at iso YYYY-MM-DD HH:MM:SS
  subtype Iso_Date is String (1 .. 19);
  -- A log entry
  type Log_Entry_Rec is record
    Hash : Git_Hash;
    Date : Iso_Date;
    Comment : Comment_1;
  end record;
  package Log_Mng is new Dynamic_List (Log_Entry_Rec);
  subtype Log_List is Log_Mng.Dyn_List.List_Type;

  -- List the log of a dir or file
  procedure List_Log (Path : in String;
                      Log : in out Log_List);


  -- COMMIT DETAILS
  -- A commit file entry
  type Commit_Entry_Rec is record
    Status : Character;
    File : As.U.Asu_Us;
  end record;
  package Commit_File_Mng is new Dynamic_List (Commit_Entry_Rec);
  subtype Commit_List is Commit_File_Mng.Dyn_List.List_Type;

  -- List detailed info on a commit
  procedure List_Commit (Hash : in Git_Hash;
                         Date : out Iso_Date;
                         Comment : out Comment_Array;
                         Commit : in out Commit_List);

  -- Cat a file at a Hash in a file, Ok if success
  function Cat (Name : String; Hash : String; File : String) return Boolean;

  -- Launch a diff (asynchronous) from current to HEAD
  procedure Launch_Diff (Differator, File_Name : in String);

  -- Launch a diff (asynchronous) from Comp to Ref
  procedure Launch_Delta (Differator, File_Name : in String;
                          Ref_Rev, Comp_Rev : in String);

  -- Launch a revert (checkout) synchronous
  procedure Do_Revert (File : in String);

  -- Get current branch name
  function Current_Branch return String;

end Git_If;

