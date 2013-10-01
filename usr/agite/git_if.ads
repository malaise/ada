with As.U.Utils, Dynamic_List, Sys_Calls;
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
  -- Two letters of GIT status (? A M D R C U)
  -- The file name (and previous name in case of R)
  -- ONe letter of file kind
  type File_Entry_Rec is record
    S2 : Character;      -- ' ', '?', 'A', 'M', 'D', 'R', 'C' or 'U'
    S3 : Character;
    Name : As.U.Asu_Us;
    Kind : Character;    -- ' ', '@', '/' or '?'
    Prev : As.U.Asu_Us;
  end record;
  package File_Mng is new Dynamic_List (File_Entry_Rec);
  subtype File_List is File_Mng.Dyn_List.List_Type;

  -- List the files of Current_Path and status
  procedure List_Files (Current_Path : in String;
                        Files : in out File_List);

  -- List all the files modified in the current repository
  procedure List_Changes (Files : in out File_List);

  -- Status of a file
  function Get_Status (File : String) return File_Entry_Rec;

  -- The character Kinds associated to a kind
  function Char_Of (Kind : Sys_Calls.File_Kind_List) return Character;

  -- Is a file (full path) locally modified
  function Is_Modified (File : String) return Boolean;

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

  -- Get last hash (hash of last commit) of file or dir
  function Last_Hash (Path : in String) return Git_Hash;

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

  -- List references
  package Reference_Mng renames As.U.Utils.Asu_Dyn_List_Mng;
  procedure List_References (References : in out Reference_Mng.List_Type);

  -- Cat a file at a Hash in a file, Ok if success
  function Cat (Name : String; Hash : String; File : String;
                Log_Error : Boolean := True) return Boolean;

  -- Launch a diff (asynchronous) from current to HEAD
  procedure Launch_Diff (Differator, File_Name : in String);

  -- Launch a diff (asynchronous) from Comp to Ref
  procedure Launch_Delta (Differator, File_Name : in String;
                          Ref_Rev, Comp_Rev : in String);

  -- Launch a revert (checkout HEAD) synchronous
  procedure Do_Revert (File : in String);

  -- Launch a reset of index synchronous
  procedure Do_Reset (File : in String);

  -- Launch a add to index synchronous
  procedure Do_Add (File : in String);

  -- Launch a rm to index synchronous
  procedure Do_Rm (File : in String);

  -- Launch a commit synchronous, return "" if OK, else the error
  function Do_Commit (Comment : String) return String;

  -- Launch a push synchronous, return True if OK
  function Do_Push (Remote : String) return Boolean;

  -- Launch a pull synchronous, return True if OK
  function Do_Pull (Remote : String; Branch : String) return Boolean;

  -- Get current branch name
  function Current_Branch return String;

  -- Get current user name and email
  function Get_User return String;

  -- Stash management
  subtype Stash_Number is Natural;
  type Stash_Entry_Rec is record
    Num : Stash_Number;
    Branch : As.U.Asu_Us;
    Name : As.U.Asu_Us;
  end record;
  package Stash_Mng is new Dynamic_List (Stash_Entry_Rec);
  subtype Stash_List is Stash_Mng.Dyn_List.List_Type;

  -- List the stashes
  procedure List_Stashes (Stashes : in out Stash_List);

  -- Stash current context, return "" if Ok
  function Add_Stash (Name : String) return String;

  -- Apply a stash, return "" if Ok
  function Apply_Stash (Num : Stash_Number) return String;

  -- Pop (apply & delete) a stash, return "" if Ok
  function Pop_Stash (Num : Stash_Number) return String;

  -- Drop a stash, return "" if Ok
  function Drop_Stash (Num : Stash_Number) return String;

end Git_If;

