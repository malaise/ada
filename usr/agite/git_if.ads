with As.U.Utils, Long_Long_Limited_List, Sys_Calls;
package Git_If is

  -- All the calls to Git wait until completion of the sub-command
  --  which require them to be encapsulated between Afpx.Suspend.Resume if
  --  Afpx is active
  -- Except Launch_Diff and Launch_Delta that do not wait
  procedure Entering_Afpx;
  procedure Leaving_Afpx;

  -- Git version
  -- No_Git is raised if "git --version" fails
  -- Calls Command
  type Version_Rec is record
    Major, Medium, Minor : Natural;
  end record;
  No_Git : exception;
  function Get_Version return Version_Rec;


  -- Current Root and relative path to git, empty or "/" appended
  -- No_Git is raised if no ".git" (or $GIT_DIR) is found
  --  in parent dirs
  procedure Get_Root_And_Path (Root, Path : out As.U.Asu_Us);

  -- Is current repository a bare one
  function Is_Bare return Boolean;

  -- LIST OF FILES AND STATUS
  -- A file entry
  -- Two letters of Git status (? A M D R C U)
  -- The file/dir name
  -- One letter of file kind (' ', '@', '/', '?')
  -- Previous name (in case of R)
  -- Target path (in case of @), only set by List_Files
  type File_Entry_Rec is record
    S2 : Character;      -- ' ', '?', 'A', 'M', 'D', 'R', 'C' or 'U'
    S3 : Character;
    Name : As.U.Asu_Us;
    Kind : Character;    -- ' ', '@', '/' or '?'
    Prev : As.U.Asu_Us;
    Link_Ok : Boolean;    -- Only set by List_Files
    Target : As.U.Asu_Us; -- Only set by List_Files
  end record;
  procedure Set (To : out File_Entry_Rec; Val : in File_Entry_Rec);
  package  File_Mng is new Long_Long_Limited_List (File_Entry_Rec, Set);
  subtype File_List is File_Mng.List_Type;

  -- List the files of Current_Path and status
  procedure List_Files (Current_Path : in String;
                        Files : in out File_List);

  -- List all the files modified in the current directory or the
  --  current repository
  procedure List_Changes (Files : in out File_List;
                          Path : in String := ".");

  -- Status of a file
  function Get_Status (File : String) return File_Entry_Rec;

  -- The character Kinds associated to a kind
  function Char_Of (Kind : Sys_Calls.File_Kind_List) return Character;

  -- Resolve recursively a symlink (if Kind is '@' then fill Target)
  procedure Resolve_Link (File_Entry : in out File_Entry_Rec);

  -- Is a file (full path) locally modified
  function Is_Modified (File : String) return Boolean;

  -- A comment of commit
  type Comment_Array is array (Positive range <>) of As.U.Asu_Us;
  subtype Comment_1 is Comment_Array (1 .. 1);
  subtype Comment_2 is Comment_Array (1 .. 2);

  -- LOG HISTORY
  -- Git hashing number
  subtype Git_Hash is As.U.Asu_Us;
  No_Hash : constant Git_Hash := As.U.Asu_Null;
  -- A date at iso YYYY-MM-DD HH:MM:SS
  subtype Iso_Date is String (1 .. 19);
  No_Date : constant Iso_Date := (others => ' ');
  -- A log entry
  type Log_Entry_Rec is record
    Hash : Git_Hash := No_Hash;
    Merged : Boolean := False;
    Date : Iso_Date := No_Date;
    Comment : Comment_2;
    Extra : As.U.Asu_Us;
  end record;
  procedure Set (To : out Log_Entry_Rec; Val : in Log_Entry_Rec);
  package Log_Mng is new Long_Long_Limited_List (Log_Entry_Rec, Set);
  subtype Log_List is Log_Mng.List_Type;

  -- List the log on a branch of a dir or file
  -- Stop at Max if not 0
  -- Set --sparse (when on root) to log full reposit history (including merges)
  -- Set Extra to the status of last file of the commit
  -- Returns wether the end of list is reached at or before Max
  -- May raise anonymous exception Log_Error
  procedure List_Log (Branch, Path : in String;
                      From_Rev : in String;
                      Max : in Log_Mng.Ll_Natural;
                      Sparse : in Boolean;
                      Status : in Boolean;
                      Log : in out Log_List;
                      End_Reached : out Boolean);

  -- List Tree
  -- A tree entry
  type Tree_Entry_Rec is record
    Head : As.U.Asu_Us;
    Hash : Git_Hash := No_Hash;
    Tail : As.U.Asu_Us;
  end record;
  procedure Set (To : out Tree_Entry_Rec; Val : in Tree_Entry_Rec);
  package Tree_Mng is new Long_Long_Limited_List (Tree_Entry_Rec, Set);
  procedure List_Tree (Path : in String;
                       Max : in Tree_Mng.Ll_Natural;
                       Tree : in out Tree_Mng.List_Type;
                       End_Reached : out Boolean);

  -- Get last hash (hash of last commit) of file or dir
  function Last_Hash (Path : in String) return Git_Hash;

  -- Get info on a commit: fill Date and Comment
  procedure Info_Commit (Commit : in out Log_Entry_Rec);

  -- COMMIT DETAILS
  -- A commit file entry
  type Commit_Entry_Rec is record
    Status : Character;
    File : As.U.Asu_Us;
  end record;
  procedure Set (To : out Commit_Entry_Rec; Val : in Commit_Entry_Rec);
  package Commit_File_Mng is new Long_Long_Limited_List (Commit_Entry_Rec, Set);
  subtype Commit_List is Commit_File_Mng.List_Type;

  -- List detailed info on a commit
  -- May raise anonymous exception Log_Error
  procedure List_Commit (Rev_Tag : in String;
                         Hash : out Git_Hash;
                         Merged : out Boolean;
                         Date : out Iso_Date;
                         Comment : out Comment_Array;
                         Commit : in out Commit_List);

  -- List references
  package Reference_Mng renames As.U.Utils.Asu_Long_Long_List_Mng;
  procedure List_References (References : in out Reference_Mng.List_Type);

  -- Cat a file at a Hash in a file, Ok if success
  function Cat (Name : String; Hash : String; File : String;
                Log_Error : Boolean := True) return Boolean;

  -- Launch a diff (asynchronous) from current to HEAD
  procedure Launch_Diff (Differator, File_Name : in String);

  -- Launch a diff (asynchronous) from Comp to Ref
  -- If Comp_Name is empty the File_Name is used for both revs
  procedure Launch_Delta (Differator : in String;
                          File_Name, Ref_Rev, Comp_Rev : in String;
                          Comp_Name : in String := "");

  -- Launch a revert (checkout HEAD) synchronous
  procedure Do_Revert (File : in String);

  -- Launch a reset of index synchronous
  procedure Do_Reset (File : in String);

  -- Launch a reset --hard [ <rev> ]
  procedure Do_Reset_Hard (Rev : in String := "");

  -- Launch a soft or mixed reset
  procedure Do_Reset (Rev : in String; Soft : in Boolean);

  -- Launch a clean
  procedure Do_Clean;

  -- Launch a global checkout, return "" if OK, else the error
  function Do_Checkout (Rev_Tag, Branch : String) return String;

  -- Launch a add to index synchronous
  procedure Do_Add (File : in String);

  -- Launch a rm to index synchronous
  procedure Do_Rm (File : in String);

  -- Launch a commit synchronous, return "" if OK, else the error
  function Do_Commit (Comment : String) return String;

  -- Default name of origin in a tracking branch
  Default_Origin : constant String := "origin";

  -- Launch a push synchronous (of current branch or tag),
  --  optionnaly add --set-upstream (on branch)
  --  optionnaly add -- force
  --  return "" if OK, else the error
  function Do_Push (Remote : String; Tag : String;
                    Set_Upstream : Boolean;
                    Force : Boolean) return String;

  -- Launch a fetch (or pull) synchronous, Output flow if Ok, else Error flow
  procedure Do_Fetch (Remote : in String; Branch : in String; Pull : in Boolean;
                      Ok : out Boolean; Flow : out As.U.Asu_Us);

  -- Prune useless tracked branches on Remote
  procedure Do_Prune (Remote : String);

  -- Get current branch name
  function Current_Branch return String;

  -- List local or remote tracking branches, or both
  -- Separator between <remote> and <branch>
  Separator : constant Character := '/';
  package Branches_Mng renames As.U.Utils.Asu_Long_Long_List_Mng;
  procedure List_Branches (Local, Remote : in Boolean;
                           Branches : in out Branches_Mng.List_Type);
  -- List branches of a reference
  procedure List_Branches_Of (Reference : in String;
                              Branches : in out Branches_Mng.List_Type);

  -- Create, rename, delete, merge, rebase a branch,
  --  return "" if Ok else the error
  function Create_Branch (Name : String) return String;
  function Rename_Branch (Name, New_Name : String) return String;
  function Delete_Branch (Name : String; Remote : in Boolean := False)
           return String;
  function Merge_Branch (Name : String;
                         Comment : String;
                         No_Fast_Forward : Boolean;
                         No_Commit : Boolean) return String;

  -- Get the name of the remote tracking banch of a branch (or current)
  -- Return "" if error or no remote tracking branch found
  function Remote_Branch (Name : in String := "") return String;

  -- Get current user name and email
  function Get_User return String;

  -- Stash management
  subtype Stash_Number is Natural;
  -- Default string for branch or name
  Stash_Default_Str : constant String := "-";
  type Stash_Entry_Rec is record
    Num : Stash_Number;
    Branch : As.U.Asu_Us;
    Name : As.U.Asu_Us;
  end record;
  procedure Set (To : out Stash_Entry_Rec; Val : in Stash_Entry_Rec);
  package Stash_Mng is new Long_Long_Limited_List (Stash_Entry_Rec, Set);
  subtype Stash_List is Stash_Mng.List_Type;

  -- List the stashes
  procedure List_Stashes (Stashes : in out Stash_List);

  -- Stash current context, return "" if Ok else the error
  function Add_Stash (Name : String) return String;

  -- Apply a stash, return "" if Ok else the error
  function Apply_Stash (Num : Stash_Number) return String;

  -- Pop (apply & delete) a stash, return "" if Ok else the error
  function Pop_Stash (Num : Stash_Number) return String;

  -- Drop a stash, return "" if Ok else the error
  function Drop_Stash (Num : Stash_Number) return String;

  -- Rename a stash, return "" if Ok else the error
  function Rename_Stash (Num : Stash_Number; Name : String) return String;

  -- List of tags
  type Tag_Entry_Rec is record
    Name : As.U.Asu_Us;
    Hash : Git_Hash;
    Annotated : Boolean;
    Date : Iso_Date;
    Comment : As.U.Asu_Us;
  end record;
  procedure Set (To : out Tag_Entry_Rec; Val : in Tag_Entry_Rec);
  package Tag_Mng is new Long_Long_Limited_List (Tag_Entry_Rec, Set);
  subtype Tag_List is Tag_Mng.List_Type;

  -- List tags matching Template
  -- May raise anonymous exception Log_Error
  procedure List_Tags (Template : in String;
                       Tags : in out Tag_List);

  -- Delete tag
  procedure Delete_Tag (Tag : in String);

  -- Add a tag, return "" if Ok else the error
  function Add_Tag (Tag : String;
                    Hash : Git_Hash;
                    Annotated : Boolean;
                    Comment : in String) return String;

  -- List cherry commits: the commits in Ref, and indicates if they are
  --  or not merged in target
  -- Inserts the Log_Entry_Rec with only the fields Hash and Merged set
  procedure Cherry_List (Ref, Target : in String;
                         Commits : in out Log_List);

  -- Cherry pick a commit into current branch
  -- Commit it or not
  -- Returns "" if OK else the error
  function Cherry_Pick (Commit : in Log_Entry_Rec;
                        Do_Commit : in Boolean) return String;

  -- Reflog
  subtype Ref_Number is Natural;
  type Reflog_Entry_Rec is record
    Hash : Git_Hash;
    Id : As.U.Asu_Us;
    Date : Iso_Date := No_Date;
    Comment : As.U.Asu_Us;
  end record;
  procedure Set (To : out Reflog_Entry_Rec; Val : in Reflog_Entry_Rec);
  package Reflog_Mng is new Long_Long_Limited_List (Reflog_Entry_Rec, Set);
  subtype Reflog_List is Reflog_Mng.List_Type;

  -- List the reflog
  -- A last record with no Id indicates that the listing was aborted there
  procedure List_Reflog (Branch : in String; Reflog : in out Reflog_List);

  -- Delete a reference
  procedure Delete_Ref (Id : in String);

end Git_If;

