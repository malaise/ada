with Dynamic_List;
with Utils;
package Git_If is

  -- Asu stuff
  package Asu renames Utils.Asu;
  subtype Asu_Us is Utils.Asu_Us;

  -- Git version
  -- No_Git is call to "git --version" fails
  type Version_Rec is record
    Major, Medium, Minor : Natural;
  end record;
  No_Git : exception;
  function Get_Version return Version_Rec;


  -- Current Root and relative path to git, empty or "/" appended
  procedure Get_Root_And_Path (Root, Path : out Asu_Us);

  -- LIST OF FILES AND STATUS
  -- A file entry
  type File_Entry_Rec is record
    S2 : Character;
    S3 : Character;
    Name : Asu_Us;
    Kind : Character;
  end record;
  package File_Mng is new Dynamic_List (File_Entry_Rec);
  subtype File_List is File_Mng.Dyn_List.List_Type;

  -- List the files and status
  procedure List_Files (Current_Path : in String;
                        Files : in out File_List);


  -- A comment of commit
  type Comment_Array is array (Positive range <>) of Asu_Us;
  subtype Comment_1 is Comment_Array (1 .. 1);
  subtype Comment_5 is Comment_Array (1 .. 5);

  -- LOG HISTORY
  -- Git hashing number
  subtype Git_Hash is String (1 .. 41);
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
    File : Asu_Us;
  end record;
  package Commit_File_Mng is new Dynamic_List (Commit_Entry_Rec);
  subtype Commit_List is Commit_File_Mng.Dyn_List.List_Type;


  -- List detailed info on a commit
  procedure List_Commit (Hash : in Git_Hash;
                         Date : out Iso_Date;
                         Comment : out Comment_5;
                         Commit : in out Commit_List);

  -- Launch a diff (asynchronous)
  procedure Launch_Diff (Differator, File_Name : in String);

end Git_If;

