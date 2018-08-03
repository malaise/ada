with As.U;
package Config is

  -- Check configuration (raise Invalid_Config)
  procedure Check;

  -- X terminal
  function Xterm_Name return String;
  function Xterm return String;

  -- Editor GUI
  function Editor return String;

  -- Viewer GUI
  function Viewer return String;

  -- Diff GUI
  function Differator return String;

  -- Make command
  function Make_Name return String;
  function Make return String;

  -- Patch command
  function Patch return String;

  -- Refresh period
  function Period return Duration;

  -- List tags by default
  function List_Tags return Boolean;

  -- Default history length
  function History_Len return Natural;

  -- Last/Current dir
  procedure Save_Curr_Dir (Dir : in String);
  function Prev_Dir return String;

  -- Bookmarks
  type Bookmark_Rec is record
    Name : As.U.Asu_Us;
    Path : As.U.Asu_Us;
  end record;
  type Bookmark_Array is array (Positive range <>) of Bookmark_Rec;

  function Get_Bookmarks return Bookmark_Array;
  function Get_Bookmark (Index : Positive) return Bookmark_Rec;

  procedure Del_Bookmark (Index : in Positive);
  procedure Add_Bookmark (After_Index : in Natural; Bookmark : in Bookmark_Rec);
  procedure Move_Bookmark (Index : in Positive; Up : in Boolean);

  -- Comment
  procedure Save_Comment (Text : in String);
  function Get_Comment return String;

  Io_Error : exception;
  Invalid_Config : exception;

end Config;

