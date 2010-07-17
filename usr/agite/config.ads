with As.U; use As.U;
package Config is

  -- X terminal
  function Xterminal return String;

  -- Editor GUI
  function Editor return String;

  -- Viewer GUI
  function Viewer return String;

  -- Diff GUI
  function Differator return String;

  -- Last/Current dir
  procedure Save_Curr_Dir (Dir : in String);
  function Prev_Dir return String;

  -- Bookmarks
  type Bookmark_Rec is record
    Name : Asu_Us;
    Path : Asu_Us;
  end record;
  type Bookmark_Array is array (Positive range <>) of Bookmark_Rec;

  function Get_Bookmarks return Bookmark_Array;

  procedure Del_Bookmark (Index : in Positive);
  procedure Add_Bookmark (After_Index : in Natural; Name, Path : in String);
  procedure Move_Bookmark (Index : in Positive; Up : in Boolean);

  Invalid_Config : exception;

end Config;

