with Utils;
package Config is

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
    Name : Utils.Asu_Us;
    Path : Utils.Asu_Us;
  end record;
  type Bookmark_Array is array (Positive range <>) of Bookmark_Rec;

  function Get_Bookmarks return Bookmark_Array;

  procedure Del_Bookmark (Index : in Positive);
  procedure Add_Bookmark (After_Index : in Natural; Bookmark : in String);

  Invalid_Config : exception;

end Config;

