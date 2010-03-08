with Utils;
package Config is

  -- Editor GUI
  function Editor return String;

  -- Viewer GUI
  function Viewer return String;

  -- Diff GUI
  function Differator return String;

  -- Bookmarks
  type Bookmark_Array is array (Positive range <>) of Utils.Asu_Us;

  function Get_Bookmarks return Bookmark_Array;

  procedure Del_Bookmark (Index : in Positive);
  procedure Add_Bookmark (Bookmark : in String);

  Invalid_Config : exception;

end Config;

