-- Store several strings into one, (using a specific character as separator).
private with Unbounded_Arrays;
with Aski, As.U;
package Many_Strings is

  -- Separator of strings in a Many_String image
  --  (and also used for internal storage as a series of strings)
  Separator : Character renames Aski.Nul;

  -- A Many_String is a list of strings (possibly empty strings)
  --  separated by Separator
  -- No separator if there is no or one string
  -- <many_strings> ::= <string> [ { <separator> [ <string> ] } ]
  -- <string> ::= [ { <char> } ]
  type Many_String is tagged private;
  Empty_String : constant Many_String;

  -- Clear a Many_String
  procedure Reset (Str : in out Many_String);
  function Empty return Many_String;

  -- True if Str is really empty (no separator nor string)
  function Is_Empty (Str : Many_String) return Boolean;

  -- Init a Many_String
  function Set (From : String) return Many_String;
  function Set (From : As.U.Asu_Us) return Many_String;
  function Set (From : As.U.Asu_Array) return Many_String;
  procedure Set (Str : out Many_String; From : in String);
  procedure Set (Str : out Many_String; From : in As.U.Asu_Us);
  procedure Set (Str : out Many_String; From : in As.U.Asu_Array);

  -- Concatenation
  -- If Str is empty then set it to What
  -- otherwise concatenate to Str a Separator then What
  function Cat (Str : Many_String; What : String) return Many_String;
  function Cat (Str : Many_String; What : As.U.Asu_Us) return Many_String;
  function Cat (Str : Many_String; What : Many_String) return Many_String;
  procedure Cat (Str : in out Many_String; What : in String);
  procedure Cat (Str : in out Many_String; What : in As.U.Asu_Us);
  procedure Cat (Str : in out Many_String; What : in Many_String);

  -- Same, but do nothing of What is empty
  function Catif (Str : Many_String; What : String) return Many_String;
  function Catif (Str : Many_String; What : As.U.Asu_Us) return Many_String;
  function Catif (Str : Many_String; What : Many_String) return Many_String;
  procedure Catif (Str : in out Many_String; What : in String);
  procedure Catif (Str : in out Many_String; What : in As.U.Asu_Us);
  procedure Catif (Str : in out Many_String; What : in Many_String);

  -- String image: Separator is used to separate the strings
  function Image (Str : Many_String) return String;
  function Image (Str : Many_String) return As.U.Asu_Us;

  -- String image, using another separator to show the strings
  function Image (Str : Many_String; Separator : in Character) return String;
  function Image (Str : Many_String; Separator : in Character)
           return As.U.Asu_Us;

  -- Decode (String_Error is raised by Nth if N > Nb)
  -- An empty Many_String contains one (empty) string
  -- Str is parsed at each call except for successive calls to Nth if Str is
  --  unchanged and if N is equal or above the N of previous call
  function Nb  (Str : Many_String) return Positive;
  function Nth (Str : in Many_String; N : Positive) return String;
  function Nth (Str : in Many_String; N : Positive) return As.U.Asu_Us;

  -- Split a Many_String into strings
  function Split (Str : Many_String) return As.U.Asu_Array;

  String_Error : exception;
private

  package Asu_Unbounded_Arrays is new Unbounded_Arrays (As.U.Asu_Us,
                                                        As.U.Asu_Array);
  package Asu_Ua renames Asu_Unbounded_Arrays;

  type Many_String is new Asu_Ua.Unb_Array with null record;
  Empty_String : constant Many_String
               := (Asu_Ua.Null_Unb_Array with null record);

end Many_Strings;

