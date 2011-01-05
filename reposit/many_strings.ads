with Ada.Characters.Latin_1;
with As.U;
-- Store several strings in one, using Ascii Nul as separator.
package Many_Strings is

  -- Separator of strings in a Many_String image
  Separator : constant Character := Ada.Characters.Latin_1.Nul;

  -- A Many_String is a list of strings (possibly empty strings)
  --  separated by Separator
  -- No separator if there is no or one string
  type Many_String is tagged private;

  -- Clear a Many_String
  procedure Reset (Str : in out Many_String);
  function Empty return Many_String;

  -- Init a Many_String
  function Set (To : String) return Many_String;
  function Set (To : As.U.Asu_Us) return Many_String;
  procedure Set (Str : in out Many_String; To : in String);
  procedure Set (Str : in out Many_String; To : in As.U.Asu_Us);

  -- Concatenation
  -- If Str is empty then it set to What
  -- otherwise concatenate to Str a Separator then What (even if What is empty)
  function Cat (Str : Many_String; What : String) return Many_String;
  function Cat (Str : Many_String; What : As.U.Asu_Us) return Many_String;
  function Cat (Str : Many_String; What : Many_String) return Many_String;
  procedure Cat (Str : in out Many_String; What : in String);
  procedure Cat (Str : in out Many_String; What : in As.U.Asu_Us);
  procedure Cat (Str : in out Many_String; What : in Many_String);

  -- String image
  function Image (Str : Many_String) return String;
  function Image (Str : Many_String) return As.U.Asu_Us;

  -- Decode (String_Error is raised by Nth if N > Nb)
  -- An empty string contains one (empty) string
  function Nb  (Str : Many_String) return Positive;
  function Nth (Str : Many_String; N : Positive) return String;
  function Nth (Str : Many_String; N : Positive) return As.U.Asu_Us;

  String_Error : exception;
private
  type Many_String is tagged record
    Ustr : As.U.Asu_Us;
  end record;
end Many_Strings;

