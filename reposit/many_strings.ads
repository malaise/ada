-- Store several strings in one, using Ascii.Nul as separator.
package Many_Strings is

  -- A String is a list of strings separated by Ascii.Nul

  -- Concatenation
  function Cat (To : String; What : String) return String;

  -- Decode (String_Error is raised by Nth if N > Nb)
  -- An empty string contains one (empty) string
  function Nb  (Str : String) return Positive;
  function Nth (Str : String; N : Positive) return String;

  String_Error : exception;
end Many_Strings;

