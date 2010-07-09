with Ada.Characters.Latin_1;
with As.U; use As.U;
with Text_Handler;
-- Store several strings in one, using Ascii Nul as separator.
package Many_Strings is

  -- Separator of strings in a Many_String
  Separator : constant Character := Ada.Characters.Latin_1.Nul;

  -- A Many_String is a list of strings separated by Ascii Nul
  -- A String is a Many_String of one String.
  subtype Many_String is String;

  -- Concatenation
  function Cat (To : Many_String; What : String) return Many_String;
  procedure Cat (To : in out Text_Handler.Text; What : in String);
  procedure Cat (To : in out Asu_Us; What : in String);

  -- Decode (String_Error is raised by Nth if N > Nb)
  -- An empty string contains one (empty) string
  function Nb  (Str : Many_String) return Positive;
  function Nth (Str : Many_String; N : Positive) return String;

  String_Error : exception;
end Many_Strings;

