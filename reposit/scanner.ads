with Any_Def, Unbounded_Arrays;
package Scanner is
  -- Convert data (string) into a sequence of Anys, according to a given
  --  format (string)

  -- An array and a sequence of Anys
  type Any_Array is array (Positive range <>) of Any_Def.Any;
  package Any_Unbounded_Array is new Unbounded_Arrays (Any_Def.Any, Any_Array);
  subtype Any_Sequence is Any_Unbounded_Array.Unbounded_Array ;

  Esc : constant Character := '%';
  -- The format is a string that relies on the following types (<type>):
  -- b : Boolean
  -- t : Trilean
  -- l : Long integer (Long_Longs.ll_Integer)
  -- d : Duration
  -- r : Real (My_Math.Real)
  -- s : String (As.U.Asu_Us)
  -- i : Identifier, "letter (underscore? letter_or_digit)*" in a Asu_Us
  -- w : Word, any non-space sqience, in a Asu_Us

  --
  -- A reference to a type is indicated by the character '%',
  --  possibly followed by a length <len>, then followed by the type.
  -- If <len> is set then only <len> characters will be read,
  --  otherwise as many as possible valid (for the <type>) characters will
  -- be read.
  -- b can have a <Len>, if set it must be 5, and then " True", "True " or
  --  or "False" (in any casing) are expected, otherwise only "True" or "False"
  --  are expected (in any casing)
  -- t is the same as boolean, except that the strings "Other" and "Maybe" are
  --  also recognised
  -- l can have a <len>, [ ]*[+-]?[0-9]+ is expected
  -- d can have a <len>, [ ]*[+-]?[0-9]+(\.[0-9]+)? is expected
  -- r can have a <len>, [ ]*[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?
  --  is expected
  -- s must have a <len>, except at the end of the format (not followed by
  --  any character or type reference).
  -- i can have a <len>, then [ ]*[_a-zA-Z0-9]+ is expected,
  --  otherwise the leading [ ]* is not allowed
  -- w can have a <len>, then [ ]*[^ \t\r\n\f\v]+ is expected,
  --  otherwise the leading [ ]* is not allowed
  -- "%%" denotes the character '%'.
  -- Any other character in the format denotes the same character expected in
  --  the data
  -- Examples:
  -- - "%l%s" will read aa long integer as long as possible and will complete
  --    with a string (possibly empty)
  -- - "%2l%%%3s" will read 2 characters for a long, check that there is
  --    a '%', then read 3 characters in a string

  -- A format is invalid if:
  -- - it contains an unsupported "%[<len>]<type>" sequence
  -- - it ends with the character '%'
  -- - the "%s" sequence appears somewhere else than at the end of the foramt

  -- Compute the length of data induced by Format
  -- Raises Invalid_Format if Format is invalid
  -- Raises Unknown_Length if Format is valid but refers to a <type> without
  --  specifying the <len>
  function Length (Format : String) return Natural;

  -- Scan the Data according to Format, return the sequence of refered data
  -- Raises Invalid_Format if Format is invalid
  -- Raises Invalid_Data if Data does not match Format
  function Scan (Data : String; Format : String) return Any_Sequence;

  Invalid_Format, Unknown_Length, Invalid_Data : exception;

end Scanner;

