with Trilean;
package Ada_Words is

  procedure Init;

  -- Space, Tab or newline
  function Is_Separator (C : Character) return Boolean;

  -- *, (... . Separators are not delimiters
  function Is_Delimiter (C : Character) return Boolean;

  -- Letter [ { [ underline] letter_or_digit } ]
  function Is_Identifier (Word : String) return Boolean;

  -- Reserved words

  -- The different versions of the language (more and more reserved words)
  type Language_Versions is (Ada83, Ada95, Ada2005, Ada2012);
  Default_Version : constant Language_Versions := Ada2012;

  -- Is or not. Access, delta, digits and range may be keyword or attribute
  type Keyword_Res_List is new Trilean.Trilean;
  Maybe : constant Keyword_Res_List := Keyword_Res_List(Trilean.Maybe);

  -- Check if a word is reserved
  function Check_Keyword (Word : String;
                          Version : Language_Versions := Default_Version)
                         return Keyword_Res_List;

end Ada_Words;

