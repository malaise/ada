with Trilean;
package Ada_Words.Keywords is

  -- The different versions of the language (more and more reserved words)
  type Language_Versions is (Ada83, Ada95, Ada2005, Ada2012);
  Default_Version : constant Language_Versions := Ada2012;

  -- Is a given word a keyword or not
  -- Access, delta, digits and range may be keyword or attribute
  type Keyword_Res_List is new Trilean.Trilean;
  Maybe : constant Keyword_Res_List := Keyword_Res_List(Trilean.Maybe);

  -- Check if a word is reserved
  function Check_Keyword (Word : String;
                          Version : Language_Versions := Default_Version)
                         return Keyword_Res_List;

end Ada_Words.Keywords;

