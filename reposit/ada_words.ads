package Ada_Words is

  procedure Init;

  -- Space, Tab or newline
  function Is_Separator (C : Character) return Boolean;

  -- *, (... . Separators are not delimiters
  function Is_Delimiter (C : Character) return Boolean;

  -- Letter [ { [ underline] letter_or_digit } ]
  function Is_Identifier (Word : String) return Boolean;

  -- Reserved words
  -- Is or not. Access, delta, digits and range may be keyword or attribute
  type Keyword_Res_List is (Is_Keyword, May_Be_Keyword, Is_Not_Keyword);
  function Check_Keyword (Word : String) return Keyword_Res_List;

end Ada_Words;

