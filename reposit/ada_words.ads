package Ada_Words is

  procedure Init;

  -- Space, Tab or newline
  function Is_Separator (C : Character) return Boolean;

  -- *, (... . Separators are not delimiters
  function Is_Delimiter (C : Character) return Boolean;

  -- Letter [ { [ underline] letter_or_digit } ]
  function Is_Identifier (Word : String) return Boolean;

  -- Reserved words
  function Is_Keyword (Word : String) return Boolean;

end Ada_Words;

