package Ada_Words is

  procedure Init;

  function Is_Separator (C : Character) return Boolean;

  function Is_Delimiter (C : Character) return Boolean;

  function Is_Keyword (Word : String) return Boolean;

end Ada_Words;

