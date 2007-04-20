package Language is

  -- When ENV, UTF_8 is set if a Getenv on "LANG" gives a value
  --  ending by ".UTF-8". This is the default behaviour.
  type Language_List is (Lang_C, Lang_Utf_8, Get_Env);
  procedure Set_Language (Language : in Language_List);

  subtype Language_Set_List is Language_List range Lang_C .. Lang_Utf_8;
  function Get_Language return Language_Set_List;


  -- When a character is encoded on several bytes,
  --  language is used to detect the end of this sequence,
  --  thus to compute the length of the printed string.
  function Length (Str : String) return Natural;

end Language;
