package Pattern is

  -- Look successively in a rule (set of patterns) to find a pattern
  --   with which a given string matches.
  -- A pattern is a list of terms separated by spaces or tabs
  --  (an empty pattern matches anything).
  -- A term is string and may be:
  --  present once                          term
  --  optionnal                (0 or 1)     [ term ]
  --  repetitive               (1 or more ) { term }
  --  optionnal and repetitive (1 or more ) [ { term } ]

  -- For parsing patterns and string
  function Is_Sep (C : Character) return Boolean;

  -- The callback associated with the first matching pattern is called with
  --  the input rule no and str,
  --  the pattern id matching,
  --  the number of words of Str that have matched a term of the pattern,
  --  the index in Str of last matching character (0 if matching empty pattern).
  -- A pattern id identifies uniquely a patternin a rule, so the same callback
  --  can be associated to several patterns.
  -- Patterns are scanned in crescent order of Ids.
  type Rule_No is new Positive;
  type Pattern_Id is new Positive;
  type Match_Cb_Access is access procedure (Rule : in Rule_No;
                                            Str  : in String;
                                            Id   : in Pattern_Id;
                                            Nb_Match : in Natural;
                                            Index    : in Natural);

  -- (Re)define a pattern in a rule.
  -- May raise Invalid_Pattern if Pattern is not valid.
  procedure Set (Rule : in Rule_No;
                 Id : in Pattern_Id;
                 Pattern  : in String;
                 Match_Cb : in Match_Cb_Access);


  -- Delete a pattern of a rule.
  -- May raise Invalid_Pattern if the Id is not set.
  procedure Del (Rule : in Rule_No; Id : in Pattern_Id);



  -- Check Str versus patterns of a rule, in crescent order if Ids.
  -- Returns whether or not a match was found (and a Cb called).
  -- Separators may be spaces or tabs.
  function  Check (Rule : Rule_No; Str : String) return Boolean;
  procedure Check (Rule : in Rule_No; Str : in String);


  -- On Set/Del.
  Invalid_Pattern : exception;

  -- Example:
  -- Set (10, "get alias", Cb);
  -- Set (20, "get", Cb);
  -- Set (30, "notify", Cb);
  -- Set ("", 100, Default);
  -- Check ("get toto");      calls Cb (20, 1);
  -- Check ("get alias toto") calls Cb (10, 2);
  -- Check ("notify alias")   calls Cb (30, 1);
  -- Check ("notif alias")    calls Default (100, 0);
  -- Set (10, "get [ alias ]", Cb);
  -- Del (20);
  -- Del (30);
  -- Check ("get toto  ");      calls Cb (10, 1);
  -- Check ("get   alias toto") calls Cb (10, 2);

end Pattern;


