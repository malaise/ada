with Parser;
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
  --  the input rule no,
  --  the pattern id matching,
  --  the number of words of Str that have matched a term of the pattern,
  --  a Parser iterator in the first remaining word of Str.
  -- A pattern id identifies uniquely a pattern in a rule, so the same
  --  callback can be associated to several patterns.
  -- Patterns are scanned in crescent order of ids.
  -- The returned value is transmitted to Check.
  type Rule_No is new Positive;
  type Pattern_Id is new Positive;
  type Match_Cb_Access is access function (Rule : in Rule_No;
                                           Id   : in Pattern_Id;
                                           Nb_Match : in Natural;
                                           Iter : in Parser.Iterator)
                                 return Boolean;


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
  -- Returns whether or not a match was found (and then a Cb called)
  --  and the Cb returned True.
  -- Separators may be spaces or tabs.
  -- Comparisons may be case sensitive or not
  function  Check (Rule : Rule_No;
                   Str  : String;
                   Case_Sensitive : Boolean := True) return Boolean;
  procedure Check (Rule : in Rule_No;
                   Str : in String;
                   Case_Sensitive : in Boolean := True);


  -- Get an unused rule
  -- May raise No_Rule if no rule is available.
  function Get_Free_Rule return Rule_No;

  -- Delete all patterns of a rule
  procedure Del_Rule (Rule : in Rule_No);

  -- On Set/Del.
  Invalid_Pattern : exception;

  -- On Get_Free_Rule
  No_Rule : exception;

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


