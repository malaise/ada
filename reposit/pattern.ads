with Parser;
package Pattern is

  -- Look successively in a rule (set of patterns) to find a pattern
  --   with which a given string matches.
  type Rule_No is private;
  function Image (Rule : Rule_No) return String;

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
  --  the Id4Cb of the matching pattern,
  --  the number of words of Str that have matched a term of the pattern,
  --  a Parser iterator in the first remaining word of Str (tail can be
  --  retrieved by Current_Word, then Next_Word...).
  -- A pattern id identifies uniquely a pattern in a rule, so the same
  --  callback can be associated to several patterns.
  -- Patterns are scanned in crescent order of ids.
  -- The returned value is transmitted to Check.
  type Pattern_Id4Cb is new Natural;
  Same_Id : constant Pattern_Id4Cb := 0;
  subtype Pattern_Id is Pattern_Id4Cb range 1 .. Pattern_Id4Cb'Last;
  type Match_Cb_Access is access function (Rule     : Rule_No;
                                           Id       : Pattern_Id;
                                           Nb_Match : Natural;
                                           Iter     : Parser.Iterator)
                                 return Boolean;


  -- Define a pattern in a rule.
  -- Id4Cb will be the Id provided to Match_Cb when pattern matches.
  -- It can be Same_Id (same value as Id) or another value.
  -- May raise Invalid_Pattern if Pattern is not valid.
  -- May raise Pattern_Exists if this pattern Id is already set.
  procedure Set (Rule     : in Rule_No;
                 Id       : in Pattern_Id;
                 Pattern  : in String;
                 Match_Cb : in Match_Cb_Access;
                 Id4Cb    : in Pattern_Id4Cb := Same_Id);


  -- Delete a pattern of a rule.
  -- May raise Invalid_Pattern if the Id is not set.
  procedure Del (Rule : in Rule_No; Id : in Pattern_Id);

  -- Returns the String image of a pattern
  -- May raise Invalid_Pattern if Pattern is not valid.
  function Image (Rule : Rule_No; Id : Pattern_Id) return String;

  -- Return the Id which will be provided to the callback.
  -- May raise Invalid_Pattern if Pattern is not valid.
  function Get_Id4Cb (Rule : Rule_No; Id : Pattern_Id) return Pattern_Id;

  -- Check Str versus patterns of a rule, in crescent order if Ids.
  -- Returns whether or not a match was found (and then a Cb called)
  --  and the Cb returned True.
  -- Separators may be spaces or tabs.
  -- Comparisons may be case sensitive or not
  function  Check (Rule : Rule_No;
                   Str  : String;
                   Case_Sensitive : Boolean := True) return Boolean;
  procedure Check (Rule : in Rule_No;
                   Str  : in String;
                   Case_Sensitive : in Boolean := True);


  -- Get an unused rule
  -- May raise No_Rule if no rule is available.
  function Get_Free_Rule return Rule_No;

  -- Delete all patterns of a rule
  procedure Del_Rule (Rule : in Rule_No);

  -- On Set/Del.
  Invalid_Pattern : exception;

  -- On Set
  Pattern_Exists : exception;

  -- On Get_Free_Rule if no more free rule
  -- On any other call Rule has not been got.
  No_Rule : exception;

  -- Example:
  -- Set (R, 10, "get alias", Cb);
  -- Set (R, 20, "get", Cb);
  -- Set (R, 30, "notify", Cb);
  -- Set (R, 40, "quit", Quit);
  -- Set (R, 41, "exit", Quit, 40);
  -- Set (R, 100, "", Default);
  -- Check (R, "get foo");      calls Cb (R, 20, 1);         tail: foo
  -- Check (R, "get alias foo") calls Cb (R, 10, 2);         tail: foo
  -- Check (R, "notify alias")  calls Cb (R, 30, 1);         tail: alias
  -- Check (R, "notif alias")   calls Default (R, 100, 0);   tail: notif alias
  -- Check (R, "quit")          calls Quit (R, 40, 1);       tail:
  -- Check (R, "exit")          calls Quit (R, 40, 1);       tail:
  -- Set (R, 10, "get [ alias ]", Cb);
  -- Del (R, 20);
  -- Del (R, 30);
  -- Check (R, "get foo  ");      calls Cb (R, 10, 1);       tail: foo
  -- Check (R, "get   alias foo") calls Cb (R, 10, 2);       tail: foo

private

  type Rule_No_Range is new Natural;
  subtype Valid_Rule_No_Range is Rule_No_Range range 1 .. Rule_No_Range'Last;
  -- Valid numbers are positive
  type Rule_No is record
    No : Rule_No_Range := 0;
  end record;
  No_Rule_No : constant Rule_No := (No => 0);

end Pattern;

