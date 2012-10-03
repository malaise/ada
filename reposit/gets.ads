package Gets is
  -- Get an Integer, a Float, a Duration
  -- Str must be a valid image with no trailing spaces,
  --  leading spaces are skipped
  -- May raise Constraint_Error
  function Get_Int (Str : String) return Integer;
  function Get_Float (Str : String) return Float;
  function Get_Dur (Str : String) return Duration;

  -- Get a float or an integer
  type Int_Float_Rec (Is_Float : Boolean := True) is record
    case Is_Float is
      when True =>
        Float_Value : Float;
      when False =>
        Int_Value : Integer;
    end case;
  end record;
  function Get_Int_Float (Str : String) return Int_Float_Rec;

  -- Get a float or an integer (convert it to float)
  function Get_Int_Or_Float (Str : String) return Float;

end Gets;

