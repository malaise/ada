package Get_Float is

  -- Get a float (converts int if needed)
  -- May raise CONSTRAINT_ERROR
  function Get_Float (Str : String) return Float;

  type Int_Float_Rec (Is_Float : Boolean := True) is record
    case Is_Float is
      when True =>
        Float_Value : Float;
      when False =>
        Int_Value : Integer;
    end case;
  end record;

  -- Gets a float or an integer
  function Get_Int_Float (Str : String) return Int_Float_Rec;

end Get_Float;

