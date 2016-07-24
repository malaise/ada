with Long_Longs, My_Math;
package Gets is
  -- Get an Integer, a LL_Integer, a LL_Mod_Natural, a Float, a Real, a Duration
  -- Str must be a valid image with no trailing spaces,
  --  leading spaces are skipped
  -- May raise Constraint_Error
  function Get_Int (Str : String) return Integer;
  function Get_Llint (Str : String) return Long_Longs.Ll_Integer;
  function Get_Llmod (Str : String) return Long_Longs.Llu_Natural;
  function Get_Float (Str : String) return Float;
  function Get_Real (Str : String) return My_Math.Real;
  function Get_Dur (Str : String) return Duration;

  -- Get a float or an integer
  type Int_Or_Float_Rec (Is_Float : Boolean := True) is record
    case Is_Float is
      when True =>
        Float_Value : Float;
      when False =>
        Int_Value : Integer;
    end case;
  end record;
  function Get_Int_Or_Float (Str : String) return Int_Or_Float_Rec;

  -- Get a float or a ll_integer
  type Llint_Or_Float_Rec (Is_Float : Boolean := True) is record
    case Is_Float is
      when True =>
        Float_Value : Float;
      when False =>
        Llint_Value : Long_Longs.Ll_Integer;
    end case;
  end record;
  function Get_Llint_Or_Float (Str : String) return Llint_Or_Float_Rec;

  -- Get a float or a ll_integer (convert it to float)
  function Get_Llint_Float (Str : String) return Float;
  function Get_Int_Float (Str : String) return Float renames Get_Llint_Float;

  -- Get a real or a ll_integer
  type Llint_Or_Real_Rec (Is_Real : Boolean := True) is record
    case Is_Real is
      when True =>
        Real_Value : My_Math.Real;
      when False =>
        Llint_Value : Long_Longs.Ll_Integer;
    end case;
  end record;
  function Get_Llint_Or_Real (Str : String) return Llint_Or_Real_Rec;

  -- Get a real or a ll_integer (convert it to real)
  function Get_Llint_Real (Str : String) return My_Math.Real;
  function Get_Int_Real (Str : String) return My_Math.Real
           renames Get_Llint_Real;
end Gets;

