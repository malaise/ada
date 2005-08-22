with Upper_Str, Lower_Str, Mixed_Str, String_Mng;
separate(Mcd_Mng)

package body Strings is


  procedure Check_Chrs (X : in Item_Rec) is
  begin
    if X.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
  end Check_Chrs;

  procedure Check_Inte (X : in Item_Rec) is
  begin
    if X.Kind /= Inte then
      raise Invalid_Argument;
    end if;
  end Check_Inte;

  function Strcat (S1, S2 : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
    use type Unb.Unbounded_String;
  begin
    Check_Chrs(S1);
    Check_Chrs(S2);

    Res.Val_Text := S1.Val_Text & S2.Val_Text;
    return Res;
  exception
    when Constraint_Error =>
      raise String_Len;
  end Strcat;

  function Strsub (S, I1, I2 : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Inte(I1);
    Check_Inte(I2);

    -- Empty string
    if I2.Val_Inte = 0 or else I2.Val_Inte < I1.Val_Inte then
      return Res;
    end if;

    if I1.Val_Inte < 1
    or else I1.Val_Inte > My_Math.Inte(Unb.Length (S.Val_Text)) then
      raise Argument_Mismatch;
    end if;
    if I2.Val_Inte < 1
    or else I2.Val_Inte > My_Math.Inte(Unb.Length (S.Val_Text)) then
      raise Argument_Mismatch;
    end if;
    Res.Val_Text := Unb.To_Unbounded_String (
          Unb.Slice (S.Val_Text, Positive(I1.Val_Inte), Natural(I2.Val_Inte)));
    return Res;
  end Strsub;

  function Strloc (Occ, Pat, S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Inte);
    Pat_Len : Natural;
    Found_Occurence : My_Math.Inte := 0;
  begin
    Check_Chrs(S);
    Check_Chrs(Pat);
    Check_Inte(Occ);

    if Occ.Val_Inte < 1 or else Occ.Val_Inte > My_Math.Inte(Positive'Last) then
      raise Invalid_Argument;
    end if;

    Res.Val_Inte := My_Math.Inte(
            String_Mng.Locate (Unb.To_String(S.Val_Text),
                               Unb.To_String(Pat.Val_Text),
                               Positive(Occ.Val_Inte)));
    return Res;
  end Strloc;
  
  function Strrep (I, Pat, S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Chrs(Pat);
    Check_Inte(I);

    if I.Val_Inte < 1 or else I.Val_Inte > My_Math.Inte(Unb.Length (S.Val_Text)) then
      raise Argument_Mismatch;
    end if;

    Res.Val_Text := Unb.Overwrite (S.Val_Text, Positive (I.Val_Inte),
                                   Unb.To_String (Pat.Val_Text));
    return Res;
  end Strrep;

  function Strlen (S : Item_Rec) return Item_Rec is
  begin
    Check_Chrs(S);
    return (Kind => Inte, Val_Inte => My_Math.Inte(Unb.Length (S.Val_Text)));
  end Strlen;

  function Strupp (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Text := Unb.To_Unbounded_String (Upper_Str (Unb.To_String (S.Val_Text)));
    return Res;
  end Strupp;

  function Strlow (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Text := Unb.To_Unbounded_String (Lower_Str (Unb.To_String (S.Val_Text)));
    return Res;
  end Strlow;

  function Strmix (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Text := Unb.To_Unbounded_String (Mixed_Str (Unb.To_String (S.Val_Text)));
    return Res;
  end Strmix;

end Strings;

