with Ada.Characters.Latin_1;
package body Text_Handler is

  function Length (T : Text) return Index is
  begin
    return T.Len;
  end Length;

  function Value (T : Text) return String is
  begin
    return T.Val(1 .. T.Len);
  end Value;

  function Empty (T : Text) return Boolean is
  begin
    return T.Len = 0;
  end Empty;

  procedure Empty (T : in out Text) is
  begin
    T.Len := 0;
  end Empty;

  function To_Text (S : String; Max : Index) return Text is
    T : Text(Max);
  begin
    T.Len := S'Length;
    T.Val(1 .. T.Len) := S;
    return T;
  end To_Text;

  function To_Text (C : Character; Max : Index) return Text
   is
    T : Text(Max);
  begin
    T.Len := 1;
    T.Val(1) := C;
    return T;
  end To_Text;

  function To_Text (S : String) return Text is
    T : Text(S'Length);
  begin
    T.Len := T.Maximum_Length;
    T.Val := S;
    return T;
  end To_Text;

  function To_Text (C : Character) return Text is
    T : Text (1);
  begin
    T.Len := 1;
    T.Val(1) := C;
    return T;
  end To_Text;


  function "&" (Left : Text; Right : Text) return Text is
    T : Text(Left.Maximum_Length + Right.Maximum_Length);
  begin
    T.Len := Left.Len + Right.Len;
    T.Val(1 .. T.Len) := Value (Left) & Value (Right);
    return T;
  end "&";

  function "&" (Left : Text; Right : String) return Text is
    T : Text (Left.Maximum_Length + Right'Length);
  begin
    T.Len := Left.Len + Right'Length;
    T.Val(1 .. T.Len) := Value (Left) & Right;
    return T;
  end "&";

  function "&" (Left : String; Right : Text) return Text is
    T : Text (Left'Length + Right.Maximum_Length);
  begin
    T.Len := Left'Length + Right.Len;
    T.Val(1 .. T.Len) := Left & Value (Right);
    return T;
  end "&";

  function "&" (Left : Text; Right : Character) return Text is
    T : Text (Left.Maximum_Length + 1);
  begin
    T.Len := Left.Len + 1;
    T.Val(1 .. T.Len) := Value (Left) & Right;
    return T;
  end "&";

  function "&" (Left : Character; Right : Text) return Text is
    T : Text (1 + Right.Maximum_Length);
  begin
    T.Len := 1 + Right.Len;
    T.Val(1 .. T.Len) := Left & Value (Right);
    return T;
  end "&";


  function "=" (Left : Text; Right : Text) return Boolean is
  begin
    return Value (Left) = Value (Right);
  end "=";

  function "<" (Left : Text; Right : Text) return Boolean is
  begin
    return Value (Left) < Value (Right);
  end "<";

  function "<=" (Left : Text; Right : Text) return Boolean is
  begin
    return Value (Left) <= Value (Right);
  end "<=";

  function ">" (Left : Text; Right : Text) return Boolean is
  begin
    return Value (Left) > Value (Right);
  end ">";

  function ">=" (Left : Text; Right : Text) return Boolean is
  begin
    return Value (Left) >= Value (Right);
  end ">=";


  procedure Set (To : in out Text; Value : in Text) is
  begin
    To.Val(1 .. Value.Len) := Text_Handler.Value (Value);
    To.Len := Value.Len;
  end Set;

  procedure Set (To : in out Text; Value : in String) is
  begin
    To.Val(1 .. Value'Length) := Value;
    To.Len := Value'Length;
  end Set;

  procedure Set (To : in out Text; Value : in Character) is
  begin
    To.Val(1) := Value;
    To.Len := 1;
  end Set;


  procedure Append (To : in out Text; Tail : in Text) is
  begin
    To.Val(To.Len + 1 .. To.Len + Tail.Len) := Value(Tail);
    To.Len := To.Len + Tail.Len;
  end Append;

  procedure Append (To : in out Text; Tail : in String) is
  begin
    To.Val(To.Len +1 .. To.Len + Tail'Length) := Tail;
    To.Len := To.Len + Tail'Length;
  end Append;

  procedure Append (To : in out Text; Tail : in Character) is
  begin
    To.Val(To.Len + 1) := Tail;
    To.Len := To.Len + 1;
  end Append;


  procedure Amend (To : in out Text; By : in Text; Position : in Index) is
  begin
    Amend (To, By.Val(1 .. By.Len), Position);
  end Amend;

  procedure Amend (To : in out Text; By : in String; Position : in Index) is
  begin
    if Position > To.Len then
      raise Constraint_Error;
    end if;
    if Position + By'Length - 1 > To.Maximum_Length then
      raise Constraint_Error;
    end if;
    if Position + By'Length - 1 > To.Len then
      To.Len := Position + By'Length - 1;
      To.Val(Position .. To.Len) := By;
    else
      To.Val(Position .. Position + By'Length - 1) := By;
    end if;

  end Amend;

  procedure Amend (To : in out Text; By : in Character; Position : in Index) is
    S : constant String (1 .. 1) := By & "";
  begin
    Amend (To, S, Position);
  end Amend;


  function Locate (Within : Text; Fragment : Text;
                   Occurence : Positive := 1) return Index is
    Found_Occurence : Natural := 0;
  begin
    if Within.Len = 0 or else Fragment.Len = 0 then
      return 0;
    end if;
    for I in 1 .. Within.Len - Fragment.Len + 1 loop
      if Value(Within)(I .. I + Fragment.Len - 1) = Value(Fragment) then
        Found_Occurence := Found_Occurence + 1;
        if Found_Occurence = Occurence then
          return I;
        end if;
      end if;
    end loop;
    return 0;
  end Locate;


  function Locate (Within : Text; Fragment : String;
                   Occurence : Positive := 1) return Index is
    Found_Occurence : Natural := 0;
  begin
    if Within.Len = 0 or else Fragment'Length = 0 then
      return 0;
    end if;
    for I in 1 .. Within.Len - Fragment'Length + 1 loop
      if Value(Within)(I .. I + Fragment'Length - 1) = Fragment then
        Found_Occurence := Found_Occurence + 1;
        if Found_Occurence = Occurence then
          return I;
        end if;
      end if;
    end loop;
    return 0;
  end Locate;

  function Locate (Within : Text; Fragment : Character;
                   Occurence : Positive := 1) return Index is
    Found_Occurence : Natural := 0;
  begin
    if Within.Len = 0 then
      return 0;
    end if;
    for I in 1 .. Within.Len loop
      if Value(Within)(I) = Fragment then
        Found_Occurence := Found_Occurence + 1;
        if Found_Occurence = Occurence then
          return I;
        end if;
      end if;
    end loop;
    return 0;
  end Locate;

  procedure String_For_C (From : in out Text; String_Address : out System.Address) is
  begin
    From.Val(From.Len + 1) := Ada.Characters.Latin_1.Nul;
    String_Address := From.Val'Address;
  end String_For_C;

end Text_Handler;

