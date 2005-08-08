with Ada.Characters.Latin_1;
package body Text_Handler is

  function Length (T : Text) return Max_Len_Range is
  begin
    return T.Len;
  end Length;

  function Value  (T : Text) return String is
  begin
    return T.Val (1 .. Length(T));
  end Value;

  function Empty  (T : Text) return Boolean is
  begin
    return Length (T) = 0;
  end Empty;

  procedure Empty (T : in out Text) is
  begin
    T.Len := 0;
  end Empty;

  function To_Text (S : String; Max_Len : Max_Len_Range) return Text is
    T : Text (Max_Len);
  begin
    T.Len := S'Length;
    T.Val (1 .. Length(T)) := S;
    return T;
  end To_Text;

  function To_Text (C : Character; Max_Len : Max_Len_Range) return Text
   is
    T : Text (Max_Len);
  begin
    T.Len := 1;
    T.Val (1) := C;
    return T;
  end To_Text;

  function To_Text (S : String) return Text is
    T : Text (S'Length);
  begin
    T.Len := T.Max_Len;
    T.Val := S;
    return T;
  end To_Text;

  function To_Text (C : Character) return Text is
    T : Text (1);
  begin
    T.Len := 1;
    T.Val (1) := C;
    return T;
  end To_Text;


  function "&" (Left : Text; Right : Text) return Text is
    T : Text (Left.Max_Len + Right.Max_Len);
  begin
    T.Len := Length(Left) + Length(Right);
    T.Val(1 .. Length(T)) := Value(Left) & Value(Right); 
    return T;
  end "&";

  function "&" (Left : Text; Right : String) return Text is
    T : Text (Left.Max_Len + Right'Length);
  begin
    T.Len := Length(Left) + Right'Length;
    T.Val(1 .. Length(T)) := Value(Left) & Right;
    return T;
  end "&";

  function "&" (Left : String; Right : Text) return Text is
    T : Text (Left'Length + Right.Max_Len);
  begin
    T.Len := Left'Length + Length(Right);
    T.Val(1 .. Length(T)) := Left & Value(Right);
    return T;
  end "&";

  function "&" (Left : Text; Right : Character) return Text is
    T : Text (Left.Max_Len + 1);
  begin
    T.Len := Length(Left) + 1;
    T.Val(1 .. Length(T)) := Value(Left) & Right;
    return T;
  end "&";

  function "&" (Left : Character; Right : Text) return Text is
    T : Text (1 + Right.Max_Len);
  begin
    T.Len := 1 + Length(Right);
    T.Val(1 .. Length(T)) := Left & Value(Right);
    return T;
  end "&";


  function "=" (Left : Text; Right : Text) return Boolean is
  begin
    return Value(Left) = Value(Right);
  end "=";

  function "<" (Left : Text; Right : Text) return Boolean is
  begin
    return Value(Left) < Value(Right);
  end "<";

  function "<=" (Left : Text; Right : Text) return Boolean is
  begin
    return Value(Left) <= Value(Right);
  end "<=";

  function ">" (Left : Text; Right : Text) return Boolean is
  begin
    return Value(Left) > Value(Right);
  end ">";

  function ">=" (Left : Text; Right : Text) return Boolean is
  begin
    return Value(Left) >= Value(Right);
  end ">=";


  procedure Set (To : in out Text; Value : in Text) is
  begin
    To.Val(1..Length(Value)) := Text_Handler.Value(Value);
    To.Len := Length(Value);
  end Set;

  procedure Set (To : in out Text; Value : in String) is
  begin
    To.Val(1..Value'Length) := Value;
    To.Len := Value'Length;
  end Set;

  procedure Set (To : in out Text; Value : in Character) is
  begin
    To.Val(1) := Value;
    To.Len := 1;
  end Set;


  procedure Append (To : in out Text; Tail : in Text) is
  begin
    To.Val(Length(To)+1 .. Length(To)+Length(Tail)) := Value(Tail);
    To.Len := Length(To) + Length(Tail);
  end Append;

  procedure Append (To : in out Text; Tail : in String) is
  begin
    To.Val(Length(To)+1 .. Length(To)+Tail'Length) := Tail;
    To.Len := Length(To) + Tail'Length;
  end Append;

  procedure Append (To : in out Text; Tail : in Character) is
  begin
    To.Val(Length(To)+1) := Tail;
    To.Len := Length(To) + 1;
  end Append;


  procedure Amend (To : in out Text; By : in Text; 
                   Position : in Max_Len_Range) is
  begin
    Amend (To, By.Val (1 .. By.Len), Position);
  end Amend;

  procedure Amend (To : in out Text; By : in String; 
                   Position : in Max_Len_Range) is
  begin
    if Position > To.Len then
      raise Constraint_Error;
    end if;
    if Position + By'Length - 1 > To.Max_Len then
      raise Constraint_Error;
    end if;
    if Position + By'Length - 1 > To.Len then
      To.Len := Position + By'Length - 1;
      To.Val (Position .. To.Len) := By;
    else
      To.Val (Position .. Position + By'Length - 1) := By;
    end if;

  end Amend;

  procedure Amend (To : in out Text; By : in Character; 
                   Position : in Max_Len_Range) is
    S : constant String (1 .. 1) := By & "";
  begin
    Amend (To, S, Position);
  end Amend;


  function Locate (Within : Text; Fragment : Text; Occurence : Positive := 1) 
   return Max_Len_Range is
    Found_Occurence : Natural := 0;
  begin
    if Length(Within) = 0 or else Length(Fragment) = 0 then
      return 0;
    end if;
    for I in 1 .. Length(Within) - Length(Fragment) + 1 loop
      if Value(Within)(I .. I+Length(Fragment)-1) = Value(Fragment) then
        Found_Occurence := Found_Occurence + 1;
        if Found_Occurence = Occurence then
          return I;
        end if;
      end if;
    end loop;
    return 0;
  end Locate;


  function Locate (Within : Text; Fragment : String; Occurence : Positive := 1) 
   return Max_Len_Range is
    Found_Occurence : Natural := 0;
  begin
    if Length(Within) = 0 or else Fragment'Length = 0 then
      return 0;
    end if;
    for I in 1 .. Length(Within) - Fragment'Length + 1 loop
      if Value(Within)(I .. I+Fragment'Length-1) = Fragment then
        Found_Occurence := Found_Occurence + 1;
        if Found_Occurence = Occurence then
          return I;
        end if;
      end if;
    end loop;
    return 0;
  end Locate;

  function Locate (Within : Text; Fragment : Character; Occurence : Positive := 1) 
   return Max_Len_Range is
    Found_Occurence : Natural := 0;
  begin
    if Length(Within) = 0 then
      return 0;
    end if;
    for I in 1 .. Length(Within) loop
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
    From.Val(Length(From) + 1) := Ada.Characters.Latin_1.Nul;
    String_Address := From.Val'Address;
  end String_For_C;

end Text_Handler;

