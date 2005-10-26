with Ada.Characters.Latin_1;
package body Many_Strings is

  Sep : constant Character := Ada.Characters.Latin_1.Nul;

  -- Concatenation
  function Cat (To : Many_String; What : String) return Many_String is
  begin
    return To & Sep & What;
  end Cat;

  procedure Cat (To : in out Text_Handler.Text; What : in String) is
  begin
    Text_Handler.Append (To, Sep & What);
  end Cat;


  -- Decode
  function Nb (Str : Many_String) return Positive is
    N : Natural;
  begin
    N := 1;
    for I in Str'Range loop
      if Str(I) = Sep then
        N := N + 1;
      end if;
    end loop;
    return N;
  end Nb;

  function Nth (Str : Many_String; N : Positive) return String is
    Last : constant Natural := Str'Last;

    -- Returns 0 if not found
    function Next (I : Natural) return Natural is
    begin
      for J in I .. Last loop
        if Str(J) = Sep then
          return J;
        end if;
      end loop;
      return 0;
    end Next;

    Start, Stop : Natural;
  begin
    -- Specific to empty string
    if Str = "" then
      if N = 1 then
        return "";
      else
        raise String_Error;
      end if;
    end if;

    -- Init search of string
    Start := Str'First;

    -- Next string
    for Num in 1 .. N loop
      Stop := Next (Start);
      exit when Num = N;
      -- Check for end of string
      if Stop = 0 then
        raise String_Error;
      end if;
      Start := Stop + 1;
    end loop;

    -- Got it. Stop means Last or a Sep
    if Stop = 0 then
      return Str(Start .. Last);
    else
      return Str(Start .. Stop - 1);
    end if;
  end Nth;

end Many_Strings;


