with Common, Data, Display;
use Common, Data;
package body Resolution is

  -- Current kind of solving
  Kind : Common.Cote_Kind;
  -- Current level in kind
  Level : Data.Eff_Cote_Range;

  -- Mattrix of booleans
  type Bool_Mattrix is array (Data.Eff_Line_Range, Data.Eff_Line_Range)
    of Boolean;
  G : Bool_Mattrix;

  -- A logical cote
  type Log_Cote_Rec is record
    No : Data.Eff_Cote_Range;
    Start, Stop : Data.Eff_Line_Range;
  end record;

  package Cote_Search is
    -- Init Level
    procedure Init;
    -- May increment Level
    function Next_Cote return Log_Cote_Rec;
  end Cote_Search;

  procedure Init is
    Start, Stop : Data.Eff_Line_Range;
  begin
    G := (others => (others => False));

    -- G(l, c)  has true is exists cote(l, c)
    -- Cote is kind
    for I in Data.Eff_Cote_Range loop
      if Kind = Common.Manufa then
        Start := Manufas(I).Start;
        Stop := Manufas(I).Stop;
      else
        Start := Designs(I).Start;
        Stop := Designs(I).Stop;
      end if;
      G(Start, Stop) := True;
      G(Stop, Start) := True;
    end loop;
    Cote_Search.Init;
  end Init;


  package body Cote_Search is
    Gp : Bool_Mattrix;

    type Bool_Vector is array (Eff_Cote_Range) of Boolean;
    Cote_Done : Bool_Vector;

    subtype Cote_Nb_Range is Natural range 0 .. Data.Eff_Cote_Range'Last;
    Nb_Done : Cote_Nb_Range;
    Last_Cote_Nb : Cote_Nb_Range;

    procedure Init is
    begin
      Level := 1;
      Gp := G;
      Cote_Done := (others => False);
      Nb_Done := 0;
      Last_Cote_Nb := 0;
    end Init;

    procedure Prodg is
      R : Bool_Mattrix;
    begin
      for I in Data.Eff_Line_Range loop
        for J in Data.Eff_Line_Range range 1 .. I loop
          for K in Data.Eff_Line_Range loop
            R(I, J) := Gp(I, K) and then G(K, J);
            exit when R(I,J);
          end loop;
          R(J,I) := R(I,J);
        end loop;
      end loop;
      Gp := R;
    end Prodg;

    function Next_Cote return Log_Cote_Rec is
      I : Cote_Nb_Range;
      C : Log_Cote_Rec;
    begin

      -- Search from previously returned cote
      I := Last_Cote_Nb;
      loop
        -- Next cote, or first at next level
        if I /= Data.Eff_Cote_Range'Last then
          I := I + 1;
        elsif Level /= Data.Eff_Cote_Range'Last then
          -- Incr level and restart from first cote
          I := 1;
          Level := Level + 1;
          Prodg;
        else
          -- Pb
          for J in Data.Eff_Cote_Range loop
            if not Cote_Done(J) then
              Display.Put_No_Way (Kind, J);
            end if;
          end loop;
          raise Abort_Error;
        end if;

        if not Cote_Done(I) then
          -- When manufa, search a design cote in gp
          if Kind = Common.Manufa then
            C.Start := Data.Designs(I).Start;
            C.Stop  := Data.Designs(I).Stop;
          else
            C.Start := Data.Manufas(I).Start;
            C.Stop  := Data.Manufas(I).Stop;
          end if;
          -- Search if cote complies with level
          if Gp(C.Start, C.Stop) then
            Last_Cote_Nb := I;
            Cote_Done(I) := True;
            C.No := I;
            return C;
          end if;
        end if;
      end loop;
    end Next_Cote;

  end Cote_Search;


  -- Search in G the sequence of cotes which make
  -- Cote.
  function Search_Way (Cote : Log_Cote_Rec)
                      return Display.Way_Vector is
    Way : Display.Way_Vector(1 .. Data.Eff_Line_Range'Last);
    subtype Way_Range is Natural range 0 .. Data.Eff_Line_Range'Last;
    Next, Prev : Way_Range;
    Index : Eff_Line_Range;

    procedure Push is
    begin
      Prev := Way(Index);
      Index := Index + 1;
      Way(Index) := Next;
      Next := 0;
    end Push;

    procedure Pop is
    begin
      Next := Way(Index);
      Index := Index - 1;
      Prev := Way(Index);
    end Pop;

  begin
    -- Init at cote.start
    Way(1) := Cote.Start;
    Index := 1;
    Prev := Cote.Start;
    Next := 0;
    loop
      -- Increment Next
      if Next /= Data.Eff_Line_Range'Last then
        Next := Next + 1;
        -- Search (cur, next) in g
        if Next /= Prev and then Next /= Way(Index)
        and then G(Way(Index), Next) then
          -- Found it
          if Index = Level then
            if Next = Cote.Stop then
              -- The last one
              Push;
              return Way(1 .. Index);
            end if;
          else
            Push;
          end if;
        end if;
      else
        -- No more cote for this current
        Pop;
      end if;
    end loop;
  end Search_Way;


  procedure Solve (Kind : in Common.Cote_Kind) is
    Current_Cote : Log_Cote_Rec;
  begin
    Resolution.Kind := Kind;
    Init;
    for I in Eff_Cote_Range loop
      -- Search next cote
      Current_Cote := Cote_Search.Next_Cote;
      -- Find way of cote
      declare
        Way : constant Display.Way_Vector := Search_Way(Current_Cote);
      begin
        -- Display way
        Display.Print (Kind, Current_Cote.No, Way);
      end;
    end loop;

  end Solve;

end Resolution;

