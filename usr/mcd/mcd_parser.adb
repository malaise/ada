with Ada.Strings.Unbounded;
with My_Math, Queues, Sys_Calls, Lower_Str, Argument, Bool_Io, Arbitrary, Async_Stdin;
with Debug, Input_Dispatcher, Inte_Io, Real_Io, Io_Flow;
package body Mcd_Parser is
  use Mcd_Mng;

  package Unb renames Ada.Strings.Unbounded;

  subtype Item_Chrs_Rec is Mcd_Mng.Item_Rec(Mcd_Mng.Chrs);
  subtype Item_Prog_Rec is Mcd_Mng.Item_Rec(Mcd_Mng.Prog);

  -- Instruction stack for debug history
  -- 9 should be enough for user to identify the section
  package Instr_Stack is new Queues.Circ(9, Item_Chrs_Rec);

  Txt, Txts : Unb.Unbounded_String;

  -- Length of an operator
  Ope_Len : constant := 8;

  -- Length of a symbol
  subtype One_Word is String(1 .. 2);
  -- No symbol
  Nosy : constant One_Word := (others => ' ');

  -- Length of the comment
  subtype One_Comment is String (1 .. 30);

  -- One operator definition
  type One_Rec is record
    Word : One_Word;
    Comment : One_Comment;
    New_Line : Boolean;
  end record;

  -- The operators
  Words : constant array (Mcd_Mng.Operator_List) of One_Rec :=

  (Add      => ("+ ", "push B + A                    ", False),
   Sub      => ("- ", "push B - A                    ", False),
   Mult     => ("* ", "push B * A                    ", False),
   Div      => ("/ ", "push B / A                    ", False),
   Remind   => ("% ", "push B % A (rest of division) ", False),
   Pow      => ("**", "push B ** A (pow)             ", False),
   Sqrt     => (Nosy, "push Sqrt(A)                  ", False),
   Minus    => ("+-", "push -A                       ", False),
   Absv     => (Nosy, "push |A|                      ", False),
   Fact     => ("! ", "push A!                       ", True),

   Bitand   => ("&&", "push B and A (bit and)        ", False),
   Bitor    => ("||", "push B or A  (bit or)         ", False),
   Bitxor   => ("^^", "push B xor A (bit xor)        ", False),
   Bitneg   => ("~~", "push neg A   (bit neg)        ", False),
   Shl      => ("<<", "push B << A (bits shift left) ", False),
   Shr      => (">>", "push B >> A (bits shift right)", True),

   Equal    => ("= ", "push B = A                    ", False),
   Diff     => ("/=", "push B /= A                   ", False),
   Greater  => ("> ", "push B > A                    ", False),
   Smaller  => ("< ", "push B < A                    ", False),
   Greateq  => (">=", "push B >= A                   ", False),
   Smalleq  => ("<=", "push B <= A                   ", True),

   Boland   => ("& ", "push B and A                  ", False),
   Bolor    => ("| ", "push B or A                   ", False),
   Bolxor   => ("^ ", "push B xor A                  ", False),
   Bolneg   => ("~ ", "push not A                    ", True),

   Pi       => (Nosy, "push pi                       ", False),
   Sin      => (Nosy, "push Sin(A) A in radiants     ", False),
   Cos      => (Nosy, "push Cos(A) A in radiants     ", False),
   Tan      => (Nosy, "push Tan(A) A in radiants     ", False),
   Asin     => (Nosy, "push ASin(A) in radiants      ", False),
   Acos     => (Nosy, "push ACos(A) in radiants      ", False),
   Atan     => (Nosy, "push ATan(A) in radiants      ", True),

   Epsilon  => (Nosy, "push epsilon (1.0E-10)        ", False),
   Exp      => (Nosy, "push e                        ", False),
   Ln       => (Nosy, "push ln(A)                    ", False),
   Log      => (Nosy, "push log(A)                   ", True),

   Toreal   => (Nosy, "push Real(A) (A integer)      ", False),
   Tointe   => (Nosy, "push Inte(A) (A arbitrary)    ", False),
   Toarbi   => (Nosy, "push Arbi(A) (A interger)     ", False),
   Round    => (Nosy, "push Inte(A) (round A real)   ", False),
   Trunc    => (Nosy, "push Inte(A) (trunc A real)   ", False),
   Int      => (Nosy, "push int  part of A real      ", False),
   Frac     => (Nosy, "push frac part of A real      ", False),
   Dms      => (Nosy, "A.Frac -> A.MinSecFrac        ", False),
   Msd      => (Nosy, "A.MinSecFrac -> A.Frac        ", False),
   Proport  => (Nosy, "push A * B / C                ", True),
 
   Isarbi   => (Nosy, "push True if A in an arbitrari", False),
   Isinte   => (Nosy, "push True if A in an integer  ", False),
   Isreal   => (Nosy, "push True if A is a real      ", False),
   Isbool   => (Nosy, "push True if A in a boolean   ", False),
   Isstr    => (Nosy, "push True if A is a string    ", False),
   Isreg    => (Nosy, "push True if A is a register  ", False),
   Isprog   => (Nosy, "push True if A is a subprog   ", True),

   Ispos    => (Nosy, "push True if A >  0 or 0.0    ", False),
   Isnul    => (Nosy, "push True if A =  0 or 0.0    ", False),
   Isnotnul => (Nosy, "push True if A /= 0 or 0.0    ", False),
   Isneg    => (Nosy, "push True if A <  0 or 0.0    ", True),
 
   Ssize    => (Nosy, "push stack size               ", False),
   Swap     => (Nosy, "push A, push B                ", False),
   Swap3    => (Nosy, "push A, push B, push C        ", False),
   Dup      => (Nosy, "push A, push A                ", False),
   Prevtop  => (Nosy, "push previous top of stack    ", False),
   Pop      => (Nosy, "pop A                         ", False),
   Popn     => (Nosy, "pop B A times                 ", True),

   Popr     => (Nosy, "B -> regA                     ", False),
   Copyr    => (Nosy, "B -> regA, push B             ", False),
   Pushr    => (Nosy, "push regA                     ", False),
   Swapr    => (Nosy, "B <-> regA                    ", False),
   Swap2R   => (Nosy, "regB <-> regA                 ", False),
   Clearr   => (Nosy, "clear regA                    ", False),
   Clearall => (Nosy, "clear all registers           ", False),
   Emptyr   => (Nosy, "push True is regA is empty    ", False),
   Nextr    => (Nosy, "push next reg (RegA -> RegB)  ", False),
   Prevr    => (Nosy, "push prev reg (RegB -> RegA)  ", False),
   Regind   => (Nosy, "push inte index of RegA       ", False),
   Indreg   => (Nosy, "push Reg of inte index A      ", True),

   Pope     => (Nosy, "pop A push_extra A            ", False),
   Copye    => (Nosy, "pop A push_extra A push A     ", False),
   Pushle   => (Nosy, "pop_extra last  X push X      ", False),
   Pushfe   => (Nosy, "pop_extra first X push X      ", False),
   Rotle    => (Nosy, "Pushle Pope A times (>=0)     ", False),
   Rotfe    => (Nosy, "Pushfe Pope A times (>=0)     ", False),
   Esize    => (Nosy, "push extra_stack size         ", False),
   Cleare   => (Nosy, "clear extra_stack             ", True),

   Ifthen   => (Nosy, "if B then push A              ", False),
   Ifte     => (Nosy, "if C then push B else push A  ", False),
   Etfi     => (Nosy, "if A then push C else push B  ", True),

   Call     => (Nosy, "call A                        ", False),
   Ifcall   => (Nosy, "if B then call A              ", False),
   Ret      => (Nosy, "return                        ", False),
   Retn     => (Nosy, "return A levels (0=none)      ", False),
   Retall   => (Nosy, "return all levels             ", False),
   Ifret    => (Nosy, "if A return                   ", False),
   Ifretn   => (Nosy, "if B return A levels (0=none) ", False),
   Ifretall => (Nosy, "if A return all levels        ", False),
   Retacal  => (Nosy, "return and call A             ", True),

   Format   => (Nosy, "xx or xx.yyy format           ", False),
   Put      => (Nosy, "put A                         ", False),
   Newl     => (Nosy, "new line                      ", False),
   Putl     => (Nosy, "put_line A                    ", True),

   Strlen   => (Nosy, "push length of A              ", False),
   Strcat   => (Nosy, "push B & A                    ", False),
   Strsub   => (Nosy, "push C(B..A)                  ", False),
   Strloc   => (Nosy, "push B occurence of A in C    ", False),
   Strrep   => (Nosy, "push C replaced by A at pos B ", False),
   Strupp   => (Nosy, "push A in uppercase           ", False),
   Strlow   => (Nosy, "push A in LOWERCASE           ", False),
   Strmix   => (Nosy, "push A in Mixed_Case          ", False),
   Strarbi  => (Nosy, "push A converted to arbitrary ", False),
   Strinte  => (Nosy, "push A converted to integer   ", False),
   Strreal  => (Nosy, "push A converted to real      ", False),
   Strbool  => (Nosy, "push A converted to boolean   ", False),
   Strregi  => (Nosy, "push A converted to register  ", False),
   Strprog  => (Nosy, "push A converted to program   ", False),
   Strof    => (Nosy, "push formated string of A     ", False),
   Normal   => (Nosy, "push normalised string of int ", False), 
   Regex    => (Nosy, "push if B matches regex A     ", True),

   Clock    => (Nosy, "push current time             ", False),
   Dateof   => (Nosy, "int -> YYyy/mm/dd-hh:mm:ss.mmm", False),
   Daysof   => (Nosy, "int -> days-hh:mm:ss.mmm      ", False),
   Timeof   => (Nosy, "YYyy/mm/dd-hh:mm:ss.mmm -> int", True),

   Obase    => (Nosy, "set output base to A          ", False),
   Nop      => (Nosy, "no operation                  ", False),
   Getenv   => (Nosy, "push getenv(A) or False       ", False),
   Rnd      => (Nosy, "push 0.0 <= Rnd < 1.0         ", False),
   Sleep    => (Nosy, "sleep A seconds               ", False),
   Debugall => (Nosy, "set debug to A (boolean)      ", False),
   Help     => (Nosy, "put help                      ", False) );


  function Next_Item return Mcd_Mng.Item_Rec is
    Level : Natural;
    Item_Chrs : Item_Chrs_Rec;
    Item_Prog : Item_Prog_Rec;
    First_Word : Boolean;
    W : One_Word;
    C : Character;
    B : Boolean;
    I : My_Math.Inte;
    R : My_Math.Real;
    L : Positive;
    use type Unb.Unbounded_String;
  begin

    -- Get a word
    begin
      Txt := Unb.To_Unbounded_String (Input_Dispatcher.Next_Word);
    exception
      when Input_Dispatcher.String_Error =>
        -- Impossible to parse a word
        Item_Chrs.Val_Text := Unb.To_Unbounded_String (
               Input_Dispatcher.Current_String);
        Instr_Stack.Push(Item_Chrs);
      raise Parsing_Error;  
    end;
    if Debug.Debug_Level_Array(Debug.Parser) then
      Async_Stdin.Put_Line_Err ("Parser: Getting >"
               & Unb.To_String (Txt)  & "<");
    end if;
    Item_Chrs.Val_Text := Txt;

    -- Eof
    if Unb.Length (Txt) = 0 then
      if Debug.Debug_Level_Array(Debug.Parser) then
        Async_Stdin.Put_Line_Err ("Parser: Eof");
      end if;
      Item_Chrs.Val_Text := Unb.To_Unbounded_String ("EOF");
      Instr_Stack.Push(Item_Chrs);
      return (Kind => Oper, Val_Oper => Ret);
    end if;

    C := Unb.Element (Txt, 1);

    -- Strings
    if C = Input_Dispatcher.Sd
    and then Unb.Element (Txt, Unb.Length (Txt)) = Input_Dispatcher.Sd then
      -- Save raw string for history
      Instr_Stack.Push(Item_Chrs);
      -- Remove first and last string delimiters, and replace
      --  pairs of delimiters by one delimiter
      begin
        Item_Chrs.Val_Text := Unb.To_Unbounded_String (
           Input_Dispatcher.Parse_Substring (Unb.To_String (
                              Item_Chrs.Val_Text)));
      exception
        when Input_Dispatcher.String_Error =>
          raise Parsing_Error;
      end;
      return Item_Chrs;
    end if;

    -- No [ nor ] in word
    if Unb.To_String(Txt) /= "["
    and then Unb.To_String(Txt) /= "]"
    and then (Unb.Index (Txt, "[") /= 0
      or else Unb.Index (Txt, "]") /= 0) then
      Instr_Stack.Push(Item_Chrs);
      raise Parsing_Error;
    end if;

    -- Parse [ or Regi
    if Unb.Length(Txt) = 1 then
      
      if Mcd_Mng.Is_Register(C) then
        -- A register
        Instr_Stack.Push(Item_Chrs);
        return (Kind => Mcd_Mng.Regi, Val_Regi => C);
      elsif C = '[' then
        -- Parse subprogram(s)
        Txts := Unb.To_Unbounded_String ("");
        First_Word := True;
        Level := 1;
        while Level /= 0 loop
          Txt := Unb.To_Unbounded_String (Input_Dispatcher.Next_Word);
          if Debug.Debug_Level_Array(Debug.Parser) then
            Async_Stdin.Put_Line_Err ("Parser: Getting >"
                     & Unb.To_String(Txt)  & "<");
          end if;
          if Unb.To_String(Txt) = "[" then
            Level := Level + 1;
          elsif Unb.To_String(Txt) = "]" then
            Level := Level - 1;
            exit when Level = 0;
          elsif Unb.To_String(Txt) = "" then
            -- Unexpected Eof
            Item_Prog.Val_Text := Txts;
            Item_Chrs.Val_Text := Unb.To_Unbounded_String ("[ ") & Txts;
            Instr_Stack.Push(Item_Chrs);
            raise Parsing_Error;
          end if;
          -- No space before first word
          if First_Word then
            First_Word  := False;
          else
            Unb.Append (Txts,  ' ');
          end if;
          Unb.Append (Txts, Txt);
        end loop;
        Item_Prog.Val_Text := Txts;
        Item_Chrs.Val_Text := Unb.To_Unbounded_String ("[ ") & Txts
                            & Unb.To_Unbounded_String (" ]");
        Instr_Stack.Push(Item_Chrs);
        return Item_Prog;
      end if;

    end if;

    -- Parse arbitrary : @num
    if Unb.Length(Txt) >= 2 and then Unb.Element(Txt, 1) = '@' then
      declare
        N : Arbitrary.Number;
      begin
        Instr_Stack.Push(Item_Chrs);
        return (Kind => Mcd_Mng.Arbi,
                Val_Arbi => Arbitrary.Set (Unb.Slice(Txt, 2, Unb.Length(Txt))));
      exception
        when others =>
          raise Parsing_Error;
      end;
    end if;

    -- Parse Oper : string
    declare
      Op : Mcd_Mng.Operator_List;
    begin
      Op := Mcd_Mng.Operator_List'Value(Unb.To_String(Txt));
      -- Allow string only if no symbol defined
      if Words(Op).Word = Nosy then
        Instr_Stack.Push(Item_Chrs);
        return (Kind => Mcd_Mng.Oper, Val_Oper => Op);
      end if;
    exception
      -- Does not match
      when others => null;
    end;

    -- Parse Oper : symbol
    if Unb.Length (Txt) <= 2 then
      if Unb.Length (Txt) = 2 then
        W := Unb.To_String (Txt);
      else
        W(1) := Unb.Element (Txt, 1);
        W(2) := ' ';
      end if;
      for O in Mcd_Mng.Operator_List loop
        if Words(O).Word = W then
          Instr_Stack.Push(Item_Chrs);
          return (Kind => Mcd_Mng.Oper, Val_Oper => O);
        end if;
      end loop;
    end if;

    -- Parse Inte Real Bool
    begin
      Inte_Io.Get (Unb.To_String (Txt), I, L);
      if L = Unb.Length (Txt) then
        Instr_Stack.Push (Item_Chrs);
        return (Kind => Mcd_Mng.Inte, Val_Inte => I);
      end if;
    exception
      when others => null;
    end;

    begin
      Real_Io.Get (Unb.To_String (Txt), R, L);
      if L = Unb.Length (Txt)
      and then Unb.Element (Txt, L) /= '.' then
        Instr_Stack.Push (Item_Chrs);
        return (Kind => Mcd_Mng.Real, Val_Real => R);
      end if;
    exception
      when others => null;
    end;

    begin
      Bool_Io.Get (Unb.To_String (Txt), B, L);
      if L = Unb.Length (Txt) then
        Instr_Stack.Push (Item_Chrs);
        return (Kind => Mcd_Mng.Bool, Val_Bool => B);
      end if;
    exception
      when others => null;
    end;
    
    -- Cannot recognise anything
    Instr_Stack.Push(Item_Chrs);
    raise Parsing_Error;  
    
  exception
    when Io_Flow.Fifo_Error =>
      raise;
    when others =>
      raise Parsing_Error;  
  end Next_Item;


  procedure Print_Help is
    Ope_Name : String (1 .. Ope_Len);
  begin
    Io_Flow.Put_Line ("Usage: " & Argument.Get_Program_Name & " [ -f<fifo_name> | -h ]");
    Io_Flow.Put_Line ("Commands are strings read from standard input or from a fifo.");
    Io_Flow.Put_Line ("Separators are space and horizontal tab.");
    Io_Flow.Put_Line ("Comments start by '#', up to the end of line");
    Io_Flow.Put_Line ("Item ::= <arbitrary> <integer> <real> <boolean> <operator> <register> <subprogram> <string>");
    Io_Flow.Put_Line ("  <arbitrary>  ::= @<number>");
    Io_Flow.Put_Line ("  <integer>    ::= <number> | <base>#<number>#");
    Io_Flow.Put_Line ("  <register>   ::= 'a' .. 'z'  | 'A' .. 'Z'");
    Io_Flow.Put_Line ("  <subprogram> ::= '[' { <item> } ']'");
    Io_Flow.Put_Line ("  <string>     ::= ""<text>""");
    Io_Flow.Put_Line ("Operators are: Name       Action (A is top of stack, then B...)");
    for O in Mcd_Mng.Operator_List loop
      Ope_Name:= (others => ' ');
      if Words(O).Word /= Nosy then
        Ope_Name(1 .. 2) := Words(O).Word;
      else 
        Ope_Name(1 .. Mcd_Mng.Operator_List'Image(O)'Length)
                := Lower_Str(Mcd_Mng.Operator_List'Image(O));
      end if;
      Io_Flow.Put_Line ("               " & Ope_Name & "   " & Words(O).Comment);
      if Words(O).New_Line then
        Io_Flow.New_Line;
      end if;
    end loop;
  end Print_Help;
    
  procedure Dump_Stack is
    Item_Chrs : Item_Chrs_Rec;
  begin
    if not Debug.Debug_Level_Array(Debug.History) then
      return;
    end if;
    Sys_Calls.Put_Line_Error ("History:");
    loop
      begin
        Instr_Stack.Pop(Item_Chrs);
        Sys_Calls.Put_Error (Unb.To_String(Item_Chrs.Val_Text) & " ");
      exception
        when Instr_Stack.Circ_Empty =>
         exit;
      end;
    end loop;
    Sys_Calls.New_Line_Error;
  end Dump_Stack;

end Mcd_Parser;

