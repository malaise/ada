with As.U, My_Math, Queues, Basic_Proc, Lower_Str, Argument, Bool_Io, Arbitrary,
     Arbitrary.Fractions, Async_Stdin, String_Mng;
with Debug, Input_Dispatcher, Inte_Io, Io_Flow;
package body Mcd_Parser is
  use type As.U.Asu_Us;
  use Mcd_Mng;

  subtype Item_Chrs_Rec is Mcd_Mng.Item_Rec(Mcd_Mng.Chrs);
  subtype Item_Prog_Rec is Mcd_Mng.Item_Rec(Mcd_Mng.Prog);

  -- Instruction stack for debug history
  -- 9 should be enough for user to identify the section
  package Instr_Stack is new Queues.Circ(9, Item_Chrs_Rec);
  Stack : Instr_Stack.Circ_Type;

  Txt, Txts : As.U.Asu_Us;

  -- Length of an operator
  Ope_Len : constant := 8;

  -- Length of a symbol
  subtype One_Word is String(1 .. 2);
  -- No symbol
  Nosy : constant One_Word := (others => ' ');

  -- Length of the comment
  subtype One_Comment is String (1 .. 50);

  -- One operator definition
  type One_Rec is record
    Word : One_Word;
    Comment : One_Comment;
    Extra_Comment : As.U.Asu_Us;
    New_Line : Boolean;
  end record;

  -- The operators
  Words : constant array (Mcd_Mng.Operator_List) of One_Rec :=
  -- Basic operations on numbers
  (Add      => ("+ ", "push B + A                                        ", As.U.Asu_Null, False),
   Sub      => ("- ", "push B - A                                        ", As.U.Asu_Null, False),
   Mult     => ("* ", "push B * A                                        ", As.U.Asu_Null, False),
   Div      => ("/ ", "push B / A                                        ", As.U.Asu_Null, False),
   Remind   => ("% ", "push B % A (rest of division)                     ", As.U.Asu_Null, False),
   Pow      => ("**", "push B ** A (pow)                                 ", As.U.Asu_Null, False),
   Sqrt     => (Nosy, "push Sqrt(A)                                      ", As.U.Asu_Null, False),
   Minus    => ("+-", "push -A                                           ", As.U.Asu_Null, False),
   Absv     => (Nosy, "push |A| (absolute value)                         ", As.U.Asu_Null, False),
   Fact     => ("! ", "push A! (factorial)                               ", As.U.Asu_Null, True),
   -- Bits operations
   Bitand   => ("&&", "push B and A (bit and)                            ", As.U.Asu_Null, False),
   Bitor    => ("||", "push B or A  (bit or)                             ", As.U.Asu_Null, False),
   Bitxor   => ("^^", "push B xor A (bit xor)                            ", As.U.Asu_Null, False),
   Bitneg   => ("~~", "push neg A   (bit neg)                            ", As.U.Asu_Null, False),
   Shl      => ("<<", "push B << A (bits shift left)                     ", As.U.Asu_Null, False),
   Shr      => (">>", "push B >> A (bits shift right)                    ", As.U.Asu_Null, True),
   -- Comparisons
   Equal    => ("= ", "push B = A                                        ", As.U.Asu_Null, False),
   Diff     => ("/=", "push B /= A                                       ", As.U.Asu_Null, False),
   Greater  => ("> ", "push B > A                                        ", As.U.Asu_Null, False),
   Smaller  => ("< ", "push B < A                                        ", As.U.Asu_Null, False),
   Greateq  => (">=", "push B >= A                                       ", As.U.Asu_Null, False),
   Smalleq  => ("<=", "push B <= A                                       ", As.U.Asu_Null, True),
   -- Boolean operations
   Boland   => ("& ", "push B and A (boolean)                            ", As.U.Asu_Null, False),
   Bolor    => ("| ", "push B or A  (boolean)                            ", As.U.Asu_Null, False),
   Bolxor   => ("^ ", "push B xor A (boolean)                            ", As.U.Asu_Null, False),
   Bolneg   => ("~ ", "push not A   (boolean)                            ", As.U.Asu_Null, True),
   -- Trigonometry
   Pi       => (Nosy, "push pi                                           ", As.U.Asu_Null, False),
   Sin      => (Nosy, "push Sin(A) A in radiants                         ", As.U.Asu_Null, False),
   Cos      => (Nosy, "push Cos(A) A in radiants                         ", As.U.Asu_Null, False),
   Tan      => (Nosy, "push Tan(A) A in radiants                         ", As.U.Asu_Null, False),
   Asin     => (Nosy, "push ASin(A) in radiants                          ", As.U.Asu_Null, False),
   Acos     => (Nosy, "push ACos(A) in radiants                          ", As.U.Asu_Null, False),
   Atan     => (Nosy, "push ATan(A) in radiants                          ", As.U.Asu_Null, True),
   -- Logarithm
   Epsilon  => (Nosy, "push epsilon (1.0E-10)                            ", As.U.Asu_Null, False),
   Exp      => (Nosy, "push e (exponential)                              ", As.U.Asu_Null, False),
   Ln       => (Nosy, "push ln(A)  (neper logarithm)                     ", As.U.Asu_Null, False),
   Log      => (Nosy, "push log(A) (base 10 logarithm)                   ", As.U.Asu_Null, True),
   -- Numerical conversion
   Toreal   => (Nosy, "push Real(A) (A integer)                          ", As.U.Asu_Null, False),
   Tointe   => (Nosy, "push Inte(A) (A arbitrary)                        ", As.U.Asu_Null, False),
   Toarbi   => (Nosy, "push Arbi(A) (A interger)                         ", As.U.Asu_Null, False),
   Round    => (Nosy, "push Inte(A) (round A real)                       ", As.U.Asu_Null, False),
   Trunc    => (Nosy, "push Inte(A) (trunc A real)                       ", As.U.Asu_Null, False),
   Int      => (Nosy, "push int  part of A real                          ", As.U.Asu_Null, False),
   Frac     => (Nosy, "push frac part of A real                          ", As.U.Asu_Null, False),
   Maxint   => (Nosy, "push max integer value                            ", As.U.Asu_Null, False),
   Minint   => (Nosy, "push min integer value                            ", As.U.Asu_Null, False),
   Roundif  => (Nosy, "push Round(A) if A (real) within inte else push A ", As.U.Asu_Null, False),
   Dms      => (Nosy, "A.frac -> A.MinSecMillis                          ", As.U.Asu_Null, False),
   Msd      => (Nosy, "A.MinSecMillis -> A.frac                          ", As.U.Asu_Null, False),
   Mkfrac   => (Nosy, "push Fraction B:A (A, B arbi)                     ", As.U.Asu_Null, False),
   Numerof  => (Nosy, "push numerator of Fraction A                      ", As.U.Asu_Null, False),
   Denomof  => (Nosy, "push denominator of Fraction A                    ", As.U.Asu_Null, False),
   Proport  => (Nosy, "push A * B / C                                    ", As.U.Asu_Null, False),
   Roundat  => (Nosy, "push B (real) rounded at A (integer) digits       ", As.U.Asu_Null, True),
   -- Tests on type and value
   Isarbi   => (Nosy, "push True if A in an arbitrary                    ", As.U.Asu_Null, False),
   Isfrac   => (Nosy, "push True if A in a Fraction                      ", As.U.Asu_Null, False),
   Isinte   => (Nosy, "push True if A in an integer                      ", As.U.Asu_Null, False),
   Isreal   => (Nosy, "push True if A is a real                          ", As.U.Asu_Null, False),
   Isbool   => (Nosy, "push True if A in a boolean                       ", As.U.Asu_Null, False),
   Isstr    => (Nosy, "push True if A is a string                        ", As.U.Asu_Null, False),
   Isreg    => (Nosy, "push True if A is a register                      ", As.U.Asu_Null, False),
   Isprog   => (Nosy, "push True if A is a subprog                       ", As.U.Asu_Null, False),
   Ispos    => (Nosy, "push True if A >  0 or 0.0                        ", As.U.Asu_Null, False),
   Isnul    => (Nosy, "push True if A =  0 or 0.0                        ", As.U.Asu_Null, False),
   Isnotnul => (Nosy, "push True if A /= 0 or 0.0                        ", As.U.Asu_Null, False),
   Isneg    => (Nosy, "push True if A <  0 or 0.0                        ", As.U.Asu_Null, True),
   -- Main stack management
   Ssize    => (Nosy, "push stack size                                   ", As.U.Asu_Null, False),
   Swap     => (Nosy, "push A, push B                                    ", As.U.Asu_Null, False),
   Swap3    => (Nosy, "push A, push B, push C                            ", As.U.Asu_Null, False),
   Dup      => (Nosy, "push A, push A                                    ", As.U.Asu_Null, False),
   Prevtop  => (Nosy, "push previous top of stack                        ", As.U.Asu_Null, False),
   Pop      => (Nosy, "pop A                                             ", As.U.Asu_Null, False),
   Popn     => (Nosy, "pop B A times                                     ", As.U.Asu_Null, True),
   -- Registers and arrays
   Popr     => (Nosy, "B -> regA                                         ", As.U.Asu_Null, False),
   Copyr    => (Nosy, "B -> regA, push B                                 ", As.U.Asu_Null, False),
   Pushr    => (Nosy, "push regA                                         ", As.U.Asu_Null, False),
   Swapr    => (Nosy, "B <-> regA                                        ", As.U.Asu_Null, False),
   Swap2R   => (Nosy, "regB <-> regA                                     ", As.U.Asu_Null, False),
   Clearr   => (Nosy, "clear regA                                        ", As.U.Asu_Null, False),
   Clearall => (Nosy, "clear all registers                               ", As.U.Asu_Null, False),
   Emptyr   => (Nosy, "push True if regA is empty                        ", As.U.Asu_Null, False),
   Nextr    => (Nosy, "push next reg (RegA -> RegB)                      ", As.U.Asu_Null, False),
   Prevr    => (Nosy, "push prev reg (RegB -> RegA)                      ", As.U.Asu_Null, False),
   Regind   => (Nosy, "push integer index of RegA                        ", As.U.Asu_Null, False),
   Indreg   => (Nosy, "push Reg of integer index A                       ", As.U.Asu_Null, False),
   Popa     => (Nosy, "C -> regB[A]                                      ", As.U.Asu_Null, False),
   Pusha    => (Nosy, "push regB[A]                                      ", As.U.Asu_Null, False),
   Cleara   => (Nosy, "clear regB[A]                                     ", As.U.Asu_Null, False),
   Emptya   => (Nosy, "push True if regB[A] is empty                     ", As.U.Asu_Null, True),
   -- Extra stack
   Pope     => (Nosy, "pop A push_extra A                                ", As.U.Asu_Null, False),
   Copye    => (Nosy, "pop A push_extra A push A                         ", As.U.Asu_Null, False),
   Pushle   => (Nosy, "pop_extra last  X push X                          ", As.U.Asu_Null, False),
   Pushfe   => (Nosy, "pop_extra first X push X                          ", As.U.Asu_Null, False),
   Rotle    => (Nosy, "pushle Pope A times (>=0)                         ", As.U.Asu_Null, False),
   Rotfe    => (Nosy, "pushfe Pope A times (>=0)                         ", As.U.Asu_Null, False),
   Esize    => (Nosy, "push extra_stack size                             ", As.U.Asu_Null, False),
   Cleare   => (Nosy, "clear extra_stack                                 ", As.U.Asu_Null, True),
   -- Conditions
   Ifthen   => (Nosy, "if B then push A                                  ", As.U.Asu_Null, False),
   Ifte     => (Nosy, "if C then push B else push A                      ", As.U.Asu_Null, False),
   Etfi     => (Nosy, "if A then push C else push B                      ", As.U.Asu_Null, True),
   -- Subprograms
   Call     => (Nosy, "call A                                            ", As.U.Asu_Null, False),
   Ifcall   => (Nosy, "if B then call A                                  ", As.U.Asu_Null, False),
   Include  => (Nosy, "include content of file A (string) as a subprog   ", As.U.Asu_Null, False),
   Ret      => (Nosy, "return                                            ", As.U.Asu_Null, False),
   Retn     => (Nosy, "return A levels (0=none)                          ", As.U.Asu_Null, False),
   Retall   => (Nosy, "return all levels                                 ", As.U.Asu_Null, False),
   Ifret    => (Nosy, "if A return                                       ", As.U.Asu_Null, False),
   Ifretn   => (Nosy, "if B return A levels (0=none)                     ", As.U.Asu_Null, False),
   Ifretall => (Nosy, "if A return all levels                            ", As.U.Asu_Null, False),
   Retacal  => (Nosy, "return and call A                                 ", As.U.Asu_Null, False),
   Callbrk  => (Nosy, "set program to be called after break before exit  ", As.U.Asu_Null, True),
   -- Output
   Format   => (Nosy, "set format to A (for put and strof)               ",
    As.U.Tus ("xx (for integers) or xx.yyy (for reals) "), False),
   Obase    => (Nosy, "set output base to A (for put and strof)          ", As.U.Asu_Null, False),
   Put      => (Nosy, "put A                                             ", As.U.Asu_Null, False),
   Newl     => (Nosy, "new line                                          ", As.U.Asu_Null, False),
   Putl     => (Nosy, "put_line A                                        ", As.U.Asu_Null, True),
   -- String management and conversions
   Strlen   => (Nosy, "push length of A                                  ", As.U.Asu_Null, False),
   Strcat   => (Nosy, "push B & A                                        ", As.U.Asu_Null, False),
   Strsub   => (Nosy, "push C(B..A)                                      ", As.U.Asu_Null, False),
   Strloc   => (Nosy, "push B occurence of A in C                        ", As.U.Asu_Null, False),
   Strrep   => (Nosy, "push D with its slice C .. B replaced by A        ", As.U.Asu_Null, False),
   Strins   => (Nosy, "push C after inserting A at pos B in it           ", As.U.Asu_Null, False),
   Strovw   => (Nosy, "push C overwriten by A from pos B                 ", As.U.Asu_Null, False),
   Strdel   => (Nosy, "push C after deleting C(B..A)                     ", As.U.Asu_Null, False),
   Strupp   => (Nosy, "push A in uppercase                               ", As.U.Asu_Null, False),
   Strlow   => (Nosy, "push A in LOWERCASE                               ", As.U.Asu_Null, False),
   Strmix   => (Nosy, "push A in Mixed_Case                              ", As.U.Asu_Null, False),
   Strarbi  => (Nosy, "push A converted to arbitrary                     ", As.U.Asu_Null, False),
   Strfrac  => (Nosy, "push A converted to Fraction                      ", As.U.Asu_Null, False),
   Strinte  => (Nosy, "push A converted to integer                       ", As.U.Asu_Null, False),
   Strreal  => (Nosy, "push A converted to real                          ", As.U.Asu_Null, False),
   Strbool  => (Nosy, "push A converted to boolean                       ", As.U.Asu_Null, False),
   Strregi  => (Nosy, "push A converted to register                      ", As.U.Asu_Null, False),
   Strprog  => (Nosy, "push A converted to program                       ", As.U.Asu_Null, False),
   Strof    => (Nosy, "push formated string of A                         ", As.U.Asu_Null, False),
   Normal   => (Nosy, "push normalised string of D (integer)             ",
    As.U.Tus ("C is Nb(positive), B is Right(boolean), A is Pad(string)"), False),
   Regmatch => (Nosy, "push where B matches regex A                      ", As.U.Asu_Null, True),
   -- Time
   Clock    => (Nosy, "push current time                                 ", As.U.Asu_Null, False),
   Dateof   => (Nosy, "int -> YYyy/mm/dd-hh:mm:ss.mmm                    ", As.U.Asu_Null, False),
   Daysof   => (Nosy, "int -> days-hh:mm:ss.mmm                          ", As.U.Asu_Null, False),
   Timeof   => (Nosy, "YYyy/mm/dd-hh:mm:ss.mmm -> int                    ", As.U.Asu_Null, True),
   -- Miscelaneous
   Nop      => (Nosy, "no operation                                      ", As.U.Asu_Null, False),
   Getenv   => (Nosy, "push getenv(A) or False                           ", As.U.Asu_Null, False),
   Read     => (Nosy, "push lines of file as strings (first on top)      ", As.U.Asu_Null, False),
   Rnd      => (Nosy, "push 0.0 <= Rnd < 1.0                             ", As.U.Asu_Null, False),
   Sleep    => (Nosy, "sleep A seconds                                   ", As.U.Asu_Null, False),
   Version  => (Nosy, "push current version (string)                     ", As.U.Asu_Null, False),
   Setexit  => (Nosy, "set exit code to A (natural)                      ", As.U.Asu_Null, False),
   Debugall => (Nosy, "set debug to A (boolean)                          ", As.U.Asu_Null, False),
   Help     => (Nosy, "put help                                          ", As.U.Asu_Null, False) );


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
  begin

    -- Get a word
    begin
      Txt := As.U.Tus (Input_Dispatcher.Next_Word);
    exception
      when Input_Dispatcher.String_Error =>
        -- Impossible to parse a word
        Item_Chrs.Val_Text := As.U.Tus (Input_Dispatcher.Current_String);
        Instr_Stack.Push (Stack, Item_Chrs);
      raise Parsing_Error;
    end;
    if Debug.Debug_Level_Array(Debug.Parser) then
      Async_Stdin.Put_Line_Err ("Parser: Getting >" & Txt.Image  & "<");
    end if;
    Item_Chrs.Val_Text := Txt;

    -- Eof
    if Txt.Length = 0 then
      if Debug.Debug_Level_Array(Debug.Parser) then
        Async_Stdin.Put_Line_Err ("Parser: Eof");
      end if;
      Item_Chrs.Val_Text := As.U.Tus ("EOF");
      Instr_Stack.Push (Stack, Item_Chrs);
      return (Kind => Oper, Val_Oper => Ret);
    end if;

    C := Txt.Element (1);

    -- Strings
    if C = Input_Dispatcher.Sd
    and then Txt.Element (Txt.Length) = Input_Dispatcher.Sd then
      -- Save raw string for history
      Instr_Stack.Push (Stack, Item_Chrs);
      -- Remove first and last string delimiters, and replace
      --  pairs of delimiters by one delimiter
      begin
        Item_Chrs.Val_Text := As.U.Tus (
           Input_Dispatcher.Parse_Substring (Item_Chrs.Val_Text.Image));
      exception
        when Input_Dispatcher.String_Error =>
          raise Parsing_Error;
      end;
      return Item_Chrs;
    end if;

    -- No [ nor ] in word
    if Txt.Image /= "["
    and then Txt.Image /= "]"
    and then (Txt.Locate ("[") /= 0
      or else Txt.Locate ("]") /= 0) then
      Instr_Stack.Push (Stack, Item_Chrs);
      raise Parsing_Error;
    end if;

    -- Parse [ or Regi
    if Txt.Length = 1 then

      if Mcd_Mng.Is_Register(C) then
        -- A register
        Instr_Stack.Push (Stack, Item_Chrs);
        return (Kind => Mcd_Mng.Regi, Val_Regi => C);
      elsif C = '[' then
        -- Parse subprogram(s)
        Txts.Set_Null;
        First_Word := True;
        Level := 1;
        while Level /= 0 loop
          Txt := As.U.Tus (Input_Dispatcher.Next_Word);
          if Debug.Debug_Level_Array(Debug.Parser) then
            Async_Stdin.Put_Line_Err ("Parser: Getting >"
                     & Txt.Image  & "<");
          end if;
          if Txt.Image = "[" then
            Level := Level + 1;
          elsif Txt.Image = "]" then
            Level := Level - 1;
            exit when Level = 0;
          elsif Txt.Image = "" then
            -- Unexpected Eof
            Item_Prog.Val_Text := Txts;
            Item_Chrs.Val_Text := As.U.Tus ("[ ") & Txts;
            Instr_Stack.Push (Stack, Item_Chrs);
            raise Parsing_Error;
          end if;
          -- No space before first word
          if First_Word then
            First_Word  := False;
          else
            Txts.Append (' ');
          end if;
          Txts.Append (Txt);
        end loop;
        Item_Prog.Val_Text := Txts;
        Item_Chrs.Val_Text := As.U.Tus ("[ ") & Txts & As.U.Tus (" ]");
        Instr_Stack.Push (Stack, Item_Chrs);
        return Item_Prog;
      end if;

    end if;

    -- Parse arbitrary or fraction: @num or @num:denom
    if Txt.Length >= 2 and then Txt.Element(1) = '@' then
      declare
        I : constant Natural := String_Mng.Locate (Txt.Image, ":");
        N, D : Arbitrary.Number;
      begin
        Instr_Stack.Push (Stack, Item_Chrs);
        if I = 0 then
          N := Arbitrary.Set (Txt.Slice(2, Txt.Length));
          return (Kind => Mcd_Mng.Arbi, Val_Arbi => N);
        else
          N := Arbitrary.Set (Txt.Slice(2, I - 1));
          D := Arbitrary.Set (Txt.Slice(I + 1, Txt.Length));
          return (Kind => Mcd_Mng.Frac,
                  Val_Frac => Arbitrary.Fractions.Set (N, D));
        end if;
      exception
        when others =>
          raise Parsing_Error;
      end;
    end if;

    -- Parse Oper : string
    declare
      Op : Mcd_Mng.Operator_List;
    begin
      Op := Mcd_Mng.Operator_List'Value (Txt.Image);
      -- Allow string only if no symbol defined
      if Words(Op).Word = Nosy then
        Instr_Stack.Push (Stack, Item_Chrs);
        return (Kind => Mcd_Mng.Oper, Val_Oper => Op);
      end if;
    exception
      -- Does not match
      when others => null;
    end;

    -- Parse Oper : symbol
    if Txt.Length <= 2 then
      if Txt.Length = 2 then
        W := Txt.Image;
      else
        W(1) := Txt.Element (1);
        W(2) := ' ';
      end if;
      for O in Mcd_Mng.Operator_List loop
        if Words(O).Word = W then
          Instr_Stack.Push (Stack, Item_Chrs);
          return (Kind => Mcd_Mng.Oper, Val_Oper => O);
        end if;
      end loop;
    end if;

    -- Parse Inte Real Bool
    begin
      Inte_Io.Get (Txt.Image, I, L);
      if L = Txt.Length then
        Instr_Stack.Push (Stack, Item_Chrs);
        return (Kind => Mcd_Mng.Inte, Val_Inte => I);
      end if;
    exception
      when others => null;
    end;

    begin
      My_Math.Real_Io.Get (Txt.Image, R, L);
      if L = Txt.Length and then Txt.Element (L) /= '.' then
        Instr_Stack.Push (Stack, Item_Chrs);
        return (Kind => Mcd_Mng.Real, Val_Real => R);
      end if;
    exception
      when others => null;
    end;

    begin
      Bool_Io.Get (Txt.Image, B, L);
      if L = Txt.Length then
        Instr_Stack.Push (Stack, Item_Chrs);
        return (Kind => Mcd_Mng.Bool, Val_Bool => B);
      end if;
    exception
      when others => null;
    end;

    -- Cannot recognise anything
    Instr_Stack.Push (Stack, Item_Chrs);
    raise Parsing_Error;

  exception
    when others =>
      raise Parsing_Error;
  end Next_Item;


  procedure Print_Help is
    Tab1 : constant String (1 .. 5) := (others => ' ');
    Tab2 : constant String (1 .. 3) := (others => ' ');
    Ope_Name : String (1 .. Ope_Len);
    Pad : constant String (1 .. Ope_Len) := (others => ' ');
    Separator : constant String (1 .. Ope_Len) := (others => '-');
  begin
    Io_Flow.Put_Line ("Usage: " & Argument.Get_Program_Name & " [ -a<bus_address> | -u<udp_spec> | -t<tcp_port> | -h ]");
    Io_Flow.Put_Line ("  <udp_spec> ::= [<ipm_lan_name_or_num>]:<udp_port>");
    Io_Flow.Put_Line ("  <port>     ::= <port_name> | <port_num>");
    Io_Flow.New_Line;
    Io_Flow.Put_Line ("Commands are strings read from standard input or from a fifo.");
    Io_Flow.Put_Line ("Separators are space and horizontal tab.");
    Io_Flow.Put_Line ("Comments start by '#', up to the end of line");
    Io_Flow.Put_Line ("Item ::= <arbitrary> <fraction> <integer> <real> <boolean> <operator> <register> <subprogram> <string>");
    Io_Flow.Put_Line ("  <arbitrary>  ::= @<number>");
    Io_Flow.Put_Line ("  <fraction>   ::= @<number>:<number>");
    Io_Flow.Put_Line ("  <integer>    ::= <number> | <base>#<number>#");
    Io_Flow.Put_Line ("  <register>   ::= 'a' .. 'z'  | 'A' .. 'Z'");
    Io_Flow.Put_Line ("  <subprogram> ::= '[' { <item> } ']'");
    Io_Flow.Put_Line ("  <string>     ::= ""<text>""");
    Io_Flow.Put_Line ("Operators are: ");
    Io_Flow.Put_Line (Tab1
                    & "Name       Action (A is top of stack, then B...)");
    for O in Mcd_Mng.Operator_List loop
      Ope_Name:= (others => ' ');
      if Words(O).Word /= Nosy then
        Ope_Name(1 .. 2) := Words(O).Word;
      else
        Ope_Name(1 .. Mcd_Mng.Operator_List'Image(O)'Length)
                := Lower_Str(Mcd_Mng.Operator_List'Image(O));
      end if;
      Io_Flow.Put_Line (Tab1 & Ope_Name & Tab2 & Words(O).Comment);
      if not Words(O).Extra_Comment.Is_Null then
        Io_Flow.Put_Line (Tab1 & Pad & Tab2
                        & "  " & Words(O).Extra_Comment.Image);
      end if;
      if Words(O).New_Line then
        Io_Flow.Put_Line (Tab1 & Separator);
      end if;
    end loop;
  end Print_Help;

  procedure Dump_Stack is
    Item_Chrs : Item_Chrs_Rec;
  begin
    if not Debug.Debug_Level_Array(Debug.History) then
      return;
    end if;
    Basic_Proc.Put_Line_Error ("History:");
    loop
      begin
        Instr_Stack.Pop (Stack, Item_Chrs);
        Basic_Proc.Put_Error (Item_Chrs.Val_Text.Image & " ");
      exception
        when Instr_Stack.Circ_Empty =>
         exit;
      end;
    end loop;
    Basic_Proc.New_Line_Error;
  end Dump_Stack;

end Mcd_Parser;

