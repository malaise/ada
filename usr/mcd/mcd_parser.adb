with Text_Io;
with Text_Handler, My_Math, Queues, Sys_Calls, Lower_Str;
with Debug, Input_Dispatcher, Bool_Io, Inte_Io, Real_Io;
package body Parser is
  use Mcd_Mng;

  subtype Item_Chrs_Rec is Mcd_Mng.Item_Rec(Mcd_Mng.Chrs);
  subtype Item_Prog_Rec is Mcd_Mng.Item_Rec(Mcd_Mng.Prog);

  -- Instruction stack for debug history
  package Instr_Stack is new Queues.Circ(7, Item_Chrs_Rec);

  Txt, Txts : Text_Handler.Text(Input_Dispatcher.Max_String_Lg);

  Input_Error : Boolean := False;

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
   Minus    => ("+-", "push -A                       ", False),
   Absv     => (Nosy, "push |A|                      ", True),

   Bitand   => ("&&", "push B & A (bit and)          ", False),
   Bitor    => ("||", "push B | A (bit and)          ", False),
   Bitxor   => ("^^", "push B ^ A (bit and)          ", False),
   Bitneg   => ("~~", "push ~A    (bit neg)          ", False),
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

   Pi       => (Nosy, "push PI                       ", False),
   Sin      => (Nosy, "push Sin(A) A in radiants     ", False),
   Cos      => (Nosy, "push Cos(A) A in radiants     ", False),
   Tan      => (Nosy, "push Tan(A) A in radiants     ", False),
   ASin     => (Nosy, "push ASin(A) in radiants      ", False),
   ACos     => (Nosy, "push ACos(A) in radiants      ", False),
   ATan     => (Nosy, "push ATan(A) in radiants      ", True),

   Exp      => (Nosy, "push E                        ", False),
   Ln       => (Nosy, "push ln(A)                    ", False),
   Log      => (Nosy, "push log(A)                   ", True),

   Toreal   => (Nosy, "push REAL(A)                  ", False),
   Round    => (Nosy, "push INTE(A) (round)          ", False),
   Trunc    => (Nosy, "push INTE(A) (int part)       ", False),
   Int      => (Nosy, "push int part of A            ", False),
   Frac     => (Nosy, "push frac part of A           ", True),
 
   Isreal   => (Nosy, "push True if A is a real      ", False),
   Isinte   => (Nosy, "push True if A in an integer  ", False),
   Isstr    => (Nosy, "push True if A is a string    ", False),
   Isreg    => (Nosy, "push True if A is a register  ", True),
 
   Ssize    => (Nosy, "push stack size               ", False),
   Swap     => (Nosy, "push A, push B                ", False),
   Swap3    => (Nosy, "push A, push B, push C        ", False),
   Dup      => (Nosy, "push A, push A                ", False),
   Pop      => (Nosy, "pop A                         ", False),
   Popn     => (Nosy, "pop B A times                 ", True),

   Popr     => (Nosy, "B -> regA                     ", False),
   Copyr    => (Nosy, "B -> regA, push B             ", False),
   Pushr    => (Nosy, "push regA                     ", False),
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
   Strloc   => (Nosy, "push C occurence of B in A    ", False),
   Strrep   => (Nosy, "push A replaced by B at pos C ", False),
   Strupp   => (Nosy, "push A in uppercase           ", False),
   Strlow   => (Nosy, "push A in lowercase           ", False),
   Strreal  => (Nosy, "push A converted to real      ", False),
   Strinte  => (Nosy, "push A converted to integer   ", False),
   Strbool  => (Nosy, "push A converted to boolean   ", False),
   Strregi  => (Nosy, "push A converted to register  ", False),
   Strof    => (Nosy, "push formated string of A     ", True),

   Obase    => (Nosy, "set output base to A          ", False),
   Nop      => (Nosy, "no operation                  ", False),
   Rnd      => (Nosy, "push 0.0 <= RND < 1.0         ", False),
   Sleep    => (Nosy, "sleep A seconds               ", False),
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
  begin

    Text_Handler.Set (Txt, Input_Dispatcher.Next_Word);
    if Debug.Debug_Level_Array(Debug.Parser) then
      Text_Io.Put_Line ("Parser: Getting >"
               & Text_Handler.Value(Txt)  & "<");
    end if;
    Item_Chrs.Val_Len := Text_Handler.Length(Txt);
    Item_Chrs.Val_Text(1 .. Item_Chrs.Val_Len) := Text_Handler.Value(Txt);

    -- EOF
    if Text_Handler.Empty(Txt) then
      if Debug.Debug_Level_Array(Debug.Parser) then
        Text_Io.Put_Line ("Parser: Eof");
      end if;
      Item_Chrs.Val_Len := 3;
      Item_Chrs.Val_Text(1 .. 3) := "EOF";
      Instr_Stack.Push(Item_Chrs);
      return (Kind => Oper, Val_Oper => Ret);
    end if;

    C := Text_Handler.Value(Txt)(1);

    -- No [ nor ] in word
    if Text_Handler.Value(Txt) /= "["
    and then Text_Handler.Value(Txt) /= "]"
    and then (Text_Handler.Locate(Txt, "[") /= 0
      or else Text_Handler.Locate(Txt, "]") /= 0) then
      Instr_Stack.Push(Item_Chrs);
      raise Parsing_Error;
    end if;

    -- Parse [ or REGI
    if Text_Handler.Length(Txt) = 1 then
      
      if Mcd_Mng.Is_Register(C) then
        -- A register
        Instr_Stack.Push(Item_Chrs);
        return (Kind => Mcd_Mng.Regi, Val_Regi => C);
      elsif C = '[' then
        -- Get rid of strings
        Text_Handler.Empty(Txts);
        First_Word := True;
        Level := 1;
        while Level /= 0 loop
          Text_Handler.Set(Txt, Input_Dispatcher.Next_Word);
          if Debug.Debug_Level_Array(Debug.Parser) then
            Text_Io.Put_Line ("Parser: Getting >"
                     & Text_Handler.Value(Txt)  & "<");
          end if;
          if Text_Handler.Value(Txt) = "[" then
            Level := Level + 1;
          elsif Text_Handler.Value(Txt) = "]" then
            Level := Level - 1;
            exit when Level = 0;
          elsif Text_Handler.Empty(Txt) then
            -- Unexpected Eof
            Item_Prog.Val_Len := Text_Handler.Length(Txts);
            Item_Prog.Val_Text(1 .. Item_Prog.Val_Len) := Text_Handler.Value(Txts);
            if Item_Chrs.Val_Len + 2 <= Input_Dispatcher.Max_String_Lg then
              Item_Chrs.Val_Len := Text_Handler.Length(Txts) + 2;
              Item_Chrs.Val_Text(1 .. Item_Chrs.Val_Len) := "[ " & Text_Handler.Value(Txts);
            else
              Item_Chrs.Val_Len := Text_Handler.Length(Txts);
              Item_Chrs.Val_Text(1 .. Item_Chrs.Val_Len) := Text_Handler.Value(Txts);
            end if;
            Instr_Stack.Push(Item_Chrs);
            raise Parsing_Error;
          end if;
          -- No space before first word
          if First_Word then
            First_Word  := False;
          else
            Text_Handler.Append (Txts, ' ');
          end if;
          Text_Handler.Append (Txts, Txt);
        end loop;
        Item_Prog.Val_Len := Text_Handler.Length(Txts);
        Item_Prog.Val_Text(1 .. Item_Prog.Val_Len) := Text_Handler.Value(Txts);
        if Item_Chrs.Val_Len + 4 <= Input_Dispatcher.Max_String_Lg then
          Item_Chrs.Val_Len := Text_Handler.Length(Txts) + 4;
          Item_Chrs.Val_Text(1 .. Item_Chrs.Val_Len) := "[ " & Text_Handler.Value(Txts) & " ]";
        else
          Item_Chrs.Val_Len := Text_Handler.Length(Txts);
          Item_Chrs.Val_Text(1 .. Item_Chrs.Val_Len) := Text_Handler.Value(Txts);
        end if;
        Instr_Stack.Push(Item_Chrs);
        return Item_Prog;
      end if;

    end if;

    -- Strings
    if Text_Handler.Value(Txt)(1) = Input_Dispatcher.Sd
    and then Text_Handler.Value(Txt)(Text_Handler.Length(Txt)) = Input_Dispatcher.Sd then
      Item_Chrs.Val_Len := Text_Handler.Length(Txt);
      Item_Chrs.Val_Text(1 .. Item_Chrs.Val_Len) := Text_Handler.Value(Txt);
      Instr_Stack.Push(Item_Chrs);
      Item_Chrs.Val_Len := Item_Chrs.Val_Len - 2;
      Item_Chrs.Val_Text(1 .. Item_Chrs.Val_Len) :=
             Item_Chrs.Val_Text(2 .. Item_Chrs.Val_Len + 1);
      return Item_Chrs;
    end if;


    -- Parse OPER : string
    declare
      Op : Mcd_Mng.Operator_List;
    begin
      Op := Mcd_Mng.Operator_List'Value(Text_Handler.Value(Txt));
      -- Allow string only if no symbol defined
      if Words(Op).Word = Nosy then
        Instr_Stack.Push(Item_Chrs);
        return (Kind => Mcd_Mng.Oper, Val_Oper => Op);
      end if;
    exception
      -- Does not match
      when others => null;
    end;


    -- Parse OPER : synbol
    if Text_Handler.Length(Txt) <= 2 then
      if Text_Handler.Length(Txt) = 2 then
        W := Text_Handler.Value(Txt);
      else
        W(1) := Text_Handler.Value(Txt)(1);
        W(2) := ' ';
      end if;
      for O in Mcd_Mng.Operator_List loop
        if Words(O).Word = W then
          Instr_Stack.Push(Item_Chrs);
          return (Kind => Mcd_Mng.Oper, Val_Oper => O);
        end if;
      end loop;
    end if;


    -- Parse INTE REAL BOOL
    begin
      Bool_Io.Get(Text_Handler.Value(Txt), B, L);
      if L = Text_Handler.Length(Txt) then
        Instr_Stack.Push(Item_Chrs);
        return (Kind => Mcd_Mng.Bool, Val_Bool => B);
      end if;
    exception
      when others => null;
    end;
    begin
      Inte_Io.Get(Text_Handler.Value(Txt), I, L);
      if L = Text_Handler.Length(Txt) then
        Instr_Stack.Push(Item_Chrs);
        return (Kind => Mcd_Mng.Inte, Val_Inte => I);
      end if;
    exception
      when others => null;
    end;
    begin
      Real_Io.Get(Text_Handler.Value(Txt), R, L);
      if L = Text_Handler.Length(Txt) then
        Instr_Stack.Push(Item_Chrs);
        return (Kind => Mcd_Mng.Real, Val_Real => R);
      end if;
    exception
      when others => null;
    end;
    
    -- Cannot recognise anything
    Instr_Stack.Push(Item_Chrs);
    raise Parsing_Error;  
    
  exception
    when Input_Dispatcher.String_Error =>
      Input_Error := True;
      raise Parsing_Error;
    when others =>
      raise Parsing_Error;  
  end Next_Item;


  procedure Print_Help is
    use Text_Io;
    Ope_Name : String (1 .. Ope_Len);
  begin
    Put_Line ("Commands are read from standard input. No argument accepted.");
    Put_Line ("Separators are space and horizontal tab.");
    Put_Line ("Item ::= <integer> <real> <boolean> <operator> <register> <subprogram> <string>");
    Put_Line ("  <integer>    ::= <number> | <base>#<number># ");
    Put_Line ("  <register>   ::= 'a' .. 'z'  | 'A' .. 'Z'");
    Put_Line ("  <subprogram> ::= '[' { <item> } ']'");
    Put_Line ("  <string>     ::= ""<text>""");
    Put_Line ("Operators are: Name      Action (A is top of stack, then B...)");
    for O in Mcd_Mng.Operator_List loop
      Ope_Name:= (others => ' ');
      if Words(O).Word /= Nosy then
        Ope_Name(1 .. 2) := Words(O).Word;
      else 
        Ope_Name(1 .. Mcd_Mng.Operator_List'Image(O)'Length)
                := Lower_Str(Mcd_Mng.Operator_List'Image(O));
      end if;
      Put_Line("               " & Ope_Name & "   " & Words(O).Comment);
      if Words(O).New_Line then
        New_Line;
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
        Sys_Calls.Put_Error (Item_Chrs.Val_Text(1 .. Item_Chrs.Val_Len) & " ");
      exception
        when Instr_Stack.Circ_Empty =>
         exit;
      end;
    end loop;
    if Input_Error then
      Sys_Calls.Put_Line_Error (Input_Dispatcher.Error_String);
    else
      Sys_Calls.New_Line_Error;
    end if;
  end Dump_Stack;

end Parser;

