with Text_Io;
with Text_Handler, My_Math, Queues, Sys_Calls, Lower_Str;
with Debug, Input_Dispatcher, Bool_Io, Inte_Io, Real_Io;
package body Parser is
  use Mcd_Mng;

  subtype Item_Chrs_Rec is Mcd_Mng.Item_Rec(Mcd_Mng.Chrs);

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
  end record;

  -- The operators
  Words : constant array (Mcd_Mng.Operator_List) of One_Rec :=
  (Nop      => (Nosy, "No operation                  "),

   Add      => ("+ ", "push B + A                    "),
   Sub      => ("- ", "push B - A                    "),
   Mult     => ("* ", "push B * A                    "),
   Div      => ("/ ", "push B / A                    "),
   Remind   => ("% ", "push B % A (rest of division) "),
   Pow      => ("**", "push B ** A (pow)             "),
   Minus    => ("+-", "push -A                       "),
   Absv     => (Nosy, "push |A|                      "),

   Bitand   => ("&&", "push B & A (bit and)          "),
   Bitor    => ("||", "push B | A (bit and)          "),
   Bitxor   => ("^^", "push B ^ A (bit and)          "),
   Bitneg   => ("~~", "push ~A    (bit neg)          "),
   Shl      => ("<<", "push B << A (bits shift left) "),
   Shr      => (">>", "push B >> A (bits shift right)"),

   Equal    => ("= ", "push B = A                    "),
   Diff     => ("/=", "push B /= A                   "),
   Greater  => ("> ", "push B > A                    "),
   Smaller  => ("< ", "push B < A                    "),
   Greateq  => (">=", "push B >= A                   "),
   Smalleq  => ("<=", "push B <= A                   "),

   Boland   => ("& ", "push B and A                  "),
   Bolor    => ("| ", "push B or A                   "),
   Bolxor   => ("^ ", "push B xor A                  "),
   Bolneg   => ("~ ", "push not A                    "),

   Pi       => (Nosy, "push PI                       "),
   Sin      => (Nosy, "push Sin(A) A in radiants     "),
   Cos      => (Nosy, "push Cos(A) A in radiants     "),
   Tan      => (Nosy, "push Tan(A) A in radiants     "),
   ASin     => (Nosy, "push ASin(A) in radiants      "),
   ACos     => (Nosy, "push ACos(A) in radiants      "),
   ATan     => (Nosy, "push ATan(A) in radiants      "),

   Toreal   => (Nosy, "push REAL(A)                  "),
   Round    => (Nosy, "push INTE(A) (round)          "),
   Trunc    => (Nosy, "push INTE(A) (int part)       "),
   Int      => (Nosy, "push int part of A            "),
   Frac     => (Nosy, "push frac part of A           "),
 
   Isreal   => (Nosy, "push True if A is a real      "),
   Isinte   => (Nosy, "push True if A in an integer  "),
   Isstr    => (Nosy, "push True if A is a string    "),
   Isreg    => (Nosy, "push True if A is a register  "),

   Obase    => (Nosy, "set output base to A          "),
 
   Ssize    => (Nosy, "push stack size               "),
   Swap     => (Nosy, "push A, push B                "),
   Dup      => (Nosy, "push A, push A                "),
   Pop      => (Nosy, "pop A                         "),
   Popn     => (Nosy, "pop B A times                 "),
   Rnd      => (Nosy, "push 0.0 <= RND < 1.0         "),
   Sleep    => (Nosy, "sleep A seconds               "),

   Popr     => (Nosy, "B -> regA                     "),
   Copyr    => (Nosy, "B -> regA, push B             "),
   Pushr    => (Nosy, "push regA                     "),

   Pope     => (Nosy, "pop A push_extra A            "),
   Copye    => (Nosy, "pop A push_extra A push A     "),
   Pushle   => (Nosy, "pop_extra last  X push X      "),
   Pushfe   => (Nosy, "pop_extra first X push X      "),
   Esize    => (Nosy, "push extra_stack size         "),

   Ifthen   => (Nosy, "if B then push A              "),
   Ifte     => (Nosy, "if C then push B else push A  "),
   Etfi     => (Nosy, "if A then push C else push B  "),

   Call     => (Nosy, "call A                        "),
   Ifcall   => (Nosy, "if B then call A              "),
   Ret      => (Nosy, "return                        "),
   Retn     => (Nosy, "return A levels (0=none)      "),
   Retall   => (Nosy, "return all levels             "),
   Ifret    => (Nosy, "if A return                   "),
   Ifretn   => (Nosy, "if B return A levels (0=none) "),
   Ifretall => (Nosy, "if A return all levels        "),
   Retacal  => (Nosy, "return and call A             "),

   Format   => (Nosy, "xx or xx.yyy format           "),
   Put      => (Nosy, "put A                         "),
   Newl     => (Nosy, "new line                      "),
   Putl     => (Nosy, "put_line A                    "),

   Strlen   => (Nosy, "push length of A              "),
   Strcat   => (Nosy, "push B & A                    "),
   Strsub   => (Nosy, "push C(B..A)                  "),
   Strloc   => (Nosy, "push C occurence of B in A    "),
   Strrep   => (Nosy, "push A replaced by B at pos C "),
   Strupp   => (Nosy, "push A in uppercase           "),
   Strlow   => (Nosy, "push A in lowercase           "),
   Strreal  => (Nosy, "push A converted to real      "),
   Strinte  => (Nosy, "push A converted to inte      "),
   Strbool  => (Nosy, "push A converted to bool      "),
   Strof    => (Nosy, "push formated string of A     "),

   Help     => (Nosy, "put help                      ") );


  function Next_Item return Mcd_Mng.Item_Rec is
    Level : Natural;
    Item_Chrs, Saved_Item_Chrs : Item_Chrs_Rec;
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

    -- Parse [ or REGI
    if Text_Handler.Length(Txt) = 1 then
      
      if C in 'a' .. 'z' or else C in 'A' .. 'Z' then
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
          if Text_Handler.Value(Txt) = "[" then
            Level := Level + 1;
          elsif Text_Handler.Value(Txt) = "]" then
            Level := Level - 1;
            exit when Level = 0;
          end if;
          -- No space before first word
          if First_Word then
            First_Word  := False;
          else
            Text_Handler.Append (Txts, ' ');
          end if;
          Text_Handler.Append (Txts, Txt);
        end loop;
        Item_Chrs.Val_Len := Text_Handler.Length(Txts);
        Item_Chrs.Val_Text(1 .. Item_Chrs.Val_Len) := Text_Handler.Value(Txts);
        if Item_Chrs.Val_Len + 4 <= Input_Dispatcher.Max_String_Lg then
          Saved_Item_Chrs.Val_Len := Text_Handler.Length(Txts) + 4;
          Saved_Item_Chrs.Val_Text(1 .. Saved_Item_Chrs.Val_Len) := "[ " & Text_Handler.Value(Txts) & " ]";
        else
          Saved_Item_Chrs := Item_Chrs;
        end if;
        Instr_Stack.Push(Saved_Item_Chrs);
        return Item_Chrs;
      end if;

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
    Put_Line ("Separators are space and horiz tab.");
    Put_Line ("Item ::= <integer> <real> <boolean> <operator> <register> <string/subprogram>");
    Put_Line ("  <integer>           ::= <number> | <base>#<number># ");
    Put_Line ("  <register>          ::= 'a' .. 'z'  | 'A' .. 'Z'");
    Put_Line ("  <string/subprogram> ::= '[' <text> ']'");
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

