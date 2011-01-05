with Random, Async_Stdin;
with Debug, Input_Dispatcher, Mcd_Parser;
pragma Elaborate(Random);
package body Mcd_Mng is

  -- Values poped and processed by oper
  A, B, C, D : Item_Rec;

  -- Saved value (previous top of stack), invalid when of kind Oper
  Invalid_Item : constant Item_Rec
       := (Kind => Oper, Val_Oper => Operator_List'First);
  S : Item_Rec := Invalid_Item;

  -- Subprogram called
  Call_Entry : As.U.Asu_Us;

  package Stack is
    -- What can we store in stack
    subtype Operand_Kind_List is Item_Kind_List range Arbi .. Regi;
    -- On push : Invalid_Item;

    procedure Push (Item : in Item_Rec; Default_Stack : in Boolean := True);

    procedure Pop (Item : out Item_Rec; Default_Stack : in Boolean := True);
    procedure Read (Item : out Item_Rec; Default_Stack : in Boolean := True);

    function Stack_Size (Default_Stack : Boolean := True) return Natural;

    -- Dump last N elements popped or read, if debug history
    procedure Dump_History;
    -- Clear history
    procedure Clear_History;

    -- Pop first pushed in extra stack
    procedure Popfe (Item : out Item_Rec);

    -- Push before first pushed in extra stack
    procedure Pushfe (Item : in Item_Rec);
  end Stack;

  package Operations is

    function Is_True (X : Item_Rec) return Boolean;

    -- Arbi,Arbi->Arbi or Frac,Frac->Frac or Inte,Inte->Inte or
    -- Real,Real->Real
    function Add     (L, R : Item_Rec) return Item_Rec;
    function Sub     (L, R : Item_Rec) return Item_Rec;
    function Mult    (L, R : Item_Rec) return Item_Rec;
    function Div     (L, R : Item_Rec) return Item_Rec;

    -- Arbi,Arbi->Arbi or Frac,Arbi->Frac or Inte,Inte->Inte or
    -- Real,Real->Real
    function Pow     (L, R : Item_Rec) return Item_Rec;

    -- Arbi,Arbi->Arbi or Inte,Inte->Inte
    function Remind  (L, R : Item_Rec) return Item_Rec;

    -- Inte,Inte->Inte
    function Bitand  (L, R : Item_Rec) return Item_Rec;
    function Bitor   (L, R : Item_Rec) return Item_Rec;
    function Bitxor  (L, R : Item_Rec) return Item_Rec;
    function Shl     (L, R : Item_Rec) return Item_Rec;
    function Shr     (L, R : Item_Rec) return Item_Rec;

    -- Arbi->Arbi or Frac->Frac or Inte->Inte or Real->Real
    function Minus   (X : Item_Rec) return Item_Rec;
    function Absv    (X : Item_Rec) return Item_Rec;

    -- Inte->Inte
    function Bitneg  (X : Item_Rec) return Item_Rec;

    -- Arbi,Arbi->Bool or Frac,Frac->Bool or Inte,Inte->Bool
    -- or Real,Real->Bool or Bool,Bool->Bool or Chars,Chars->Bool
    -- or Regi->Regi->Bool
    function Equal   (L, R : Item_Rec) return Item_Rec;
    function Diff    (L, R : Item_Rec) return Item_Rec;
    function Greater (L, R : Item_Rec) return Item_Rec;
    function Smaller (L, R : Item_Rec) return Item_Rec;
    function Greateq (L, R : Item_Rec) return Item_Rec;
    function Smalleq (L, R : Item_Rec) return Item_Rec;

    -- Arbi,Inte->Inte
    function Tointe  (X : Item_Rec) return Item_Rec;

    -- Arbi,Inte->Arbi
    function Toarbi  (X : Item_Rec) return Item_Rec;

    -- Inte,Real->Real
    function Toreal  (X : Item_Rec) return Item_Rec;

    -- Inte,Real->Inte
    function Round   (X : Item_Rec) return Item_Rec;
    function Trunc   (X : Item_Rec) return Item_Rec;

    -- Real->Real
    function Int     (X : Item_Rec) return Item_Rec;
    function Frac    (X : Item_Rec) return Item_Rec;
    function Dms     (X : Item_Rec) return Item_Rec;
    function Msd     (X : Item_Rec) return Item_Rec;
    function Sqrt    (X : Item_Rec) return Item_Rec;

    -- -> Inte
    function Maxint return Item_Rec;
    function Minint return Item_Rec;

    -- Real->Inte or Real
    function Roundif (X : Item_Rec) return Item_Rec;

    -- *->Bool
    function Isarbi  (X : Item_Rec) return Item_Rec;
    function Isfrac  (X : Item_Rec) return Item_Rec;
    function Isinte  (X : Item_Rec) return Item_Rec;
    function Isreal  (X : Item_Rec) return Item_Rec;
    function Isbool  (X : Item_Rec) return Item_Rec;
    function Isstr   (X : Item_Rec) return Item_Rec;
    function Isreg   (X : Item_Rec) return Item_Rec;
    function Isprog  (X : Item_Rec) return Item_Rec;

    -- Frac <-> Arbi
    function Mkfrac (N, D : Item_Rec) return Item_Rec;
    function Numerof (X : Item_Rec) return Item_Rec;
    function Denomof (X : Item_Rec) return Item_Rec;

    -- Arbi->Bool or Inte->Bool or Real->Bool
    function Ispos    (X : Item_Rec) return Item_Rec;
    function Isnul    (X : Item_Rec) return Item_Rec;
    function Isnotnul (X : Item_Rec) return Item_Rec;
    function Isneg    (X : Item_Rec) return Item_Rec;

    -- Bool,Bool->Bool
    function Boland  (L, R : Item_Rec) return Item_Rec;
    function Bolor   (L, R : Item_Rec) return Item_Rec;
    function Bolxor  (L, R : Item_Rec) return Item_Rec;

    -- Bool->Bool
    function Bolneg  (X : Item_Rec) return Item_Rec;

    -- Bool,*,*->*
    function Ifte    (X, A, B : Item_Rec) return Item_Rec;

    -- Real->Real
    function Sin     (X : Item_Rec) return Item_Rec;
    function Cos     (X : Item_Rec) return Item_Rec;
    function Tan     (X : Item_Rec) return Item_Rec;
    function Asin    (X : Item_Rec) return Item_Rec;
    function Acos    (X : Item_Rec) return Item_Rec;
    function Atan    (X : Item_Rec) return Item_Rec;
    function Ln      (X : Item_Rec) return Item_Rec;
    function Log     (X : Item_Rec) return Item_Rec;

    -- Arbi,Arbi,Arbi->Arbi or Inte,Inte,Inte->Inte or Real,Real,Real->Real
    function Proport (X, Y, Z : Item_Rec) return Item_Rec;

    -- Arbi->Arbi or Inte->Real
    function Fact    (X : Item_Rec) return Item_Rec;

    -- Real,Inte->Real
    function Roundat (X : Item_Rec; N : Item_Rec) return Item_Rec;

    -- Argument does not mach operator
    -- Invalid_Argument : exception;
    -- Arguments are not compatible to each other
    -- Argument_Mismatch : exception;
  end Operations;

  package Registers is
    subtype Register_Content_List is Item_Kind_List range Arbi .. Regi;

    -- Is a character a register (in a..z or A..Z)
    function Is_Register (C : in Character) return Boolean;

    -- Store to / retrieve from / Clear
    procedure Store (Val : in Item_Rec; To_Reg : in Item_Rec);
    function  Retrieve (From_Reg : Item_Rec) return Item_Rec;
    procedure Clear (Reg : in Item_Rec);

    -- Clear all registers
    procedure Clear_All;

    -- Empty?
    function Is_Empty (Reg : Item_Rec) return Item_Rec;

    -- Next / prev register
    procedure Next (Reg : in out Item_Rec);
    procedure Prev (Reg : in out Item_Rec);

    -- Register <-> Index
    function Index_Of (Reg : Item_Rec) return Item_Rec;
    function Register_At (Index : Item_Rec) return Item_Rec;

    -- Array: store / retrieve Var[Index]
    -- Var must be a register; Index must be Inte (otherwise Invalid_Argument)
    procedure Store_Array (Val : in Item_Rec;
                           To_Reg : in Item_Rec; Index : in Item_Rec);
    function Retrieve_Array (To_Reg : Item_Rec; Index : Item_Rec)
                            return Item_Rec;

    -- Valid registers  are 'a' .. 'z' and 'A' .. 'Z'
    -- Invalid_Register : exception;

    -- Valid contents are Arbi Inte Real Bool Prog Chrs Regi
    -- Invalid_Argument : exception;

    -- Nothing to retrieve
    -- Empty_Register : exception;
  end Registers;

  package Ios is

    procedure Set_Obase (Base : in Item_Rec);

    procedure Format (Item : in Item_Rec);
    procedure Put (Item : in Item_Rec);
    procedure Put_Line (Item : in Item_Rec);
    procedure New_Line;

    function Strarbi (S : Item_Rec) return Item_Rec;
    function Strfrac (S : Item_Rec) return Item_Rec;
    function Strinte (S : Item_Rec) return Item_Rec;
    function Strreal (S : Item_Rec) return Item_Rec;
    function Strbool (S : Item_Rec) return Item_Rec;
    function Strregi (S : Item_Rec) return Item_Rec;
    function Strprog (S : Item_Rec) return Item_Rec;
    function Strof (Item : Item_Rec) return Item_Rec;
    function Normalof (Item      : Item_Rec;
                       Len       : Item_Rec;
                       Right_Len : Item_Rec;
                       Gap       : Item_Rec) return Item_Rec;
    -- Invalid_Argument, Argument_Mismatch : exception;
  end Ios;

  package Call_Stack is

    procedure Push (Item : in As.U.Asu_Us);
    function  Pop return As.U.Asu_Us;

    function Level return Natural;

  end Call_Stack;

  package Strings is
    function Strlen (S : Item_Rec) return Item_Rec;
    function Strcat (S1, S2 : Item_Rec) return Item_Rec;
    function Strsub (S, I1, I2 : Item_Rec) return Item_Rec;
    function Strloc (S, Occ, Pat : Item_Rec) return Item_Rec;
    function Strrep (S, I, Pat : Item_Rec) return Item_Rec;
    function Strupp (S : Item_Rec) return Item_Rec;
    function Strlow (S : Item_Rec) return Item_Rec;
    function Strmix (S : Item_Rec) return Item_Rec;
  end Strings;

  package Dates is
    function Clock return Item_Rec;
    function Time_To_Date (Time : Item_Rec) return Item_Rec;
    function Time_To_Days (Time : Item_Rec) return Item_Rec;
    function Date_To_Time (Date : Item_Rec) return Item_Rec;
  end Dates;

  function Is_Register (C : in Character) return Boolean
                       renames Registers.Is_Register;

  package Misc is
    procedure Do_Call;

    procedure Do_Popn;

    procedure Do_Clear_Extra;

    procedure Do_Rotate_Extra (First : in Boolean; Times : in Item_Rec);

    subtype Delay_Status_List is End_Status_List range Continue .. Exit_Break;
    function Do_Delay (The_Delay : Duration) return Delay_Status_List;

    function Do_Delay (The_Delay : Item_Rec) return Delay_Status_List;

    procedure Set_Debug (Set : in Item_Rec);

    function Reg_Match (Pattern, Str : Item_Rec) return Item_Rec;

    function Getenv (Item : Item_Rec) return Item_Rec;

    procedure Set_Exit_Code (Code : Item_Rec);
  end Misc;

  package body Stack is separate;
  package body Operations is separate;
  package body Registers is separate;
  package body Ios is separate;
  package body Call_Stack is separate;
  package body Strings is separate;
  package body Dates is separate;
  package body Misc is separate;

  procedure Dump_Stack renames Stack.Dump_History;

  -- Check for Ctrl Break each Item_Check_Period items
  --  to dectect it even if no input (e.g. within a loop)
  Item_Check_Period : constant Positive := 100;
  Nb_Item : Natural := 0;

  procedure New_Item (Item : in Item_Rec; The_End : out End_Status_List) is

    procedure Do_Retn (All_Levels    : in Boolean;
                       Levels        : in Item_Rec;
                       Allow_Level_0 : in Boolean) is
      L : Integer;
      Call_Stack_Level : Natural;
    begin
      if Debug.Debug_Level_Array(Debug.Oper) then
        Async_Stdin.Put_Line_Err("Mng: Do_ret");
      end if;
      Call_Stack_Level := Call_Stack.Level;
      if not All_Levels then
        -- Has to be Inte and val NaturaL
        begin
          L := Natural(Levels.Val_Inte);
        exception
          when others => raise Invalid_Argument;
        end;
        if L = 0 then
          return;
        end if;
      else
        -- Return all
        L := Call_Stack_Level + 1;
      end if;
      -- Can return by one more than level
      if L - 1 > Call_Stack_Level then
        raise Invalid_Argument;
      elsif L - 1 = Call_Stack_Level then
        if Allow_Level_0 then
          The_End := Exit_Return;
          return;
        else
          -- Retacal from level 0
          raise Invalid_Argument;
        end if;
      end if;
      -- Return N times
      for I in reverse 1 .. L loop
        -- Restart form previous context
        Call_Entry := Call_Stack.Pop;
      end loop;
      Input_Dispatcher.Set_Input(Call_Entry.Image);
    end Do_Retn;

    procedure Do_Retall is
    begin
      Do_Retn(True, A, True);
    end Do_Retall;

    procedure Do_Ret (Allow_Level_0 : in Boolean := True) is
    begin
      Stack.Pop(A);
      Do_Retn(False, A, Allow_Level_0);
    end Do_Ret;

    use Stack;
  begin
    -- Check for Ctrl C
    if Nb_Item = Item_Check_Period then
      Nb_Item := 0;
      The_End := Misc.Do_Delay(0.0);
    else
      Nb_Item := Nb_Item + 1;
      -- Default, except Ret and delay
      The_End := Continue;
    end if;
    -- Dispatch
    Clear_History;
    if Item.Kind /= Oper then
      -- Push operand
      Push(Item);
    else -- Operator
      if Debug.Debug_Level_Array(Debug.Oper) then
        Async_Stdin.Put_Err("Mng: ");
        Debug.Put(Item);
        Async_Stdin.New_Line_Err;
      end if;
      case Item.Val_Oper is
        -- These I do it myself
        when Nop =>
          null;
        when Swap =>
          Pop(A); Pop(B); Push(A); Push(B);
        when Swap3 =>
          Pop(A); Pop(B); Pop(C); Push(A); Push(B); Push(C);
        when Dup =>
          Read(A); Push(A);
        when Pop =>
          Pop(A);
        when Rnd =>
          Push( (Kind => Real,
                 Val_Real => My_Math.Real(Random.Float_Random)) );
        when Sleep =>
          Pop(A);
          S := A;
          The_End := Misc.Do_Delay(A);
        when Prevtop =>
          if S.Kind = Oper then
            raise Invalid_Argument;
          end if;
          Push (S);

        when Popn =>
          Misc.Do_Popn;

        -- These are operations
        when Add =>
          Pop(A); Pop(B); Push (Operations.Add(B,A));
          S := A;
        when Sub =>
          Pop(A); Pop(B); Push (Operations.Sub(B,A));
          S := A;
        when Mult =>
          Pop(A); Pop(B); Push (Operations.Mult(B,A));
          S := A;
        when Div =>
          Pop(A); Pop(B); Push (Operations.Div(B,A));
          S := A;
        when Remind =>
          Pop(A); Pop(B); Push (Operations.Remind(B,A));
          S := A;
        when Pow =>
          Pop(A); Pop(B); Push (Operations.Pow(B,A));
          S := A;
        when Bitand =>
          Pop(A); Pop(B); Push (Operations.Bitand(B,A));
          S := A;
        when Bitor =>
          Pop(A); Pop(B); Push (Operations.Bitor(B,A));
          S := A;
        when Bitxor =>
          Pop(A); Pop(B); Push (Operations.Bitxor(B,A));
          S := A;
        when Shl =>
          Pop(A); Pop(B); Push (Operations.Shl(B,A));
          S := A;
        when Shr =>
          Pop(A); Pop(B); Push (Operations.Shr(B,A));
          S := A;
        when Minus =>
          Pop(A); Push (Operations.Minus(A));
          S := A;
        when Absv =>
          Pop(A); Push (Operations.Absv(A));
          S := A;
        when Fact =>
          Pop(A); Push (Operations.Fact(A));
          S := A;
        when Bitneg =>
          Pop(A); Push (Operations.Bitneg(A));
          S := A;
        when Equal =>
          Pop(A); Pop(B); Push (Operations.Equal(B,A));
          S := A;
        when Diff =>
          Pop(A); Pop(B); Push (Operations.Diff(B,A));
          S := A;
        when Greater =>
          Pop(A); Pop(B); Push (Operations.Greater(B,A));
          S := A;
        when Smaller =>
          Pop(A); Pop(B); Push (Operations.Smaller(B,A));
          S := A;
        when Greateq =>
          Pop(A); Pop(B); Push (Operations.Greateq(B,A));
          S := A;
        when Smalleq =>
          Pop(A); Pop(B); Push (Operations.Smalleq(B,A));
          S := A;
        when Tointe =>
          Pop(A); Push (Operations.Tointe(A));
          S := A;
        when Toarbi =>
          Pop(A); Push (Operations.Toarbi(A));
          S := A;
        when Toreal =>
          Pop(A); Push (Operations.Toreal(A));
          S := A;
        when Round =>
          Pop(A); Push (Operations.Round(A));
          S := A;
        when Trunc =>
          Pop(A); Push (Operations.Trunc(A));
          S := A;
        when Int =>
          Pop(A); Push (Operations.Int(A));
          S := A;
        when Frac =>
          Pop(A); Push (Operations.Frac(A));
          S := A;
        when Maxint =>
          A := Operations.Maxint;
          Push (A);
          S := A;
        when Minint =>
          A := Operations.Minint;
          Push (A);
          S := A;
        when Roundif =>
          Pop(A); Push (Operations.Roundif(A));
          S := A;
          S := A;
        when Mkfrac =>
          Pop(A); Pop(B); Push (Operations.Mkfrac(B, A));
          S := A;
        when Numerof =>
          Pop(A); Push (Operations.Numerof(A));
          S := A;
        when Denomof =>
          Pop(A); Push (Operations.Denomof(A));
          S := A;
        when Dms =>
          Pop(A); Push (Operations.Dms(A));
          S := A;
        when Msd =>
          Pop(A); Push (Operations.Msd(A));
          S := A;
        when Sqrt =>
          Pop(A); Push (Operations.Sqrt(A));
          S := A;
        when Isarbi =>
          Pop(A); Push (Operations.Isarbi(A));
          S := A;
        when Isfrac =>
          Pop(A); Push (Operations.Isfrac(A));
          S := A;
        when Isinte =>
          Pop(A); Push (Operations.Isinte(A));
          S := A;
        when Isreal =>
          Pop(A); Push (Operations.Isreal(A));
          S := A;
        when Isbool =>
          Pop(A); Push (Operations.Isbool(A));
          S := A;
        when Isstr =>
          Pop(A); Push (Operations.Isstr(A));
          S := A;
        when Isreg =>
          Pop(A); Push (Operations.Isreg(A));
          S := A;
        when Isprog =>
          Pop(A); Push (Operations.Isprog(A));
          S := A;
        when Ispos =>
          Pop(A); Push (Operations.Ispos(A));
          S := A;
        when Isnul =>
          Pop(A); Push (Operations.Isnul(A));
          S := A;
        when Isnotnul =>
          Pop(A); Push (Operations.Isnotnul(A));
          S := A;
        when Isneg =>
          Pop(A); Push (Operations.Isneg(A));
          S := A;
        when Boland =>
          Pop(A); Pop(B); Push (Operations.Boland(B,A));
          S := A;
        when Bolor =>
          Pop(A); Pop(B); Push (Operations.Bolor(B,A));
          S := A;
        when Bolxor =>
          Pop(A); Pop(B); Push (Operations.Bolxor(B,A));
          S := A;
        when Bolneg =>
          Pop(A); Push (Operations.Bolneg(A));
          S := A;
        when Proport =>
          Pop(A); Pop(B); Pop(C); Push (Operations.Proport(C,B,A));
          S := A;
        when Roundat =>
          Pop(A); Pop(B); Push (Operations.Roundat(B,A));
          S := B;

        -- Trigo
        when Pi =>
          Push( (Kind => Real,
                 Val_Real => My_Math.Real(My_Math.Pi)) );
        when Sin =>
          Pop(A); Push (Operations.Sin(A));
          S := A;
        when Cos =>
          Pop(A); Push (Operations.Cos(A));
          S := A;
        when Tan =>
          Pop(A); Push (Operations.Tan(A));
          S := A;
        when Asin =>
          Pop(A); Push (Operations.Asin(A));
          S := A;
        when Acos =>
          Pop(A); Push (Operations.Acos(A));
          S := A;
        when Atan =>
          Pop(A); Push (Operations.Atan(A));
          S := A;

        -- Exp, logs
        when Epsilon =>
          Push( (Kind => Real,
                 Val_Real => 1.0E-10) );
        when Exp =>
          Push( (Kind => Real,
                 Val_Real => My_Math.Real(My_Math.E)) );
        when Ln =>
          Pop(A); Push (Operations.Ln(A));
          S := A;
        when Log =>
          Pop(A); Push (Operations.Log(A));
          S := A;

        -- Conditions
        when Ifthen =>
          Pop(A); Pop(B);
          if Operations.Is_True(B) then
            Push(A);
          end if;
          S := A;
        when Ifte =>
          Pop(A); Pop(B); Pop(C); Push (Operations.Ifte(C,B,A));
          S := A;
        when Etfi =>
          Pop(A); Pop(B); Pop(C); Push (Operations.Ifte(A,C,B));
          S := A;

        when Obase =>
          Pop(A); Ios.Set_Obase(A);
          S := A;

        -- These are about registers
        when Popr =>
          -- Store B in reg A
          Pop(A); Pop(B); Registers.Store(B, A);
          S := B;
        when Copyr =>
          -- Store B in reg A and push B
          Pop(A); Read(B); Registers.Store(B, A);
          S := A;
        when Pushr =>
          -- Push content of reg A
          Pop(A); Push(Registers.Retrieve(A));
          S := A;
        when Swapr =>
          -- Exchange B and content of reg A
          Pop(A); Pop(B); Push(Registers.Retrieve(A)); Registers.Store(B, A);
          S := B;
        when Swap2R =>
          -- Exchange content of reg B and content of reg A
          Pop(A); Pop(B);
          C := Registers.Retrieve(A);
          Registers.Store(Registers.Retrieve(B), A);
          Registers.Store(C, B);
          S := A;
        when Clearr =>
          -- Clear reg A
          Pop(A); Registers.Clear(A);
          S := A;
        when Clearall =>
          -- Clear all registers
          Registers.Clear_All;
        when Emptyr =>
          -- True is reg A is empty
          Pop(A); Push(Registers.Is_Empty(A));
          S := A;
        when Nextr =>
          -- Reg A -> Reg B
          Pop(A); Registers.Next(A); Push(A);
          S := A;
        when Prevr =>
          -- Reg A -> Reg B
          Pop(A); Registers.Prev(A); Push(A);
          S := A;
        when Regind =>
          -- Index of register
          Pop(A); Push(Registers.Index_Of(A));
          S := A;
        when Indreg =>
          -- Register at index
          Pop(A); Push(Registers.Register_At(A));
          S := A;
        when Popa =>
          -- B[A] <- C
          Pop(A); Pop(B); Pop(C);
          Registers.Store_Array(C, B, A);
          S := C;
        when Pusha =>
          -- Push B[A]
          Pop(A); Pop(B);
          Push (Registers.Retrieve_Array(B, A));

        -- Stack size
        when Ssize =>
          Push( (Kind => Inte, Val_Inte => My_Math.Inte(Stack.Stack_Size)));

        -- Extra stack
        when Pope =>
          -- pushe A
          Pop(A); Push (A, Default_Stack => False);
          S := A;
        when Copye =>
          -- pushe A push A
          Read(A); Push (A, Default_Stack => False);
        when Pushle =>
          -- pushe X push X
          Pop(A, Default_Stack => False); Push (A);
        when Pushfe =>
          -- pushe X push X
          Popfe(A); Push (A);
        when Rotle =>
          -- rotate from last
          Pop(A);
          Misc.Do_Rotate_Extra (False, A);
          S := A;
        when Rotfe =>
          -- rotate from first
          Pop(A);
          Misc.Do_Rotate_Extra (True, A);
          S := A;
        when Esize =>
           Push( (Kind => Inte,
                  Val_Inte => My_Math.Inte(Stack.Stack_Size(
                                Default_Stack => False))));
        when Cleare =>
           Misc.Do_Clear_Extra;


        -- These ones are subprogram
        when Call =>
          Misc.Do_Call;
        when Ifcall =>
          Pop(A);
          Pop(B);
          if Operations.Is_True(B) then
            Push(A);
            Misc.Do_Call;
          end if;

        when Ret =>
          Push( (Kind => Inte, Val_Inte => 1) );
          Do_Ret;
        when Retn =>
          Read (S);
          Do_Ret;
        when Retall =>
          Do_Retall;
        when Ifret =>
          Pop(A);
          if Operations.Is_True(A) then
            Push( (Kind => Inte, Val_Inte => 1) );
            Do_Ret;
          end if;
          S := A;
        when Ifretn =>
          Pop(A);
          Pop(B);
          if Operations.Is_True(B) then
            Push(A);
            Do_Ret;
          end if;
          S := A;
        when Ifretall =>
          Pop(A);
          if Operations.Is_True(A) then
            Do_Retall;
          end if;
          S := A;

        when Retacal =>
          Push( (Kind => Inte, Val_Inte => 1) );
          -- Return but forbid level 0
          Do_Ret(False);
          Misc.Do_Call;

        -- Puts
        when Format =>
          Pop(A); Ios.Format(A);
          S := A;
        when Put =>
          Pop(A); Ios.Put(A);
          S := A;
        when Putl =>
          Pop(A); Ios.Put_Line(A);
          S := A;
        when Newl =>
          Ios.New_Line;

        -- Strings
        when Strlen =>
          Pop(A); Push (Strings.Strlen(A));
          S := A;
        when Strcat =>
          Pop(A); Pop(B); Push (Strings.Strcat(B, A));
          S := A;
        when Strsub =>
          Pop(A); Pop(B); Pop(C); Push (Strings.Strsub(C, B, A));
          S := A;
        when Strloc =>
          Pop(A); Pop(B); Pop(C); Push (Strings.Strloc(C, B, A));
          S := A;
        when Strrep =>
          Pop(A); Pop(B); Pop(C); Push (Strings.Strrep(C, B, A));
          S := A;
        when Strupp =>
          Pop(A); Push (Strings.Strupp(A));
          S := A;
        when Strlow =>
          Pop(A); Push (Strings.Strlow(A));
          S := A;
        when Strmix =>
          Pop(A); Push (Strings.Strmix(A));
          S := A;
        when Strarbi =>
          Pop(A); Push (Ios.Strarbi(A));
          S := A;
        when Strfrac =>
          Pop(A); Push (Ios.Strfrac(A));
          S := A;
        when Strinte =>
          Pop(A); Push (Ios.Strinte(A));
          S := A;
        when Strreal =>
          Pop(A); Push (Ios.Strreal(A));
          S := A;
        when Strbool =>
          Pop(A); Push (Ios.Strbool(A));
          S := A;
        when Strregi =>
          Pop(A); Push (Ios.Strregi(A));
          S := A;
        when Strprog =>
          Pop(A); Push (Ios.Strprog(A));
          S := A;
        when Strof =>
          Pop(A); Push (Ios.Strof(A));
          S := A;
        when Normal =>
          Pop(A); Pop(B); Pop(C); Pop(D);
          Push (Ios.Normalof(D, C, B, A));
          S := A;
        when Regmatch =>
          Pop(A); Pop(B); Push (Misc.Reg_Match(A, B));
          S := A;

        -- Dates
        when Clock =>
          Push (Dates.Clock);
        when Dateof =>
          Pop(A); Push (Dates.Time_To_Date(A));
          S := A;
        when Daysof =>
          Pop(A); Push (Dates.Time_To_Days(A));
          S := A;
        when Timeof =>
          Pop(A); Push (Dates.Date_To_Time(A));
          S := A;

        -- Misc
        when Getenv =>
          Pop(A); Push (Misc.Getenv(A));
          S := A;
        when Setexit =>
          Pop (A); Misc.Set_Exit_Code (A);
        when Debugall =>
          Pop(A); Misc.Set_Debug(A);
          S := A;
        when Help =>
          Mcd_Parser.Print_Help;
      end case;
    end if;
  end New_Item;

  function Check_Empty_Stack return Boolean is
  begin
    return Stack.Stack_Size = 0;
  end Check_Empty_Stack;

begin
  Random.Randomize;
end Mcd_Mng;

