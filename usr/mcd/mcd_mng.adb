with Random, As.U.Utils;
with Debug, Input_Dispatcher, Mcd_Parser, Io_Flow;
pragma Elaborate_All (Random);
package body Mcd_Mng is

  -- Current version
  Mcd_Version : constant String := "V16.0";

  package Stack is
    -- What can we store in stack
    subtype Operand_Kind_List is Item_Kind_List range Arbi .. Regi;
    -- On push : Invalid_Item;

    procedure Push (Item : in Item_Rec; Default_Stack : in Boolean := True);

    procedure Pop (Item : out Item_Rec; Default_Stack : in Boolean := True);
    procedure Read (Item : out Item_Rec; Default_Stack : in Boolean := True);

    function Stack_Size (Default_Stack : Boolean := True) return Natural;

    -- Read / Get Nth item of main stack
    procedure Readn (Item : out Item_Rec; N : in Positive);
    procedure Getn (Item : out Item_Rec; N : in Positive);

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
    function Roundiv (L, R : Item_Rec) return Item_Rec;

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
    function Lg      (X : Item_Rec) return Item_Rec;

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

    -- Array: store / retrieve / clear / check_empty: Var[Index]
    -- Var must be a register; Index must be Inte (otherwise Invalid_Argument)
    procedure Store_Array (Val : in Item_Rec;
                           To_Reg : in Item_Rec; Index : in Item_Rec);
    function Retrieve_Array (From_Reg : Item_Rec; Index : Item_Rec)
                            return Item_Rec;
    procedure Clear_Array (Reg : in Item_Rec; Index : Item_Rec);
    function Is_Empty_Array (Reg : Item_Rec; Index : Item_Rec)
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

    procedure Set_Echo (Item : in Item_Rec);
    function Get_Key return Item_Rec;
    function Get_Str return Item_Rec;

    function Strarbi (S : Item_Rec) return Item_Rec;
    function Strfrac (S : Item_Rec) return Item_Rec;
    function Strinte (S : Item_Rec) return Item_Rec;
    function Strreal (S : Item_Rec) return Item_Rec;
    function Strbool (S : Item_Rec) return Item_Rec;
    function Strregi (S : Item_Rec) return Item_Rec;
    function Strprog (S : Item_Rec) return Item_Rec;
    function Strof (Item : Item_Rec) return Item_Rec;
    function Normalof (Item       : Item_Rec;
                       Len        : Item_Rec;
                       Right_Fore : Item_Rec;
                       Gap        : Item_Rec) return Item_Rec;
    -- Invalid_Argument, Argument_Mismatch : exception;
  end Ios;

  package Call_Stack is

    procedure Push (Item : in As.U.Asu_Us);
    function  Pop return As.U.Asu_Us;

    function Level return Natural;

  end Call_Stack;

  package Strings is
    function Strnull (S : Item_Rec) return Item_Rec;
    function Strlen (S : Item_Rec) return Item_Rec;
    function Strcat (S1, S2 : Item_Rec) return Item_Rec;
    function Strsub (S, I1, I2 : Item_Rec) return Item_Rec;
    function Strloc (S, Occ, Pat : Item_Rec) return Item_Rec;
    function Strins (S, I, Sub : Item_Rec) return Item_Rec;
    function Strovw (S, I, Sub : Item_Rec) return Item_Rec;
    function Strdel (S, I, J : Item_Rec) return Item_Rec;
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
    procedure Do_Readn;
    procedure Do_Moven;

    procedure Do_Clear_Extra;

    procedure Do_Rotate_Extra (First : in Boolean; Times : in Item_Rec);

    function Check_Break return Boolean;

    function Do_Delay (The_Delay : Item_Rec) return Boolean;

    procedure Set_Debug (Set : in Item_Rec);

    function Reg_Match (Pattern, Str : Item_Rec) return Item_Rec;

    function Getenv (Item : Item_Rec) return Item_Rec;

    procedure Set_Exit_Code (Code : Item_Rec);
  end Misc;

  package File is
    -- Read and concatenate lines (including Lf) into a string
    procedure Read (File_Name : in Item_Rec; Content : out Item_Rec);
    -- Read and store each string (excluding Lfs) in Content
    -- Ok = False if open error
    procedure Read (File_Name : in Item_Rec;
                    Ok        : out Item_Rec;
                    Content   : out As.U.Utils.Asu_Ua.Unb_Array);
    -- Popo arguments and execute command and arguments
    -- Push resulting string and exit code
    procedure Exec (Cmd    : in Item_Rec;
                    Code   : out Item_Rec;
                    Output : out Item_Rec);
  end File;

  package body Stack is separate;
  package body Operations is separate;
  package body Registers is separate;
  package body Ios is separate;
  package body Call_Stack is separate;
  package body Strings is separate;
  package body Dates is separate;
  package body File is separate;

  procedure Dump_Stack renames Stack.Dump_History;

  -- Values poped and processed by oper
  A, B, C, D : Item_Rec;

  -- Array of lines read from file
  Read_Lines : As.U.Utils.Asu_Ua.Unb_Array;

  -- Saved value (previous top of stack), invalid when of kind Oper
  Invalid_Item : constant Item_Rec
       := (Kind => Oper, Val_Oper => Operator_List'First);
  S : Item_Rec := Invalid_Item;


  -- Subprogram to call when Break
  Breaking : Boolean := False;
  Break_Program : Item_Rec := (Prog, As.U.Tus ("nop"));

  -- Check for Ctrl Break each Item_Check_Period items
  --  to dectect it even if no input (e.g. within a loop)
  Item_Check_Period : constant Positive := 100;
  Nb_Item : Natural := 0;

  -- Misc uses A, Call_Entry, S...
  package body Misc is separate;

  -- Init the manager
  procedure Init is
  begin
    null;
  end Init;

  -- Slices of operations
  subtype Basic_List  is Operator_List range Add     .. Fact;
  subtype Bits_List   is Operator_List range Bitand  .. Shr;
  subtype Compar_List is Operator_List range Equal   .. Smalleq;
  subtype Bool_List   is Operator_List range Boland  .. Bolneg;
  subtype Math_List   is Operator_List range Pi      .. Lg;
  subtype Conv_List   is Operator_List range Toreal  .. Roundat;
  subtype Test_List   is Operator_List range Isarbi  .. Isneg;
  subtype Stack_List  is Operator_List range Ssize   .. Moven;
  subtype Reg_List    is Operator_List range Popr    .. Emptya;
  subtype Extra_List  is Operator_List range Pope    .. Cleare;
  subtype Cond_List   is Operator_List range Ifthen  .. Etfi;
  subtype Prog_List   is Operator_List range Call    .. Callbrk;
  subtype Io_List     is Operator_List range Format  .. Exec;
  subtype String_List is Operator_List range Strnull .. Regmatch;
  subtype Time_List   is Operator_List range Clock   .. Timeof;
  subtype Exec_List   is Operator_List range Nop     .. Help;

  -- Process next item
  procedure New_Item (Item : in Item_Rec; The_End : out End_Status_List) is

    procedure Do_Retn (All_Levels    : in Boolean;
                       Levels        : in Item_Rec;
                       Allow_Level_0 : in Boolean) is
      L : Integer;
      Call_Stack_Level : Natural;
      Ret_All : Boolean;
      -- Subprogram called
      Call_Entry : As.U.Asu_Us;
    begin
      Ret_All := False;
      Call_Stack_Level := Call_Stack.Level;
      Debug.Log (Debug.Oper, "Do_ret from" & Call_Stack_Level'Img);
      if not All_Levels then
        -- Has to be Inte and val NaturaL
        begin
          L := Natural(Levels.Val_Inte);
          Debug.Log (Debug.Oper, "Do_ret" & L'Img);
        exception
          when others => raise Invalid_Argument;
        end;
        if L = 0 then
          return;
        end if;
      else
        -- Return all
        Debug.Log (Debug.Oper, "Do_ret all");
        L := Call_Stack_Level + 1;
      end if;
      -- Can return by one more than level
      if L - 1 > Call_Stack_Level then
        raise Invalid_Argument;
      elsif L - 1 = Call_Stack_Level
      or else (Breaking and then L = Call_Stack_Level) then
        if Allow_Level_0 then
          Ret_All := True;
          L := Call_Stack_Level;
          if Breaking then
            Debug.Log (Debug.Oper, "Return from Break");
            The_End := Exit_Break;
          else
            Debug.Log (Debug.Oper, "Return to exit");
            The_End := Exit_Return;
          end if;
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
      if not Ret_All then
        Input_Dispatcher.Set_Input (Call_Entry.Image);
      end if;
    end Do_Retn;

    procedure Do_Retall is
    begin
      Do_Retn (True, A, True);
    end Do_Retall;

    procedure Do_Ret (Allow_Level_0 : in Boolean := True) is
    begin
      Stack.Pop (A);
      Do_Retn (False, A, Allow_Level_0);
    end Do_Ret;

    procedure Handle_Break is
    begin
      Debug.Log (Debug.Oper, "Handle_Break");
      -- Pop all stacks
      Do_Retall;
      for I in 1 .. Stack.Stack_Size loop
        Stack.Pop (A);
      end loop;
      if Breaking then
        -- Break while breaking => return
        return;
      end if;
      -- Call break program
      Debug.Log (Debug.Oper, "Breaking");
      Breaking := True;
      -- This is not the end yet (despite Do_Retall)
      The_End := Continue;
      Stack.Push (Break_Program);
      Break_Program.Val_Text.Set_Null;
      Misc.Do_Call;
    end Handle_Break;

    use Stack;

    -- Handle a basic numeric operation
    procedure Do_Basic (Kind : Basic_List) is
    begin
      case Kind is
        -- Basic operations on numbers
        when Add =>
          -- push B + A
          Pop(A); Pop(B); Push (Operations.Add(B,A));
          S := A;
        when Sub =>
          -- push B - A
          Pop(A); Pop(B); Push (Operations.Sub(B,A));
          S := A;
        when Mult =>
          -- push B * A
          Pop(A); Pop(B); Push (Operations.Mult(B,A));
          S := A;
        when Div =>
          -- push B / A
          Pop(A); Pop(B); Push (Operations.Div(B,A));
          S := A;
        when Roundiv =>
          -- push Roundiv (B, A)
          Pop(A); Pop(B); Push (Operations.Roundiv(B,A));
          S := A;
        when Pow =>
          -- push B ** A
          Pop(A); Pop(B); Push (Operations.Pow(B,A));
          S := A;
        when Minus =>
          -- push -A
          Pop(A); Push (Operations.Minus(A));
          S := A;
        when Absv =>
          -- push |A|
          Pop(A); Push (Operations.Absv(A));
          S := A;
        when Sqrt =>
          -- push sqrt(A)
          Pop(A); Push (Operations.Sqrt(A));
          S := A;
        when Remind =>
          -- push B rem A
          Pop(A); Pop(B); Push (Operations.Remind(B,A));
          S := A;
        when Fact =>
          -- push A!
          Pop(A); Push (Operations.Fact(A));
          S := A;
      end case;
    end Do_Basic;

    -- Handle a bits operation
    procedure Do_Bits (Kind : Bits_List) is
    begin
      case Kind is
        -- Bits operations
        when Bitand =>
          -- push B and A
          Pop(A); Pop(B); Push (Operations.Bitand(B,A));
          S := A;
        when Bitor =>
          -- push B or A
          Pop(A); Pop(B); Push (Operations.Bitor(B,A));
          S := A;
        when Bitxor =>
          -- push B xor A
          Pop(A); Pop(B); Push (Operations.Bitxor(B,A));
          S := A;
        when Bitneg =>
          -- push not A
          Pop(A); Push (Operations.Bitneg(A));
          S := A;
        when Shl =>
          -- push shl(B,A)
          Pop(A); Pop(B); Push (Operations.Shl(B,A));
          S := A;
        when Shr =>
          -- push shr(B,A)
          Pop(A); Pop(B); Push (Operations.Shr(B,A));
          S := A;
      end case;
    end Do_Bits;

    -- Handle a comparison operation
    procedure Do_Compare (Kind : Compar_List) is
    begin
      case Kind is
        when Equal =>
          -- push B = A
          Pop(A); Pop(B); Push (Operations.Equal(B,A));
          S := A;
        when Diff =>
          -- push B /= A
          Pop(A); Pop(B); Push (Operations.Diff(B,A));
          S := A;
        when Greater =>
          -- push B > A
          Pop(A); Pop(B); Push (Operations.Greater(B,A));
          S := A;
        when Smaller =>
          -- push B < A
          Pop(A); Pop(B); Push (Operations.Smaller(B,A));
          S := A;
        when Greateq =>
          -- push B >= A
          Pop(A); Pop(B); Push (Operations.Greateq(B,A));
          S := A;
        when Smalleq =>
          -- push B <= A
          Pop(A); Pop(B); Push (Operations.Smalleq(B,A));
          S := A;
      end case;
    end Do_Compare;

    -- Handle a boolean operation
    procedure Do_Boolean (Kind : Bool_List) is
    begin
      case Kind is
        when Boland =>
          -- push B and then A
          Pop(A); Pop(B); Push (Operations.Boland(B,A));
          S := A;
        when Bolor =>
          -- push B or else A
          Pop(A); Pop(B); Push (Operations.Bolor(B,A));
          S := A;
        when Bolxor =>
          -- push B xor A
          Pop(A); Pop(B); Push (Operations.Bolxor(B,A));
          S := A;
        when Bolneg =>
          -- push not A
          Pop(A); Push (Operations.Bolneg(A));
          S := A;
      end case;
    end Do_Boolean;

    -- Handle a math (trigo, log) operation
    procedure Do_Math (Kind : Math_List) is
    begin
      case Kind is
        when Pi =>
          -- push value of Pi
          Push( (Kind => Real,
                 Val_Real => My_Math.Pi) );
        when Sin =>
          -- push sin(A)
          Pop(A); Push (Operations.Sin(A));
          S := A;
        when Cos =>
          -- push cos(A)
          Pop(A); Push (Operations.Cos(A));
          S := A;
        when Tan =>
          -- push tg(A)
          Pop(A); Push (Operations.Tan(A));
          S := A;
        when Asin =>
          -- push asin(A)
          Pop(A); Push (Operations.Asin(A));
          S := A;
        when Acos =>
          -- push acos(A)
          Pop(A); Push (Operations.Acos(A));
          S := A;
        when Atan =>
          -- push atg(A)
          Pop(A); Push (Operations.Atan(A));
          S := A;
        when Rnd =>
          -- push random value
          S := (Kind => Real,
                 Val_Real => My_Math.Real(Random.Gen.Float_Random));
          Push(S);
        when Epsilon =>
          -- push value of Epsilon
          Push( (Kind => Real,
                 Val_Real => 1.0E-10) );
        when Exp =>
          -- push value of e
          Push( (Kind => Real,
                 Val_Real => My_Math.E) );
        when Ln =>
          -- push ln(A)
          Pop(A); Push (Operations.Ln(A));
          S := A;
        when Lg =>
          -- push lg(A)
          Pop(A); Push (Operations.Lg(A));
          S := A;
      end case;
    end Do_Math;

    -- Handle numeric convertion
    procedure Do_Convert (Kind : Conv_List) is
    begin
      case Kind is
        -- Numerical conversion
        when Toreal =>
          -- push A converted to real
          Pop(A); Push (Operations.Toreal(A));
          S := A;
        when Tointe =>
          -- push A converted to inte
          Pop(A); Push (Operations.Tointe(A));
          S := A;
        when Toarbi =>
          -- push A converted to arbi
          Pop(A); Push (Operations.Toarbi(A));
          S := A;
        when Round =>
          -- push A rounded
          Pop(A); Push (Operations.Round(A));
          S := A;
        when Trunc =>
          -- push A truncated
          Pop(A); Push (Operations.Trunc(A));
          S := A;
        when Int =>
          -- push int part of A
          Pop(A); Push (Operations.Int(A));
          S := A;
        when Frac =>
          -- push frac part of A
          Pop(A); Push (Operations.Frac(A));
          S := A;
        when Maxint =>
          -- push value of maxint
          A := Operations.Maxint;
          Push (A);
          S := A;
        when Minint =>
          -- push value of minint
          A := Operations.Minint;
          Push (A);
          S := A;
        when Roundif =>
          -- if A within inte then push round(A) elas push(A)
          Pop(A); Push (Operations.Roundif(A));
          S := A;
        when Dms =>
          -- push A converted to deg.MiSe
          Pop(A); Push (Operations.Dms(A));
          S := A;
        when Msd =>
          -- push A (deg.MiSe) converted to A.frac
          Pop(A); Push (Operations.Msd(A));
          S := A;
        when Mkfrac =>
          -- push fraction B:A
          Pop(A); Pop(B); Push (Operations.Mkfrac(B, A));
          S := A;
        when Numerof =>
          -- push numerator of Fraction A
          Pop(A); Push (Operations.Numerof(A));
          S := A;
        when Denomof =>
          -- push denominatorof Fraction A
          Pop(A); Push (Operations.Denomof(A));
          S := A;
        when Proport =>
          -- push A * B / C
          Pop(A); Pop(B); Pop(C); Push (Operations.Proport(C,B,A));
          S := A;
        when Roundat =>
          -- push B rounded at A digits
          Pop(A); Pop(B); Push (Operations.Roundat(B,A));
          S := B;
      end case;
    end Do_Convert;

    -- Handle test on type or avlue
    procedure Do_Test (Kind : Test_List) is
    begin
      case Kind is
        when Isarbi =>
          -- push whether A is arbitrari
          Pop(A); Push (Operations.Isarbi(A));
          S := A;
        when Isfrac =>
          -- push whether A is fraction
          Pop(A); Push (Operations.Isfrac(A));
          S := A;
        when Isinte =>
          -- push whether A is integer
          Pop(A); Push (Operations.Isinte(A));
          S := A;
        when Isreal =>
          -- push whether A is real
          Pop(A); Push (Operations.Isreal(A));
          S := A;
        when Isbool =>
          -- push whether A is boolean
          Pop(A); Push (Operations.Isbool(A));
          S := A;
        when Isstr =>
          -- push whether A is string
          Pop(A); Push (Operations.Isstr(A));
          S := A;
        when Isreg =>
          -- push whether A is register
          Pop(A); Push (Operations.Isreg(A));
          S := A;
        when Isprog =>
          -- push whether A is sub-programm
          Pop(A); Push (Operations.Isprog(A));
          S := A;
        when Ispos =>
          -- push whether A is positive
          Pop(A); Push (Operations.Ispos(A));
          S := A;
        when Isnul =>
          -- push whether A is null
          Pop(A); Push (Operations.Isnul(A));
          S := A;
        when Isnotnul =>
          -- push whether A is not null
          Pop(A); Push (Operations.Isnotnul(A));
          S := A;
        when Isneg =>
          -- push whether A is negative
          Pop(A); Push (Operations.Isneg(A));
          S := A;
      end case;
    end Do_Test;

    -- Handle main stack management
    procedure Do_Stack (Kind : Stack_List) is
    begin
      case Kind is
        -- Main stack management
        when Ssize =>
          -- push stack size
          Push( (Kind => Inte, Val_Inte => My_Math.Inte(Stack.Stack_Size)));
        when Swap =>
          -- push A push B
          Pop(A); Pop(B); Push(A); Push(B);
        when Swap3 =>
          -- push A push B push C
          Pop(A); Pop(B); Pop(C); Push(A); Push(B); Push(C);
        when Swap2 =>
          -- push B push C push A
          Pop(A); Pop(B); Pop(C); Push(B); Push(C); Push(A);
        when Dup =>
          -- push A push A
          Read(A); Push(A);
        when Prevtop =>
          -- push S
          if S.Kind = Oper then
            raise Invalid_Argument;
          end if;
          Push (S);
        when Pop =>
          -- pop A
          Pop(A);
        when Popn =>
          -- pop B A times
          Misc.Do_Popn;
        when Readn =>
          -- push the Ath element of stack
          Misc.Do_Readn;
        when Moven =>
          -- move on top the Ath element of stack
          Misc.Do_Moven;
      end case;
    end Do_Stack;

    -- Handle registers and arrays
    procedure Do_Register (Kind : Reg_List) is
    begin
      case Kind is
        -- Registers and arrays
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
        when Cleara =>
          -- Clear B[A]
          Pop(A); Pop(B);
          Registers.Clear_Array(B, A);
        when Emptya =>
          -- True if B[A] is empty
          Pop(A); Pop(B);
          Push (Registers.Is_Empty_Array(B, A));
      end case;
    end Do_Register;

    -- Handle extra stack
    procedure Do_Extra (Kind : Extra_List) is
    begin
      case Kind is
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
           -- push estack size
           Push( (Kind => Inte,
                  Val_Inte => My_Math.Inte(Stack.Stack_Size(
                                Default_Stack => False))));
        when Cleare =>
           -- clear estack
           Misc.Do_Clear_Extra;
      end case;
    end Do_Extra;

    -- Handle conditions
    procedure Do_Condition (Kind : Cond_List) is
    begin
      case Kind is
        -- Conditions
        when Ifthen =>
          -- if B then push A
          Pop(A); Pop(B);
          if Operations.Is_True(B) then
            Push(A);
          end if;
          S := A;
        when Ifte =>
          -- if C then push B else push A
          Pop(A); Pop(B); Pop(C); Push (Operations.Ifte(C,B,A));
          S := A;
        when Etfi =>
          -- if A then push C else push B
          Pop(A); Pop(B); Pop(C); Push (Operations.Ifte(A,C,B));
          S := A;
      end case;
    end Do_Condition;

    -- Handle subprogramm
    procedure Do_Prog (Kind : Prog_List) is
    begin
      case Kind is
        when Call =>
          -- call A
          Misc.Do_Call;
        when Ifcall =>
          -- if B then call A
          Pop(A);
          Pop(B);
          if Operations.Is_True(B) then
            Push(A);
            Misc.Do_Call;
          end if;
        when Ret =>
          -- return
          Push( (Kind => Inte, Val_Inte => 1) );
          Do_Ret;
        when Retn =>
          -- return A levels
          Read (S);
          Do_Ret;
        when Retall =>
          -- return all levels
          Do_Retall;
        when Ifret =>
          -- if A then return
          Pop(A);
          if Operations.Is_True(A) then
            Push( (Kind => Inte, Val_Inte => 1) );
            Do_Ret;
          end if;
          S := A;
        when Ifretn =>
          -- if B then return A levels
          Pop(A);
          Pop(B);
          if Operations.Is_True(B) then
            Push(A);
            Do_Ret;
          end if;
          S := A;
        when Ifretall =>
          -- if A then return all levels
          Pop(A);
          if Operations.Is_True(A) then
            Do_Retall;
          end if;
          S := A;
        when Retacal =>
          -- return and call A
          Push( (Kind => Inte, Val_Inte => 1) );
          -- Return but forbid level 0
          Do_Ret(False);
          Misc.Do_Call;
        when Callbrk =>
          -- set program to be called on break
          Pop(A);
          if A.Kind /= Prog then
            raise Invalid_Argument;
          end if;
          Break_Program := A;
          S := A;
      end case;
    end Do_Prog;

    -- Handle outputs and inputs
    procedure Do_Io (Kind : Io_List) is
    begin
      case Kind is
        -- Output
        when Format =>
          -- set foramt to A
          Pop(A); Ios.Format(A);
          S := A;
        when Obase =>
          -- set obase to A
          Pop(A); Ios.Set_Obase(A);
          S := A;
        when Put =>
          -- put A
          Pop(A); Ios.Put(A);
          S := A;
        when Newl =>
          -- new line
          Ios.New_Line;
        when Putl =>
          -- put_line A
          Pop(A); Ios.Put_Line(A);
          S := A;
        -- Input
        when Getenv =>
          -- push getenv(A)
          Pop(A); Push (Misc.Getenv(A));
          S := A;
        when Inecho =>
          -- set echo to A
          Pop(A); Ios.Set_Echo (A);
        when Inkey =>
          -- push Get_Key
          Push (Ios.Get_Key);
        when Instr =>
          -- push Get_Str
          Push (Ios.Get_Str);
        when Readfile =>
          -- read content of A as string
          Pop(A);
          S := A;
          File.Read(A, B);
          Push(B);
        when Readlins =>
          -- push lines of the file, (first line on top)
          Pop(A);
          File.Read(A, B, Read_Lines);
          if B.Val_Bool then
            for I in reverse 1 .. Read_Lines.Length loop
              Push ((Kind => Chrs, Val_Text => Read_Lines.Element(I)));
            end loop;
            Push ((Kind => Inte, Val_Inte => My_Math.Inte(Read_Lines.Length)));
          else
            Push (B);
          end if;
          S := A;
        when Exec =>
          -- pop command and arguments, push result of command and exit code
          Pop(A);
          File.Exec(A, B, C);
          Push (C);
          Push (B);
          S := A;
      end case;
    end Do_Io;

    -- Handle strings
    procedure Do_String (Kind : String_List) is
    begin
      case Kind is
        -- String management and conversions
        when Strnull =>
          -- push True if A is empty
          Pop(A); Push (Strings.Strnull(A));
          S := A;
        when Strlen =>
          -- push string length of A
          Pop(A); Push (Strings.Strlen(A));
          S := A;
        when Strcat =>
          -- push B & A
          Pop(A); Pop(B); Push (Strings.Strcat(B, A));
          S := A;
        when Strsub =>
          -- push C(B..A)
          Pop(A); Pop(B); Pop(C); Push (Strings.Strsub(C, B, A));
          S := A;
        when Strloc =>
          -- push index of Bth occurence of A in C
          Pop(A); Pop(B); Pop(C); Push (Strings.Strloc(C, B, A));
          S := A;
        when Strins =>
          -- push C after inserting A from pos B in it
          Pop(A); Pop(B); Pop(C); Push (Strings.Strins(C, B, A));
        when Strovw =>
          -- push C overwriten by A from pos B
          Pop(A); Pop(B); Pop(C); Push (Strings.Strovw(C, B, A));
          S := A;
        when Strdel =>
          -- push C without C(B..A)
          Pop(A); Pop(B); Pop(C); Push (Strings.Strdel(C, B, A));
          S := A;
        when Strupp =>
          -- push A converted to uppercase
          Pop(A); Push (Strings.Strupp(A));
          S := A;
        when Strlow =>
          -- push A converted to lowercase
          Pop(A); Push (Strings.Strlow(A));
          S := A;
        when Strmix =>
          -- push A converted to mixed-case
          Pop(A); Push (Strings.Strmix(A));
          S := A;
        when Strarbi =>
          -- push A converted to arbitrary
          Pop(A); Push (Ios.Strarbi(A));
          S := A;
        when Strfrac =>
          -- push A converted to fraction
          Pop(A); Push (Ios.Strfrac(A));
          S := A;
        when Strinte =>
          -- push A converted to integer
          Pop(A); Push (Ios.Strinte(A));
          S := A;
        when Strreal =>
          -- push A converted to real
          Pop(A); Push (Ios.Strreal(A));
          S := A;
        when Strbool =>
          -- push A converted to boolean
          Pop(A); Push (Ios.Strbool(A));
          S := A;
        when Strregi =>
          -- push A converted to register
          Pop(A); Push (Ios.Strregi(A));
          S := A;
        when Strprog =>
          -- push A converted to subprogram
          Pop(A); Push (Ios.Strprog(A));
          S := A;
        when Strof =>
          -- push A converted to string
          Pop(A); Push (Ios.Strof(A));
          S := A;
        when Normal =>
          -- push normalised string of D
          --   C is Len, B is Right or Fore, A is Pad
          Pop(A); Pop(B); Pop(C); Pop(D);
          Push (Ios.Normalof(D, C, B, A));
          S := A;
        when Regmatch =>
          -- push whether B matches regex A
          Pop(A); Pop(B); Push (Misc.Reg_Match(A, B));
          S := A;
      end case;
    end Do_String;

    -- Handle time
    procedure Do_Time (Kind : Time_List) is
    begin
      case Kind is
        when Clock =>
          -- push current time
          Push (Dates.Clock);
        when Dateof =>
          -- push time image of A
          Pop(A); Push (Dates.Time_To_Date(A));
          S := A;
        when Daysof =>
          -- push delay in days of A
          Pop(A); Push (Dates.Time_To_Days(A));
          S := A;
        when Timeof =>
          -- push time corresponding to time image A
          Pop(A); Push (Dates.Date_To_Time(A));
          S := A;
      end case;
    end Do_Time;

    -- Handle program execution
    procedure Do_Exec (Kind : Exec_List) is
    begin
      case Kind is
        -- Miscelaneous
        when Nop =>
          -- no operation
          null;
        when Sleep =>
          -- sleep A seconds
          Pop(A);
          S := A;
          Nb_Item := 0;
          if Misc.Do_Delay(A) then
            Handle_Break;
          end if;
        when Version =>
          -- push Mcd version
          S := (Kind => Chrs,
                 Val_Text => As.U.Tus (Mcd_Version));
          Push(S);
        when Setexit =>
          -- set exit code to A
          Pop (A); Misc.Set_Exit_Code (A);
        when Debugall =>
          -- set debug to True or False
          Pop(A); Misc.Set_Debug(A);
          S := A;
        when Help =>
          -- put help
          Mcd_Parser.Print_Help (Command => True);
      end case;
    end Do_Exec;

  begin
    -- Default, except Ret
    The_End := Continue;
    -- Check for Ctrl C
    if Nb_Item = Item_Check_Period then
      Nb_Item := 0;
      if Misc.Check_Break then
        -- Set Break_Program
        Handle_Break;
        -- Discard current instruction
        return;
      end if;
    else
      Nb_Item := Nb_Item + 1;
    end if;
    -- Dispatch
    Clear_History;
    if Item.Kind /= Oper then
      -- Push operand
      Push(Item);
    else -- Operator
      Debug.Log (Debug.Oper, Item);
      -- The big case on all operations
      case Item.Val_Oper is

        when Basic_List =>
          -- Basic nueric operations
          Do_Basic (Item.Val_Oper);
        when Bits_List =>
          -- Bits operations
          Do_Bits (Item.Val_Oper);
        when Compar_List =>
          -- Comparisons
          Do_Compare (Item.Val_Oper);
        when Bool_List =>
          -- Boolean operations
          Do_Boolean (Item.Val_Oper);
        when Math_List =>
          -- Math (trigo and logarithms) operations
          Do_Math (Item.Val_Oper);
        when Conv_List =>
          -- Convertion operations
          Do_Convert (Item.Val_Oper);
        when Test_List =>
          -- Testing operations
          Do_Test (Item.Val_Oper);
        when Stack_List =>
          -- Main stack operations
          Do_Stack (Item.Val_Oper);
        when Reg_List =>
          -- Registers and arrays operations
          Do_Register (Item.Val_Oper);
        when Extra_List =>
          -- Extra stack operations
          Do_Extra (Item.Val_Oper);
        when Cond_List =>
          -- Conditions operations
          Do_Condition (Item.Val_Oper);
        when Prog_List =>
          -- Subprogram operations
          Do_Prog (Item.Val_Oper);
        when Io_List =>
          -- Input and output operations
          Do_Io (Item.Val_Oper);
        when String_List =>
          -- String operations
          Do_String (Item.Val_Oper);
        when Time_List =>
          -- Time operations
          Do_Time (Item.Val_Oper);
        when Exec_List =>
          -- Program execution operations
          Do_Exec (Item.Val_Oper);
      end case;
    end if;
  exception
    when Io_Flow.End_Error =>
      Handle_Break;
  end New_Item;

  function Check_Empty_Stack return Boolean is (Stack.Stack_Size = 0);

begin
  Random.Gen.Randomize;
end Mcd_Mng;

