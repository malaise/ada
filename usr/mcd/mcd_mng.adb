with Text_Io;
with Text_Handler;
with Random;
with Debug, Input_Dispatcher, Parser;
pragma Elaborate(Random);
package body Mcd_Mng is


  A, B, C : Item_Rec;
  Call_Entry : Text_Handler.Text (Input_Dispatcher.Max_String_Lg);

  package Stack is 
    -- What can we store in stack
    subtype Operand_Kind_List is Item_Kind_List range Inte .. Regi;
    -- On push : INVALID_ITEM;

    procedure Push (Item : in Item_Rec; Default_Stack : in Boolean := True);

    procedure Pop (Item : out Item_Rec; Default_Stack : in Boolean := True);
    procedure Read (Item : out Item_Rec; Default_Stack : in Boolean := True);

    function Stack_Size (Default_Stack : Boolean := True) return Natural;

    procedure Popf (Item : out Item_Rec);
  end Stack;

  package Operations is

    function Is_True (X : Item_Rec) return Boolean;

    -- INTE,INTE->INTE or REAL,REAL->REAL
    function Add     (L, R : Item_Rec) return Item_Rec;
    function Sub     (L, R : Item_Rec) return Item_Rec;
    function Mult    (L, R : Item_Rec) return Item_Rec;
    function Div     (L, R : Item_Rec) return Item_Rec;
    function Pow     (L, R : Item_Rec) return Item_Rec;

    -- INTE,INTE->INTE
    function Remind  (L, R : Item_Rec) return Item_Rec;

    -- INTE,INTE->INTE
    function Bitand  (L, R : Item_Rec) return Item_Rec;
    function Bitor   (L, R : Item_Rec) return Item_Rec;
    function Bitxor  (L, R : Item_Rec) return Item_Rec;
    function Shl     (L, R : Item_Rec) return Item_Rec;
    function Shr     (L, R : Item_Rec) return Item_Rec;

    -- INTE->INTE or REAL->REAL
    function Minus   (X : Item_Rec) return Item_Rec;
    function Absv    (X : Item_Rec) return Item_Rec;

    -- INTE->INTE
    function Bitneg  (X : Item_Rec) return Item_Rec;

    -- INTE,INTE->BOOL or REAL,REAL->BOOL or BOOL,BOOL->BOOL 
    function Equal   (L, R : Item_Rec) return Item_Rec;
    function Diff    (L, R : Item_Rec) return Item_Rec;
    function Greater (L, R : Item_Rec) return Item_Rec;
    function Smaller (L, R : Item_Rec) return Item_Rec;
    function Greateq (L, R : Item_Rec) return Item_Rec;
    function Smalleq (L, R : Item_Rec) return Item_Rec;

    -- INTE->REAL
    function Toreal  (X : Item_Rec) return Item_Rec;

    -- REAL -> INTE
    function Round   (X : Item_Rec) return Item_Rec;
    function Trunc   (X : Item_Rec) return Item_Rec;

    -- REAL->REAL
    function Int     (X : Item_Rec) return Item_Rec;
    function Frac    (X : Item_Rec) return Item_Rec;

    -- *->BOOL
    function Isreal  (X : Item_Rec) return Item_Rec;
    function Isinte  (X : Item_Rec) return Item_Rec;
    function Isstr   (X : Item_Rec) return Item_Rec;
    function Isreg   (X : Item_Rec) return Item_Rec;

    -- BOOL,BOOL->BOOL
    function Boland  (L, R : Item_Rec) return Item_Rec;
    function Bolor   (L, R : Item_Rec) return Item_Rec;
    function Bolxor  (L, R : Item_Rec) return Item_Rec;

    -- BOOL->BOOL
    function Bolneg  (X : Item_Rec) return Item_Rec;

    -- BOOL,*,*->*
    function Ifte    (X, A, B : Item_Rec) return Item_Rec;

    -- REAL -> REAL
    function Sin     (X : Item_Rec) return Item_Rec;
    function Cos     (X : Item_Rec) return Item_Rec;
    function Tan     (X : Item_Rec) return Item_Rec;
    function ASin    (X : Item_Rec) return Item_Rec;
    function ACos    (X : Item_Rec) return Item_Rec;
    function ATan    (X : Item_Rec) return Item_Rec;


    -- Argument does not mach operator
    -- INVALID_ARGUMENT : exception;
    -- Arguments are not compatible to each other
    -- ARGUMENT_MISMATCH : exception;
  end Operations;

  package Registers is
    subtype Register_Content_List is Item_Kind_List range Inte .. Regi;

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
 
    -- Valid registers  are 'a' .. 'z' and 'A' .. 'Z'
    -- INVALID_REGISTER : exception;

    -- Valid contents are INTE REAL BOOL PROG CHRS
    -- INVALID_ARGUMENT : exception;

    -- Nothing to retrieve
    -- EMTPY_REGISTER : exception;
  end Registers;

  package Ios is

    procedure Set_Obase (Base : in Item_Rec);

    procedure Format (Item : in Item_Rec);
    procedure Put (Item : in Item_Rec);
    procedure Put_Line (Item : in Item_Rec);
    procedure New_Line;

    function Strinte (S : Item_Rec) return Item_Rec;
    function Strreal (S : Item_Rec) return Item_Rec;
    function Strbool (S : Item_Rec) return Item_Rec;
    function Strregi (S : Item_Rec) return Item_Rec;
    function Strof (Item : Item_Rec) return Item_Rec;
    -- INVALID_ARGUMENT, ARGUMENT_MISMATCH : exception;
  end Ios;

  package Call_Stack is 

    procedure Push (Item : in String);
    function  Pop return String;

    function Level return Natural;

  end Call_Stack;

  package Strings is
    function Strlen (S : Item_Rec) return Item_Rec;
    function Strcat (S1, S2 : Item_Rec) return Item_Rec;
    function Strsub (S, I1, I2 : Item_Rec) return Item_Rec;
    function Strloc (Occ, Pat, S : Item_Rec) return Item_Rec;
    function Strrep (I, Pat, S : Item_Rec) return Item_Rec;
    function Strupp (S : Item_Rec) return Item_Rec;
    function Strlow (S : Item_Rec) return Item_Rec;

  end Strings;

  package body Stack is separate;
  package body Operations is separate;
  package body Registers is separate;
  package body Ios is separate;
  package body Call_Stack is separate;
  package body Strings is separate;

  function Is_Register (C : in Character) return Boolean 
                       renames Registers.Is_Register;

  procedure New_Item (Item : in Item_Rec; The_End : out Boolean) is
    use Stack;

    procedure Do_Call is
    begin
      if Debug.Debug_Level_Array(Debug.Oper) then
        Text_Io.Put_Line("Mng: Do_call");
      end if;
      Pop(A);
      if A.Kind /= Prog then
        raise Invalid_Argument;
      end if;
      if Call_Stack.Level /= 0 then
        -- Save contect;
        Text_Handler.Set(Call_Entry, Input_Dispatcher.Get_Remaining);
        -- Even if end of subprog, this is not stdin
        if Text_Handler.Empty(Call_Entry) then
          Text_Handler.Set(Call_Entry, " ");
        end if;
        Call_Stack.Push (Text_Handler.Value(Call_Entry));
      else
        -- Dummy context
        Call_Stack.Push ("");
      end if;
      -- Call
      if A.Val_Len = 0 then
        -- Empty subprogram : not stdin
        Input_Dispatcher.Set_Input(" ");
      else
        Input_Dispatcher.Set_Input(A.Val_Text(1 .. A.Val_Len));
      end if;
    end Do_Call;

    procedure Do_Retn (All_Levels    : in Boolean;
                       Levels        : in Item_Rec;
                       Allow_Level_0 : in Boolean) is
      L : Integer;
      Call_Stack_Level : Natural;
    begin
      if Debug.Debug_Level_Array(Debug.Oper) then
        Text_Io.Put_Line("Mng: Do_ret");
      end if;
      Call_Stack_Level := Call_Stack.Level;
      if not All_Levels then
        -- has to be INTE and val NATURAL
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
          The_End := True;
          return;
        else
          -- RETACAL from level 0
          raise Invalid_Argument;
        end if;
      end if;
      -- Return N times
      for I in reverse 1 .. L loop
        -- Restart form previous context
        Text_Handler.Set(Call_Entry, Call_Stack.Pop);
      end loop;
      Input_Dispatcher.Set_Input(Text_Handler.Value(Call_Entry));
    end Do_Retn;

    procedure Do_Retall is
    begin
      Do_Retn(True, A, True);
    end Do_Retall;

    procedure Do_Ret (Allow_Level_0 : in Boolean := True) is
    begin
      Pop(A);
      Do_Retn(False, A, Allow_Level_0);
    end Do_Ret;
    

    procedure Do_Popn is
      N : Natural;
    begin
      Pop(A);
      -- has to be INTE and val NATURAL
      begin
        N := Natural(A.Val_Inte);
      exception
        when others => raise Invalid_Argument;
      end;
      for I in 1 .. N loop
        Pop(A);
      end loop;
    end Do_Popn;

    procedure Do_Clear_Extra is
      Rec : Item_Rec;
    begin
     For I in 1 .. Stack.Stack_Size(False) loop
       Stack.Pop(Rec, False);
     end loop;
    end Do_Clear_Extra;

    procedure Do_Delay(The_Delay : in Item_Rec) is
    begin
      if The_Delay.Kind = Inte then
        delay Duration(The_Delay.Val_Inte);
      elsif The_Delay.Kind = Real then
        delay Duration(The_Delay.Val_Real);
      else
        raise Invalid_Argument;
      end if;
    exception
      when others =>
        raise Invalid_Argument;
    end Do_Delay;

  begin
    -- Default, except RET
    The_End := False;
    -- Dispatch
    if Item.Kind /= Oper then
      -- Push operand
      Stack.Push(Item);
    else -- OPERATOR
      if Debug.Debug_Level_Array(Debug.Oper) then
        Text_Io.Put("Mng: ");
        Debug.Put(Item);
        Text_Io.New_Line;
      end if;
      case Item.Val_Oper is 
        -- These 5 I do it myself
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
          Do_Delay(A);

        when Popn =>
          Do_Popn;

        -- These are operations
        when Add =>
          Pop(A); Pop(B); Push (Operations.Add(B,A));
        when Sub =>
          Pop(A); Pop(B); Push (Operations.Sub(B,A));
        when Mult =>
          Pop(A); Pop(B); Push (Operations.Mult(B,A));
        when Div =>
          Pop(A); Pop(B); Push (Operations.Div(B,A));
        when Remind =>
          Pop(A); Pop(B); Push (Operations.Remind(B,A));
        when Pow =>
          Pop(A); Pop(B); Push (Operations.Pow(B,A));
        when Bitand =>
          Pop(A); Pop(B); Push (Operations.Bitand(B,A));
        when Bitor =>
          Pop(A); Pop(B); Push (Operations.Bitor(B,A));
        when Bitxor =>
          Pop(A); Pop(B); Push (Operations.Bitxor(B,A));
        when Shl =>
          Pop(A); Pop(B); Push (Operations.Shl(B,A));
        when Shr =>
          Pop(A); Pop(B); Push (Operations.Shr(B,A));
        when Minus =>
          Pop(A); Push (Operations.Minus(A));
        when Absv =>
          Pop(A); Push (Operations.Absv(A));
        when Bitneg =>
          Pop(A); Push (Operations.Bitneg(A));
        when Equal =>
          Pop(A); Pop(B); Push (Operations.Equal(B,A));
        when Diff =>
          Pop(A); Pop(B); Push (Operations.Diff(B,A));
        when Greater =>
          Pop(A); Pop(B); Push (Operations.Greater(B,A));
        when Smaller =>
          Pop(A); Pop(B); Push (Operations.Smaller(B,A));
        when Greateq =>
          Pop(A); Pop(B); Push (Operations.Greateq(B,A));
        when Smalleq =>
          Pop(A); Pop(B); Push (Operations.Smalleq(B,A));
        when Toreal =>
          Pop(A); Push (Operations.Toreal(A));
        when Round =>
          Pop(A); Push (Operations.Round(A));
        when Trunc =>
          Pop(A); Push (Operations.Trunc(A));
        when Int =>
          Pop(A); Push (Operations.Int(A));
        when Frac =>
          Pop(A); Push (Operations.Frac(A));
        when Isreal =>
          Pop(A); Push (Operations.Isreal(A));
        when Isinte =>
          Pop(A); Push (Operations.Isinte(A));
        when Isstr =>
          Pop(A); Push (Operations.Isstr(A));
        when Isreg =>
          Pop(A); Push (Operations.Isreg(A));
        when Boland =>
          Pop(A); Pop(B); Push (Operations.Boland(B,A));
        when Bolor =>
          Pop(A); Pop(B); Push (Operations.Bolor(B,A));
        when Bolxor =>
          Pop(A); Pop(B); Push (Operations.Bolxor(B,A));
        when Bolneg =>
          Pop(A); Push (Operations.Bolneg(A));
        when Pi =>
          Push( (Kind => Real,
                 Val_Real => My_Math.Real(My_Math.Pi)) );
        when Sin =>
          Pop(A); Push (Operations.Sin(A));
        when Cos =>
          Pop(A); Push (Operations.Cos(A));
        when Tan =>
          Pop(A); Push (Operations.Tan(A));
        when Asin =>
          Pop(A); Push (Operations.Asin(A));
        when Acos =>
          Pop(A); Push (Operations.Acos(A));
        when Atan =>
          Pop(A); Push (Operations.Atan(A));

        -- Conditions
        when Ifthen =>
          Pop(A); Pop(B);
          if Operations.Is_True(B) then
            Push(A);
          end if;
        when Ifte =>
          Pop(A); Pop(B); Pop(C); Push (Operations.Ifte(C,B,A));
        when Etfi =>
          Pop(A); Pop(B); Pop(C); Push (Operations.Ifte(A,C,B));
 
        when Obase =>
          Pop(A); Ios.Set_Obase(A);

        -- These are about registers
        when Popr =>
          -- store B in reg A
          Pop(A); Pop(B); Registers.Store(B, A);
        when Copyr =>
          -- store B in reg A and push B
          Pop(A); Read(B); Registers.Store(B, A);
        when Pushr =>
          -- A -> push content of reg A
          Pop(A); Push(Registers.Retrieve(A));
        when Clearr =>
          -- Clear reg A
          Pop(A); Registers.Clear(A);
        when Clearall =>
          -- Clear all registers
          Registers.Clear_All;
        when Emptyr =>
          -- True is reg A is empty
          Pop(A); Push(Registers.Is_Empty(A));
        when Nextr =>
          -- Reg A -> Reg B
          Pop(A); Registers.Next(A); Push(A);
        when Prevr =>
          -- Reg A -> Reg B
          Pop(A); Registers.Prev(A); Push(A);
        when Regind =>
          -- Index of register
          Pop(A); Push(Registers.Index_Of(A));
        when Indreg =>
          -- Register at index
          Pop(A); Push(Registers.Register_At(A));

        -- Stack size
        when Ssize =>
          Push( (Kind => Inte, Val_Inte => My_Math.Inte(Stack.Stack_Size)));

        -- Extra stack
        when Pope =>
          -- pushe A
          Pop(A); Push (A, Default_Stack => False);
        when Copye =>
          -- pushe A push A
          Read(A); Push (A, Default_Stack => False); 
        when Pushle =>
          -- pushe X push X
          Pop(A, Default_Stack => False); Push (A);
        when Pushfe =>
          -- pushe X push X
          Popf(A); Push (A);
        when Esize =>
           Push( (Kind => Inte,
                  Val_Inte => My_Math.Inte(Stack.Stack_Size(
                                Default_Stack => False))));
        when Cleare =>
           Do_Clear_Extra;


        -- These ones are subprogram
        when Call =>
          Do_Call;
        when Ifcall =>
          Pop(A);
          Pop(B);
          if Operations.Is_True(B) then
            Push(A);
            Do_Call;
          end if;

        when Ret =>
          Push( (Kind => Inte, Val_Inte => 1) );
          Do_Ret;
        when Retn =>
          Do_Ret;
        when Retall =>
          Do_Retall;
        when Ifret =>
          Pop(A);
          if Operations.Is_True(A) then
            Push( (Kind => Inte, Val_Inte => 1) );
            Do_Ret;
          end if;
        when Ifretn =>
          Pop(A);
          Pop(B);
          if Operations.Is_True(B) then
            Push(A);
            Do_Ret;
          end if;
        when Ifretall =>
          Pop(A);
          if Operations.Is_True(A) then
            Do_Retall;
          end if;

        when Retacal =>
          Push( (Kind => Inte, Val_Inte => 1) );
          -- Return but forbid level 0
          Do_Ret(False);
          Do_Call;

        -- PUTs
        when Format =>
          Pop(A); Ios.Format(A);
        when Put =>
          Pop(A); Ios.Put(A);
        when Putl =>
          Pop(A); Ios.Put_Line(A);
        when Newl =>
          Ios.New_Line;


        -- Strings
        when Strlen =>
          Pop(A); Push (Strings.Strlen(A));
        when Strcat =>
          Pop(A); Pop(B); Push (Strings.Strcat(B, A));
        when Strsub =>
          Pop(A); Pop(B); Pop(C); Push (Strings.Strsub(C, B, A));
        when Strloc =>
          Pop(A); Pop(B); Pop(C); Push (Strings.Strloc(C, B, A));
        when Strrep =>
          Pop(A); Pop(B); Pop(C); Push (Strings.Strrep(C, B, A));
        when Strupp =>
          Pop(A); Push (Strings.Strupp(A));
        when Strlow =>
          Pop(A); Push (Strings.Strlow(A));
        when Strreal =>
          Pop(A); Push (Ios.Strreal(A));
        when Strinte =>
          Pop(A); Push (Ios.Strinte(A));
        when Strbool =>
          Pop(A); Push (Ios.Strbool(A));
        when Strregi =>
          Pop(A); Push (Ios.Strregi(A));
        when Strof =>
          Pop(A); Push (Ios.Strof(A));

        when Help =>
          Parser.Print_Help;
      end case;
    end if;
  end New_Item;

  function Check_Empty_Stack return Boolean is
  begin
    return Stack.Stack_Size = 0;
  end;

begin
  Random.Randomize;
end Mcd_Mng;

