with My_Math;
with Input_Dispatcher;
package Mcd_Mng is

  type Item_Kind_List is (Inte, Real, Bool, Chrs, Prog, Regi, Oper);

  type Operator_List is (

   Add,
   Sub,
   Mult,
   Div,
   Remind,
   Pow,
   Sqrt,
   Minus,
   Absv,

   Bitand,
   Bitor,
   Bitxor,
   Bitneg,
   Shl,
   Shr,

   Equal,
   Diff,
   Greater,
   Smaller,
   Greateq,
   Smalleq,

   Boland,
   Bolor,
   Bolxor,
   Bolneg,

   Pi,
   Sin,
   Cos,
   Tan,
   Asin,
   Acos,
   Atan,

   Exp,
   Ln,
   Log,

   Toreal,
   Round,
   Trunc,
   Int,
   Frac,
   Dms,
   Msd,

   Isreal,
   Isinte,
   Isbool,
   Isstr,
   Isreg,
   Isprog,

   Ssize,
   Swap,
   Swap3,
   Dup,
   Pop,
   Popn,

   Popr,
   Copyr,
   Pushr,
   Swapr,
   Swap2r,
   Clearr,
   Clearall,
   Emptyr,
   Nextr,
   Prevr,
   Regind,
   Indreg,
   Storer,
   Loadr,

   Pope,
   Copye,
   Pushle,
   Pushfe,
   Rotle,
   Rotfe,
   Esize,
   Cleare,

   Ifthen,
   Ifte,
   Etfi,

   Call,
   Ifcall,
   Ret,
   Retn,
   Retall,
   Ifret,
   Ifretn,
   Ifretall,
   Retacal,

   Format,
   Put,
   Newl,
   Putl,

   Strlen,
   Strcat,
   Strsub,
   Strloc,
   Strrep,
   Strupp,
   Strlow,
   Strreal,
   Strinte,
   Strbool,
   Strregi,
   Strprog,
   Strof,

   Clock,
   Dateof,
   Daysof,
   Timeof,

   Obase,
   Nop,
   Rnd,
   Sleep,
   Help
   );


  subtype Chars_Text is String (1 .. Input_Dispatcher.Max_String_Lg);
  subtype Register_List is Character;

  type Item_Rec (Kind : Item_Kind_List := Item_Kind_List'First) is record
    case Kind is
      when Inte =>
        Val_Inte : My_Math.Inte;
      when Real =>
        Val_Real : My_Math.Real;
      when Bool =>
        Val_Bool : Boolean;
      when Chrs | Prog =>
        Val_Len  : Natural;
        Val_Text : Chars_Text;
      when Regi => 
        Val_Regi : Register_List;
      when Oper =>
        Val_Oper : Operator_List;
    end case;
  end record;


  -- Treat a new item
  procedure New_Item (Item : in Item_Rec; The_End : out Boolean);

  -- Check stack is empty
  function Check_Empty_Stack return Boolean;

  -- Is a character a register (in A..Z or a..z)
  function Is_Register (C : in Character) return Boolean;

  -- Close storage file
  procedure Close;

  Invalid_Argument, Argument_Mismatch, Invalid_Register, Emtpy_Register,
  Empty_Stack, String_Len, Compute_Error, File_Error: exception;

end Mcd_Mng;

