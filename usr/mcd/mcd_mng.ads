with Ada.Strings.Unbounded;
with My_Math, Arbitrary, Arbitrary.Fractions;
with Input_Dispatcher;
package Mcd_Mng is

  type Item_Kind_List is (Arbi, Frac, Inte, Real, Bool, Chrs, Prog, Regi, Oper);

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
   Fact,

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

   Epsilon,
   Exp,
   Ln,
   Log,

   Toreal,
   Tointe,
   Toarbi,
   Round,
   Trunc,
   Int,
   Frac,
   Maxint,
   Minint,
   Dms,
   Mkfrac,
   Numerof,
   Denomof,
   Msd,
   Proport,
   Roundat,

   Isarbi,
   Isfrac,
   Isinte,
   Isreal,
   Isbool,
   Isstr,
   Isreg,
   Isprog,
   Ispos,
   Isnul,
   Isnotnul,
   Isneg,

   Ssize,
   Swap,
   Swap3,
   Dup,
   Prevtop,
   Pop,
   Popn,

   Popr,
   Copyr,
   Pushr,
   Swapr,
   Swap2R,
   Clearr,
   Clearall,
   Emptyr,
   Nextr,
   Prevr,
   Regind,
   Indreg,

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
   Strmix,
   Strarbi,
   Strfrac,
   Strinte,
   Strreal,
   Strbool,
   Strregi,
   Strprog,
   Strof,
   Normal,
   Regmatch,

   Clock,
   Dateof,
   Daysof,
   Timeof,

   Obase,
   Nop,
   Getenv,
   Rnd,
   Sleep,
   Debugall,
   Help
   );


  subtype Register_List is Character;

  type Item_Rec (Kind : Item_Kind_List := Item_Kind_List'First) is record
    case Kind is
      when Arbi =>
        Val_Arbi : Arbitrary.Number;
      when Frac =>
        Val_Frac : Arbitrary.Fractions.Fraction;
      when Inte =>
        Val_Inte : My_Math.Inte;
      when Real =>
        Val_Real : My_Math.Real;
      when Bool =>
        Val_Bool : Boolean;
      when Chrs | Prog =>
        Val_Text : Ada.Strings.Unbounded.Unbounded_String;
      when Regi =>
        Val_Regi : Register_List;
      when Oper =>
        Val_Oper : Operator_List;
    end case;
  end record;


  -- Treat a new item
  type End_Status_List is (Exit_Return, Continue, Exit_Break);
  procedure New_Item (Item : in Item_Rec; The_End : out End_Status_List);

  -- Check stack is empty
  function Check_Empty_Stack return Boolean;

  -- Is a character a register (in A..Z or a..z)
  function Is_Register (C : in Character) return Boolean;

  Invalid_Argument, Argument_Mismatch, Invalid_Register, Emtpy_Register,
  Empty_Stack, String_Len, Compute_Error, File_Error: exception;

end Mcd_Mng;

