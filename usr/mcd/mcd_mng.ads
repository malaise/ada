with As.U, My_Math, Arbitrary.Fractions;
package Mcd_Mng is

  type Item_Kind_List is (Arbi, Frac, Inte, Real, Bool, Chrs, Prog, Regi, Oper);

  type Operator_List is (

   -- Basic operations on numbers
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
   -- Bits operations
   Bitand,
   Bitor,
   Bitxor,
   Bitneg,
   Shl,
   Shr,
   -- Comparisons
   Equal,
   Diff,
   Greater,
   Smaller,
   Greateq,
   Smalleq,
   -- Boolean operations
   Boland,
   Bolor,
   Bolxor,
   Bolneg,
   -- Trigonometry
   Pi,
   Sin,
   Cos,
   Tan,
   Asin,
   Acos,
   Atan,
   -- Logarithm
   Epsilon,
   Exp,
   Ln,
   Log,
   -- Numerical conversion
   Toreal,
   Tointe,
   Toarbi,
   Round,
   Trunc,
   Int,
   Frac,
   Maxint,
   Minint,
   Roundif,
   Dms,
   Msd,
   Mkfrac,
   Numerof,
   Denomof,
   Proport,
   Roundat,
   -- Tests on type and value
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
   -- Main stack management
   Ssize,
   Swap,
   Swap3,
   Dup,
   Prevtop,
   Pop,
   Popn,
   -- Registers and arrays
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
   Popa,
   Pusha,
   Cleara,
   Emptya,
   -- Extra stack
   Pope,
   Copye,
   Pushle,
   Pushfe,
   Rotle,
   Rotfe,
   Esize,
   Cleare,
   -- Conditions
   Ifthen,
   Ifte,
   Etfi,
   -- Subprograms
   Call,
   Ifcall,
   Ret,
   Retn,
   Retall,
   Ifret,
   Ifretn,
   Ifretall,
   Retacal,
   Callbrk,
   -- Output
   Format,
   Obase,
   Put,
   Newl,
   Putl,
   -- String management and conversions
   Strnull,
   Strlen,
   Strcat,
   Strsub,
   Strloc,
   Strrep,
   Strins,
   Strovw,
   Strdel,
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
   -- Time
   Clock,
   Dateof,
   Daysof,
   Timeof,
   -- Miscelaneous
   Nop,
   Getenv,
   Readfile,
   Readlins,
   Rnd,
   Sleep,
   Version,
   Setexit,
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
        Val_Text : As.U.Asu_Us;
      when Regi =>
        Val_Regi : Register_List;
      when Oper =>
        Val_Oper : Operator_List;
    end case;
  end record;

  -- Init the manager
  procedure Init;

  -- Treat a new item
  type End_Status_List is (Exit_Return, Continue, Exit_Break);
  procedure New_Item (Item : in Item_Rec; The_End : out End_Status_List);

  -- Check stack is empty
  function Check_Empty_Stack return Boolean;

  -- Dump stack if debug history
  procedure Dump_Stack;

  -- Is a character a register (in A..Z or a..z)
  function Is_Register (C : in Character) return Boolean;

  Invalid_Argument, Argument_Mismatch, Invalid_Register, Empty_Register,
  Empty_Stack, String_Len, Compute_Error, File_Error: exception;

end Mcd_Mng;

