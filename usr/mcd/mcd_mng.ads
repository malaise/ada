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

   Toreal,
   Round,
   Trunc,
   Int,
   Frac,

   Isreal,
   Isinte,
   Isstr,
   Isreg,

   Ssize,
   Swap,
   Swap3,
   Dup,
   Pop,
   Popn,

   Popr,
   Copyr,
   Pushr,
   Clearreg,

   Pope,
   Copye,
   Pushle,
   Pushfe,
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
   Strof,

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

  function Check_Empty_Stack return Boolean;

  Invalid_Argument, Argument_Mismatch, Invalid_Register, Emtpy_Register,
  Empty_Stack, String_Len : exception;

end Mcd_Mng;

