with My_Math;
with Input_Dispatcher;
package Mcd_Mng is

  type Item_Kind_List is (Inte, Real, Bool, Chrs, Regi, Oper);

  type Operator_List is (
   Nop,

   Add,
   Sub,
   Mult,
   Div,
   Remind,
   Pow,
   Swap,

   Bitand,
   Bitor,
   Bitxor,

   Shl,
   Shr,
   
   Minus,
   Absv,
   Bitneg,

   Equal,
   Diff,
   Greater,
   Smaller,
   Greateq,
   Smalleq,

   Toreal,
   Round,
   Trunc,

   Int,
   Frac,

   Isreal,
   Isinte,
   Isstr,
   Isreg,

   Obase,

   Boland,
   Bolor,
   Bolxor,

   Bolneg,

   Dup,

   Pop,
   Popn,

   Ifthen,
   Ifte,
   Etfi,
  
   Popr,
   Copyr,
   Pushr,

   Ssize,
   Rnd,
   Sleep,

   Pope,
   Copye,
   Pushle,
   Pushfe,
   Esize,

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
      when Chrs =>
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

