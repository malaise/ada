with Ada.Text_Io, Int_Io, Flo_Io, Long_Io, Bool_Io;
package My_Io is 

  Default_Exp : Ada.Text_Io.Field := 4; 

  procedure Put(Item  : in Boolean; 
                Width : in Ada.Text_Io.Field := Bool_Io.Default_Width; 
                Set   : in Ada.Text_Io.Type_Set := Bool_Io.Default_Setting)
    renames Bool_Io.Put;

  procedure Put(Item : in Character) renames Ada.Text_Io.Put;

  procedure Put(Item  : in Integer;
                Width : in Ada.Text_Io.Field := Int_Io.Default_Width;
                Base  : in Ada.Text_Io.Number_Base := Int_Io.Default_Base)
    renames Int_Io.Put;

  procedure Put(Item  : in Long_Long_Integer;
                Width : in Ada.Text_Io.Field := Long_Io.Default_Width;
                Base  : in Ada.Text_Io.Number_Base := Long_Io.Default_Base)
    renames Long_Io.Put;

  procedure Put(Item : in Float;
                Fore : in Ada.Text_Io.Field := Flo_Io.Default_Fore;
                Aft  : in Ada.Text_Io.Field := Flo_Io.Default_Aft;
                Exp  : in Ada.Text_Io.Field := Default_Exp) renames Flo_Io.Put;

  procedure Put(Item : in String) renames Ada.Text_Io.Put;

  procedure Put_Line(Item  : in Boolean;
                     Width : in Ada.Text_Io.Field := Bool_Io.Default_Width;
                     Set   : in Ada.Text_Io.Type_Set 
                           := Bool_Io.Default_Setting);

  procedure Put_Line(Item : in Character);

  procedure Put_Line(Item  : in Integer;
                     Width : in Ada.Text_Io.Field := Int_Io.Default_Width;
                     Base  : in Ada.Text_Io.Number_Base := Int_Io.Default_Base);

  procedure Put_Line(Item  : in Long_Long_Integer;
                     Width : in Ada.Text_Io.Field := Long_Io.Default_Width;
                     Base  : in Ada.Text_Io.Number_Base
                           := Long_Io.Default_Base);

  procedure Put_Line(Item : in Float;
                     Fore : in Ada.Text_Io.Field := Flo_Io.Default_Fore;
                     Aft  : in Ada.Text_Io.Field := Flo_Io.Default_Aft;
                     Exp  : in Ada.Text_Io.Field := Default_Exp);

  procedure Put_Line(Item : in String) renames Ada.Text_Io.Put_Line;

  procedure New_Line(Spacing : in Ada.Text_Io.Positive_Count := 1)
    renames Ada.Text_Io.New_Line;

  procedure Get(Item : out Boolean) renames Bool_Io.Get;

  procedure Get(Item : out Character) renames Ada.Text_Io.Get;

  procedure Get(Item  : out Integer;
                Width : in Ada.Text_Io.Field := 0) renames Int_Io.Get;

  procedure Get(Item  : out Long_Long_Integer;
                Width : in Ada.Text_Io.Field := 0) renames Long_Io.Get;

  procedure Get(Item  : out Float;
                Width : in Ada.Text_Io.Field := 0) renames Flo_Io.Get;

  procedure Get(Item : out String) renames Ada.Text_Io.Get;

  procedure Get_Line(Item : out String;
                     Last : out Natural) renames Ada.Text_Io.Get_Line;

  -- Display prompt, then get item until no error
  procedure Safe_Get(Prompt : in String;
                     Item   : out Integer);

  procedure Safe_Get(Prompt : in String;
                     Item   : out Long_Long_Integer);

  procedure Safe_Get(Prompt : in String;
                     Item   : out Float);

  procedure Skip_Line(Spacing : in Ada.Text_Io.Positive_Count := 1)
    renames Ada.Text_Io.Skip_Line;

  function End_Of_Line return Boolean renames Ada.Text_Io.End_Of_Line;

  procedure Flush renames Ada.Text_Io.Flush;

end My_Io;

