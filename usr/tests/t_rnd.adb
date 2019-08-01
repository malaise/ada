with Argument, Rnd, Basic_Proc, Images, Mixed_Str;
procedure T_Rnd is

  -- t_rnd [ <kind> ] [ <max> ]

  type Color is (Blue, Red, Yellow, Purple, Green, Orange, White, Black);

  function My_Random is new Rnd.Discr_Random (Color);

  Arr : array (Color) of Natural := (others => 0);

  Kind : Rnd.Kind_List;
  Try : Color;
  Start : Positive := 1;

  Tot : Natural := 0;
  G : access Rnd.Generator;

  procedure Help is
    use type Rnd.Kind_List;
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
      & " [ <kind> ] [ <max_val> ]");
    Basic_Proc.Put_Output ("  <kind> ::= ");
    for K in Rnd.Kind_List loop
      Basic_Proc.Put_Output (Mixed_Str (K'Img)
        & (if K /= Rnd.Kind_List'Last then " | " else ""));
    end loop;
    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Random 0 <= I < max_val, or random colors");
    Basic_Proc.Set_Error_Exit_Code;
  end Help;

begin
  -- Help
  if Argument.Get_Nbre_Arg = 1 and then
    (Argument.Get_Parameter = "-h" or else Argument.Get_Parameter = "--help")
  then
    Help;
    return;
  end if;

  if Argument.Get_Nbre_Arg >= 1 then
    -- Try to get optional kind
    begin
      Kind := Rnd.Kind_List'Value (Argument.Get_Parameter(Occurence => 1));
      Start := 2;
    exception
      when others => null;
     end;
  end if;
  if Start = 1 then
    Kind := Rnd.Universal;
  end if;
  G := new  Rnd.Generator (Kind);
  G.Randomize;

  if Argument.Get_Nbre_Arg = Start then
    -- One arg => max_val
    Basic_Proc.Put_Line_Output (Images.Integer_Image (
        G.Int_Random (0, Integer'Value (Argument.Get_Parameter
                                          (Occurence => Start)))));
    return;
  elsif Argument.Get_Nbre_Arg = Start - 1 then
    -- No arg => play with colors: random except first and last of enum
    for I in 1 .. 1_000 loop
      Try := My_Random (G.all, Color'Succ(Color'First), Color'Pred(Color'Last));
      Arr(Try) := Arr(Try) + 1;
    end loop;
    for I in Color loop
      Basic_Proc.Put_Output (Color'Image (I));
      Basic_Proc.Put_Output (" -> ");
      Basic_Proc.Put_Output (Arr(I)'Img);
      Tot := Tot + Arr (I);
      Basic_Proc.New_Line_Output;
    end loop;
    Basic_Proc.Put_Output ("Total:" );
    Basic_Proc.Put_Output (Tot'Img);
    Basic_Proc.New_Line_Output;
  else
    raise Constraint_Error;
  end if;

exception
  when others =>
    Help;
end T_Rnd;

