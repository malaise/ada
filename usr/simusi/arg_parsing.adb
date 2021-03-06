with Argument;
package body Arg_Parsing is

  Verbose_Flag : Boolean;
  Pm, Pd : Positive;

  function Manufa_File_Name return String is (Argument.Get_Parameter (Pm));

  function Design_File_Name return String is (Argument.Get_Parameter (Pd));

  function Verbose return Boolean is (Verbose_Flag);

  procedure Check is
  begin
    Verbose_Flag := False;
    Pm := 1;
    Pd := 2;
    if Argument.Get_Nbre_Arg = 3 then
      if Argument.Get_Parameter (1, "v") = ""
      and then Argument.Get_Position (1, "v") = 1 then
        Verbose_Flag := True;
        Pm := 2;
        Pd := 3;
      else
        raise Arg_Error;
      end if;
    elsif Argument.Get_Nbre_Arg /= 2 then
      raise Arg_Error;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      -- Default: no flag
      null;
  end Check;

end Arg_Parsing;

