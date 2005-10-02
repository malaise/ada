with Definition;
package body Coder is

  -- Init enigma from arguments and config files
  procedure Init is
    Def : Definition.Def_Rec;
  begin
    Definition.Read_Definition (Def);
  exception
    when Definition.Invalid_Definition =>
      null;
  end Init;

  -- Encode a letter
  function Encode (X : Types.Letter) return Types.Letter is
  begin
    return X;
  end Encode;

end Coder;

