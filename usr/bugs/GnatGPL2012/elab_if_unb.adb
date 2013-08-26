with Ada.Text_Io, Ada.Strings.Unbounded;
procedure Elab_If_Unb is

  -- Mandatory for the bug: a condition expression executed during elaboration
  --  and resulting in an Unbounded_String
  function Init (B : Boolean) return Ada.Strings.Unbounded.Unbounded_String is
  begin
    return (if B then Ada.Strings.Unbounded.To_Unbounded_String ("True")
            else Ada.Strings.Unbounded.To_Unbounded_String ("False"));
  end Init;

  Val : constant Ada.Strings.Unbounded.Unbounded_String := Init (True);

begin
  Ada.Text_Io.Put_Line (">" &  Ada.Strings.Unbounded.To_String (Val) & "<");
end Elab_If_Unb;

