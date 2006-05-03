-- Common definitions
with Text_Line;
package Common is

  -- Line feed string
  Line_Feed : constant String := Text_Line.Line_Feed  & "";

  -- Syntax error detected by any parser
  Syntax_Error : exception;

end Common;

