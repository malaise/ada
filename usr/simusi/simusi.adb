with My_Io;
with Common, Resolution, Display;
procedure Simusi is

begin
  Display.Print_Tittle(Common.Manufa);
  Resolution.Solve (Common.Manufa);

  Display.Print_Tittle(Common.Design);
  Resolution.Solve (Common.Design);

  Display.Print_Result;

exception
  when Resolution.Abort_Error =>
    Display.Print_Result;
end Simusi;

