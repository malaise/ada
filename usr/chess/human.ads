with Tcp_Util;
with Space, Players;
package Human is

  type Play_Mode is (Server, Client, Both);

  -- When server: Color, Name is file_name or empty
  --                     Set_Up is setup file name or empty
  -- When client: Color, Name is server name
  --                     Set_Up is empty
  -- When both:   Name is file_name or empty
  --              Set_Up is setup file name or empty
  -- When there is a file name, wait make user on server
  --  validate each loaded movement

  procedure Play (Mode  : in Play_Mode;
                  Color : in Space.Color_List;
                  Name  : in String;
                  Port  : in Tcp_Util.Remote_Port;
                  Setup : in String;
                  Wait  : in Boolean);
  Load_Error : exception;

end Human;

