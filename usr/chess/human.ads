with Space, Players;
package Human is

  type Play_Mode is (Server, Client, Both);

  -- When server: Color, and Name is file_name or empty
  -- When client: Color, and Name is server name
  -- When both:   Name is file_name or empty
  -- When there is a file name, wait make user on server
  --  validate each loaded movement

  procedure Play (Mode  : in Play_Mode;
                  Color : in Space.Color_List;
                  Name  : in string;
                  Wait  : in Boolean);
  Load_Error : exception;

end Human;

