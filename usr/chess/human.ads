with Space, Players;
package Human is

  type Play_Mode is (Server, Client, Both);

  -- When server: Color, and Name is file_name or empty
  -- When client: Color, and Name is server name
  -- when Both:   Name is file_name or empty

  procedure Play (Mode  : in Play_Mode;
                  Color : in Space.Color_List;
                  Name  : in string);
  Load_Error : exception;

end Human;

