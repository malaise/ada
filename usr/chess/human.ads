with Space, Players;
package Human is

  procedure Play (Server_Name : in String;
                  File_Name : in string;
                  Color  : in Space.Color_List);
  Load_Error : exception;

end Human;

