-- Interface to forker process
with System;
package Forker is

  -- Command number
  subtype Command_Number is Natural;
  type Boolean_For_C is new Boolean;
  for Boolean_For_C'Size use System.Storage_Unit;

  -------------
  -- REQUEST --
  -------------
  -- Request kind
  type Request_List is (Start_Request, Kill_Request,
                        Forker_Exit_Request, Ping_Request);

  -- Sizing: Program-arguments and Environ are stored at format
  -- <field>Nul<field>Nul ... <field>NulNul
  -- Program + arguments
  Max_Prog_Len : constant := 512;
  -- Environ
  Max_Env_Len  : constant := 256;
  -- Current dir and redirections
  Max_Dir_Len  : constant := 256;

  -- Start request
  type Start_Request_Rec is record
    Number : Command_Number;
    Command : String (1 .. Max_Prog_Len);
    Environ : String (1 .. Max_Env_Len);
    Current_Dir : String (1 .. Max_Dir_Len);
    Output_Flow : String (1 .. Max_Dir_Len);
    Append_Output : Boolean_For_C;
    Error_Flow  : String (1 .. Max_Dir_Len);
    Append_Error : Boolean_For_C;
    Pad : String (1 .. 2);
  end record;

  Init_Start : constant Start_Request_Rec
             := (Number => 0,
                 Command => (others => Ascii.Nul),
                 Environ => (others => Ascii.Nul),
                 Current_Dir => (others => Ascii.Nul),
                 Output_Flow => (others => Ascii.Nul),
                 Error_Flow  => (others => Ascii.Nul),
                 Append_Output => False,
                 Append_Error  => False,
                 Pad => (others => Ascii.Nul) );

  -- Kill request
  type Kill_Request_Rec is record
    Number : Command_Number;
    Signal : Natural;
  end record;

  -- Forker exit request
  type Forker_Exit_Request_Rec is record
    Exit_Code : Natural;
  end record;

  -- Request
  type Request_Rec (Kind : Request_List := Start_Request) is record
    case Kind is
      when Start_Request =>
        Start_Data : Start_Request_Rec;
      when Kill_Request =>
        Kill_Data : Kill_Request_Rec;
      when Forker_Exit_Request =>
        Forker_Exit_Data : Forker_Exit_Request_Rec;
      when Ping_Request =>
        null;
    end case;
  end record;


  ------------
  -- REPORT --
  ------------
  -- Report kind
  type Report_List is (Start_Report, Kill_Report, Exit_Report,
                       Forker_Exit_Report, Pong_Report);
  -- Started/killed pid or -1 if error
  subtype Pid_Result is Integer range -1 .. Integer'Last;

  -- Start report
  type Start_Report_Rec is record
    Number : Command_Number;
    Started_Pid : Pid_Result;
  end record;

  -- Kill report
  type Kill_Report_Rec is record
    Number : Command_Number;
    Killed_Pid : Pid_Result;
  end record;

  -- Exit report
  type Exit_Report_Rec is record
    Number : Command_Number;
    Exit_Pid : Pid_Result;
    Status : Integer;
  end record;

  -- Report
  type Report_Rec (Kind : Report_List := Start_Report) is record
    case Kind is
      when Start_Report =>
        Start_Result : Start_Report_Rec;
      when Kill_Report =>
        Kill_Result : Kill_Report_Rec;
      when Exit_Report =>
        Exit_Result : Exit_Report_Rec;
      when Forker_Exit_Report =>
        null;
      when Pong_Report =>
        null;
    end case;
  end record;

  -- Size to provide for reading report
  subtype Size_Report_Rec is Report_Rec(Exit_Report);
  Report_Size : constant Integer := Size_Report_Rec'Size;

  -- Exit report status decoding
  type Exit_Cause_List is (Normal, Signal, Stop);
  procedure Decode_Exit (Status : in Integer;
                         Cause  : out Exit_Cause_List;
                         Code   : out Natural);

end Forker;

