with Assertion, Event_Mng, String_Mng, Text_Handler, Normal, Event_Mng,
     Sys_Calls;
with Dictio_Lib;
package body Fifos is

  -------------------------------------------------------------------------
  -- COMMON
  -------------------------------------------------------------------------
  Local_Host_Name : constant String := Socket.Local_Host_Name;
  Local_Host_Id : constant Tcp_Util.Host_Id := Socket.Local_Host_Id;

  -- Remove trailing spaces
  function Parse (Str : String) return String is
  begin
    return Str (1 .. String_Mng.Parse_Spaces (Str, False));
  end Parse;

  procedure Host_Name2Id (Host : in out Tcp_Util.Remote_Host) is
    Id : Tcp_Util.Host_Id;
  begin
    Id := Socket.Host_Id_Of (Parse (Host.Name));
    Host := (Kind => Tcp_Util.Host_Id_Spec, Id => Id);
  exception
    when Socket.Soc_Name_Not_Found =>
      raise Name_Error;
  end Host_Name2Id;
    

  package body Fifo is

    -------------------------------------------------------------------------
    -- FIFO SEND
    -------------------------------------------------------------------------
    function Fifo_Send is new Tcp_Util.Send (Message_Type);

    -------------------------------------------------------------------------
    -- FIFO LIST
    -------------------------------------------------------------------------

    package List is
      -- Insert a fifo
      function Insert (Rec : Fifo_Rec) return Fifo_Access;


      -- Search fifo
      function Search_By_Name (Kind : Fifo_Kind_List;
                               Name : String) return Boolean;
      function Search_By_Port (Kind : Fifo_Kind_List;
                               Port : Tcp_Util.Port_Num) return Boolean;
      function Search_By_Dscr (Dscr : Socket.Socket_Dscr) return Boolean;

      function Search_By_Addr (Kind : Fifo_Kind_List;
                               Host : Tcp_Util.Host_Id;
                               Port : Tcp_Util.Port_Num) return Boolean;

      -- Get access to current fifo
      function Access_Current return Fifo_Access;

      -- Del current fifo and move to next
      procedure Del_Current;

      -- Move to first fifo in list
      procedure Rewind;

      -- Is list empty?
      function Empty return Boolean;

    end List;

    package body List is

      -- The list 
      Fifo_List : Fifo_List_Mng.List_Type;

      -- Insert a record
      function Insert (Rec : Fifo_Rec) return Fifo_Access is
      begin
        Fifo_List_Mng.Insert (Fifo_List, Rec);
        return Access_Current;
      end Insert;

      -- Get access to current record
      function Access_Current return Fifo_Access is
      begin
        return Fifo_List_Mng.Access_Current (Fifo_List);
      end Access_Current;

      -- Delete current record and move to next
      procedure Del_Current is
      begin
        if Fifo_List_Mng.Get_Position (Fifo_List) /=
           Fifo_List_Mng.List_Length (Fifo_List) then
          Fifo_List_Mng.Delete (Fifo_List, Fifo_List_Mng.Next);
        else
          Fifo_List_Mng.Delete (Fifo_List, Fifo_List_Mng.Prev);
        end if;
      end Del_Current;

      -- Move to first fifo in list
      procedure Rewind is
      begin
        if not Fifo_List_Mng.Is_Empty (Fifo_List) then
          Fifo_List_Mng.Move_To (Fifo_List, Fifo_List_Mng.Next, 0, False);
        end if;
      end Rewind;

      -- Is list empty?
      function Empty return Boolean is
      begin
        return Fifo_List_Mng.Is_Empty (Fifo_List);
      end Empty;

      -- Search by name
      function Same_Name (El1, El2 : Fifo_Rec) return Boolean is
      begin
        return   El1.Kind = El2.Kind
        and then El1.Len = El2.Len
        and then El1.Name (1 .. El1.Len) = El2.Name (1 .. El2.Len);
      end Same_Name;
      procedure Search_Name is new Fifo_List_Mng.Search (Same_Name);

      function Search_By_Name (Kind : Fifo_Kind_List;
                               Name : String) return Boolean is
        Rec : Fifo_Rec;
      begin
        Rec.Kind := Kind;
        Rec.Len := Name'Length;
        Rec.Name (1 .. Rec.Len) := Name;
        begin
          Search_Name (Fifo_List, Rec, From_Current => False);
        exception
          when Fifo_List_Mng.Not_In_List =>
            return False;
        end;
        return True;
      end Search_By_Name;

      -- Search by port num
      function Same_Port (El1, El2 : Fifo_Rec) return Boolean is
        use type Tcp_Util.Remote_Port;
      begin
        return   El1.Kind = El2.Kind
        and then El1.Port = El2.Port;
      end Same_Port;
      procedure Search_Port is new Fifo_List_Mng.Search (Same_Port);

      function Search_By_Port (Kind : Fifo_Kind_List;
                               Port : Tcp_Util.Port_Num) return Boolean is
        Rec : Fifo_Rec;
      begin
        Rec.Kind := Kind;
        Rec.Port := (Kind => Tcp_Util.Port_Num_Spec, Num => Port);

        begin
          Search_Port (Fifo_List, Rec, From_Current => False);
        exception
          when Fifo_List_Mng.Not_In_List =>
            return False;
        end;
        return True;
      end Search_By_Port;

      -- Search by socket dscr
      function Same_Dscr (El1, El2 : Fifo_Rec) return Boolean is
        use type Socket.Socket_Dscr;
      begin
        return El1.Dscr = El2.Dscr;
      end Same_Dscr;
      procedure Search_Dscr is new Fifo_List_Mng.Search (Same_Dscr);

      function Search_By_Dscr (Dscr : Socket.Socket_Dscr) return Boolean is
        Rec : Fifo_Rec;
      begin
        Rec.Dscr := Dscr;
        begin
          Search_Dscr (Fifo_List, Rec, From_Current => False);
        exception
          when Fifo_List_Mng.Not_In_List =>
            return False;
        end;
        return True;
      end Search_By_Dscr;

      -- Search by saddr
      function Same_Addr (El1, El2 : Fifo_Rec) return Boolean is
        use type Tcp_Util.Remote_Host, Tcp_Util.Remote_Port;
      begin
        return   El1.Kind = El2.Kind
        and then El1.Port = El2.Port
        and then El1.Host = El2.Host;
      end Same_Addr;
      procedure Search_Addr is new Fifo_List_Mng.Search (Same_Addr);

      function Search_By_Addr (Kind : Fifo_Kind_List;
                               Host : Tcp_Util.Host_Id;
                               Port : Tcp_Util.Port_Num) return Boolean is
        Rec : Fifo_Rec;
      begin
        Rec.Kind := Kind;
        Rec.Host := (Kind => Tcp_Util.Host_Id_Spec, Id => Host);
        Rec.Port := (Kind => Tcp_Util.Port_Num_Spec, Num => Port);

        begin
          Search_Addr (Fifo_List, Rec, From_Current => False);
        exception
          when Fifo_List_Mng.Not_In_List =>
            return False;
        end;
        return True;
      end Search_By_Addr;
    end List;

    -------------------------------------------------------------------------
    -- CONNECTION
    -------------------------------------------------------------------------
    package Connection is
      procedure Accepte (Fifo : in Fifo_Access);
      procedure Connect (Fifo : in Fifo_Access);
      procedure Close (Fifo : in Fifo_Access);
    end Connection;

    package body Connection is

      package Fifo_Reception is new Tcp_Util.Reception (Message_Type);

      procedure Close_Socket (Dscr : in out Socket.Socket_Dscr) is
      begin
        Tcp_Util.Abort_Send_And_Close (Dscr);
      exception
        when Tcp_Util.No_Such =>
          Socket.Close (Dscr);
      end Close_Socket;

      -------------------------------------------------------------------------
      -- CALLBACKS
      -------------------------------------------------------------------------
      procedure Disconnection_Cb (Dscr : in Socket.Socket_Dscr) is
        Acc : Fifo_Access;
      begin
        -- Clean fifo
        if not List.Search_By_Dscr (Dscr) then
          Assertion.Assert (False, "disconnection of unknown fifo");
          return;
        end if;
        Acc := List.Access_Current;

        -- Call Connection_Cb
        if Acc.Kind /= Accepting and then Acc.Conn_Cb /= null then
          Acc.Conn_Cb (Acc.Name(1 .. Acc.Len), (Acc => Acc), False);
        end if;

        if Acc.Kind = Connect then
          -- Clean connection info
          Acc.Dscr := Socket.No_Socket;
          Acc.State := Waiting; 
        elsif Acc.Kind = Accepted then
          List.Del_Current;
        else
          Assertion.Assert (False, "disconnection of accepting fifo");
          return;
        end if;

      end Disconnection_Cb;


      procedure Reception_Cb (Dscr    : in Socket.Socket_Dscr;
                              Message : in Message_Type;
                              Length  : in Natural) is
        Acc : Fifo_Access;
      begin
        if not List.Search_By_Dscr (Dscr) then
          Assertion.Assert (False, "reception on unknown fifo");
          return;
        end if;

        Acc := List.Access_Current;
        if Acc.Kind = Accepting then
          Assertion.Assert (False, "reception on accepting fifo");
          return;
        end if;

        if Acc.Kind /= Accepting and then Acc.Rece_Cb /= null then
          Acc.Rece_Cb ((Acc => Acc), Message, Length);
        end if;

      end Reception_Cb;


      procedure Acception_Cb (Local_Port_Num  : in Tcp_Util.Port_Num;
                              Local_Dscr      : in Socket.Socket_Dscr;
                              Remote_Port_Num : in Tcp_Util.Port_Num;
                              Remote_Host_Id  : in Tcp_Util.Host_Id;
                              New_Dscr        : in Socket.Socket_Dscr) is
        Tmp_Dscr : Socket.Socket_Dscr;
        Rec : Fifo_Rec;
        Acc : Fifo_Access;
      begin
        -- Get accepting rec
        if not List.Search_By_Port (Accepting, Local_Port_Num) then
          Tmp_Dscr := New_Dscr;
          Close_Socket (Tmp_Dscr);
          Assertion.Assert (False, "acception on unknown fifo");
          return;
        end if;

        Rec := List.Access_Current.all;
        if Rec.Kind /= Accepting then
          Assertion.Assert (False, "acception on " & Rec.Kind'Img & " fifo");
          return;
        end if;
          
        -- Insert new record
        Rec.Kind := Accepted;
        Rec.Dscr := New_Dscr;
        Rec.Host := (Kind => Tcp_Util.Host_Id_Spec, Id => Remote_Host_Id);
        Rec.Port := (Kind => Tcp_Util.Port_Num_Spec, Num => Remote_Port_Num);
        Rec.State := Connected;
        Acc := List.Insert (Rec);

        -- Hook reception/disconnection callbacks
        Fifo_Reception.Set_Callbacks (New_Dscr,
                   Reception_Cb'Unrestricted_Access,
                   Disconnection_Cb'Unrestricted_Access);

        -- Call Connection_Cb
        if Rec.Conn_Cb /= null then
          Rec.Conn_Cb (Rec.Name(1 .. Rec.Len), (Acc => Acc), True);
        end if;

      end Acception_Cb;

      procedure Connection_Cb (Remote_Port_Num : in Tcp_Util.Port_Num;
                               Remote_Host_Id  : in Tcp_Util.Host_Id;
                               Connected       : in Boolean;
                               Dscr            : in Socket.Socket_Dscr) is
        Acc : Fifo_Access;
      begin
        if not List.Search_By_Addr (Connect, Remote_Host_Id, Remote_Port_Num) then
          Assertion.Assert (False, "connection of unknown fifo");
          return;
        end if;

        Acc := List.Access_Current;
        if Acc.Kind /= Connect then
          Assertion.Assert (False, "connection of " & Acc.Kind'Img & " fifo");
          return;
        end if;

        if Acc.State /= Connecting then
          Assertion.Assert (False, " connection of "
                          & Acc.State'Img & " fifo");
          return;
        end if;

        if Connected then
          -- Update fifo
          Acc.Dscr := Dscr;
          Acc.State := Fifos.Connected;

          -- Hook reception/disconnection callbacks
          Fifo_Reception.Set_Callbacks (Dscr,
                     Reception_Cb'Unrestricted_Access,
                     Disconnection_Cb'Unrestricted_Access);

          -- Call Connection_Cb
          if Acc.Conn_Cb /= null then
            Acc.Conn_Cb (Acc.Name(1 .. Acc.Len), (Acc => Acc), True);
          end if;
        else
          -- Update fifo
          Acc.State := Waiting;
        end if;

      end Connection_Cb;


      -------------------------------------------------------------------------
      -- FUNCTIONS
      -------------------------------------------------------------------------

      procedure Connect (Fifo : in Fifo_Access) is
        Result : Boolean;
      begin
        if Fifo.Kind /= Connect then
          Assertion.Assert (False, "connecting a fifo of kind "
                          & Fifo.Kind'Img);
          return;
        end if;
        Result := Tcp_Util.Connect_To (
                       Socket.Tcp_Header, 
                       Fifo.Host, Fifo.Port,
                       Delta_Retry => 3.0,
                       Nb_Tries    => 3,
                       Connection_Cb => Connection_Cb'Unrestricted_Access);
      exception
        when Tcp_Util.Name_Error =>
          null;
      end Connect;

      procedure Close (Fifo : in Fifo_Access) is 
        procedure Call_Cb is
        begin
          -- Call Connection_Cb
          if Fifo.Conn_Cb /= null then
            Fifo.Conn_Cb (Fifo.Name(1 .. Fifo.Len), (Acc => Fifo), False);
          end if;
        end Call_Cb;
      begin
        case Fifo.Kind is
          when Connect =>
            case Fifo.State is
              when Waiting =>
                null;
              when Connecting =>
                Tcp_Util.Abort_Connect (Fifo.Host, Fifo.Port);
              when Connected =>
                Fifo_Reception.Remove_Callbacks (Fifo.Dscr);
                Close_Socket (Fifo.Dscr);
                Call_Cb;
            end case;
          when Accepting =>
            Tcp_Util.Abort_Accept (Fifo.Port.Num);
          when Accepted =>
            Fifo_Reception.Remove_Callbacks (Fifo.Dscr);
            Close_Socket (Fifo.Dscr);
            Call_Cb;
        end case;
      end Close;

      procedure Accepte (Fifo : in Fifo_Access) is
        Port_Num : Tcp_Util.Port_Num;
        Port : Tcp_Util.Local_Port;
        use type Tcp_Util.Local_Port_List;
      begin
        -- Check if Fifo_Name is a port name, link dynamic otherwise
        begin
          Port_Num := Socket.Port_Num_Of (Fifo.Name (1 .. Fifo.Len),
                                          Socket.Tcp_Header);
          Port := (Kind => Tcp_Util.Port_Num_Spec,
                   Num  => Port_Num);
        exception
          when Socket.Soc_Name_Not_Found =>
            Port := (Kind => Tcp_Util.Port_Dynamic_Spec);
        end;

        Tcp_Util.Accept_From (Socket.Tcp_Header,
                              Port,
                              Acception_Cb'Unrestricted_Access,
                              Fifo.Dscr,
                              Port_Num);
        Fifo.Host := (Kind => Tcp_Util.Host_Id_Spec, Id => Local_Host_Id); 
        Fifo.Port := (Kind => Tcp_Util.Port_Num_Spec, Num => Port_Num);
        Fifo.State := Connected;
      exception
        when Socket.Soc_Name_Not_Found =>
          raise Name_Error;
      end Accepte;

    end Connection;

    -------------------------------------------------------------------------
    -- DICTIO
    -------------------------------------------------------------------------
    package Dictio is
      procedure Set (Fifo_Name : in String; Host : in String;
                                            Port : in Tcp_Util.Port_Num);
      procedure Get (Fifo_Name : in String;
                     Got  : out Boolean;
                     Host : out Tcp_Util.Host_Name;
                     Port : out Tcp_Util.Port_Num);
      Data_Error : exception;
      procedure Notify (Fifo_Name : in String; On : in Boolean);
    end Dictio;

    package body Dictio is

      Name_Suffix : Text_Handler.Text (Dictio_Lib.Max_Name_Len);
      Default_Name_Suffix : constant String := "Fifo.";
      Name_Suffix_Name : constant String := "FIFO_NAME_SUFFIX";
      Separator : constant Character := ':';

      Init_Done : Boolean := False;
      Dictio_State : Dictio_Lib.Dictio_State_List := Dictio_Lib.Unavailable;

      function Fifo_Name_Of (Item_Name : String) return String is
        Suff_Len : constant Integer := Text_Handler.Length (Name_Suffix);
      begin
        if Item_Name'Length <= Suff_Len
        or else Item_Name(1 .. Suff_Len)
             /= Text_Handler.Value (Name_Suffix) then
          raise Data_Error;
        end if;
        return Item_Name(Suff_Len+1 .. Item_Name'Length);
      end Fifo_Name_Of;


      
      procedure Split (Data : in String; Host : out Tcp_Util.Host_Name;
                                         Port : out Tcp_Util.Port_Num) is
        Txt : Text_Handler.Text (Data'Length);
        Sep_Index : Text_Handler.Max_Len_Range;
      begin
        Text_Handler.Set (Txt, Data);
        Sep_Index := Text_Handler.Locate (Txt, Separator);
        if Sep_Index <= 1 or else Sep_Index = Text_Handler.Length(Txt) then
          raise Data_Error;
        end if;

        Host := (others => ' ');
        begin
          Host (1 .. Sep_Index - 1) :=
            Text_Handler.Value (Txt) (1 .. Sep_Index - 1);
          Port := Tcp_Util.Port_Num'Value( 
            Text_Handler.Value (Txt) (Sep_Index + 1 .. Text_Handler.Length(Txt)) );
        exception
          when others =>
            raise Data_Error;
        end;

      end Split;

      procedure State_Cb (State : in Dictio_Lib.Dictio_State_List) is
      begin
        Dictio_State := State;
      end State_Cb;

      procedure Notify_Cb (Name : in String;
                           Item : in Boolean;
                           Data : in String) is
        Acc : Fifo_Access;
      begin
        -- Cet record and check
        declare
         Fifo_Name : constant String := Fifo_Name_Of (Name);
        begin
          if not List.Search_By_Name (Connect, Fifo_Name) then
            Assertion.Assert (False, "notify on unknown fifo");
            Dictio_Lib.Notify (Name, False, False);
            return;
          end if;
        end;

        Acc := List.Access_Current;
        if Acc.Kind /= Connect then
          Assertion.Assert (False, "notify on " & Acc.Kind'Img & " fifo");
          return;
        end if;

        -- Close connecting/connected
        Connection.Close (Acc);

        -- Try to re-connect on new data
        Acc.Host := (Kind => Tcp_Util.Host_Name_Spec, Name => (others => ' '));
        Split (Data, Acc.Host.Name, Acc.Port.Num);
        begin
          Host_Name2Id (Acc.Host);
        exception
          when Name_Error =>
            Acc.State := Waiting;
            return;
        end;

        Acc.State := Connecting;
        Connection.Connect (Acc);
      exception
        when Data_Error =>
          null;
      end Notify_Cb;

      procedure Init is
        Env_Set   : Boolean;
        Env_Trunc : Boolean;
        Env_Value : String (1 .. Name_Suffix.Max_Len - 1);
        Env_Len   : Natural;

        use type Dictio_Lib.Dictio_State_List;
      begin
        if Init_Done then
          return;
        end if;
        -- Init dictio
        Dictio_Lib.Dictio_State_Cb := State_Cb'Unrestricted_Access;
        Dictio_Lib.Notify_Cb := Notify_Cb'Unrestricted_Access;
        Dictio_Lib.Init;
        if Dictio_State = Dictio_Lib.Unavailable then
          raise No_Dictio;
        end if;

        -- Getenv name suffix
        Sys_Calls.Getenv (Name_Suffix_Name, Env_Set, Env_Trunc,
                          Env_Value, Env_Len);
        -- Check a name based on this suffix is accepted by dictio
        if Env_Set and then not Env_Trunc and then Env_Len /= 0
        and then Dictio_Lib.Is_Valid_Item_Name (
                            Env_Value(1 .. Env_Len) & "a") then
          Text_Handler.Set (Name_Suffix, Env_Value(1 .. Env_Len));
        else
          Text_Handler.Set (Name_Suffix, Default_Name_Suffix);
        end if;

        Init_Done := True;
      exception
        when Dictio_Lib.No_Dictio =>
          raise No_Dictio;
        when No_Dictio =>
          raise;
        when others => 
          raise System_Error;
      end Init;
     

      procedure Set (Fifo_Name : in String; Host : in String;
                                            Port : in Tcp_Util.Port_Num) is
      begin
        Init;
        if Text_Handler.Length (Name_Suffix) + Fifo_Name'Length
         > Dictio_Lib.Max_Name_Len then
          raise Name_Too_Long;
        end if;
        Dictio_Lib.Set (Text_Handler.Value (Name_Suffix)
                      & Fifo_Name, Host & Separator
                      & Normal (Integer(Port), 5, Gap => '0'));
      exception
        when Dictio_Lib.No_Dictio =>
          raise No_Dictio;
        when No_Dictio =>
          raise;
        when Dictio_Lib.Name_Too_Long =>
          raise Name_Too_Long;
        when Name_Too_Long =>
          raise;
        when others =>
          raise System_Error;
      end Set;

      procedure Get (Fifo_Name : in String;
                     Got  : out Boolean;
                     Host : out Tcp_Util.Host_Name;
                     Port : out Tcp_Util.Port_Num) is
      begin
        Init;
        Got := False;
        if Text_Handler.Length (Name_Suffix) + Fifo_Name'Length
         > Dictio_Lib.Max_Name_Len then
          raise Name_Too_Long;
        end if;

        begin
          Split (Dictio_Lib.Get (Text_Handler.Value (Name_Suffix) & Fifo_Name),
                 Host, Port);
        exception
          when Dictio_Lib.No_Dictio =>
            raise No_Dictio;
          when No_Dictio =>
            raise;
          when Dictio_Lib.Name_Too_Long =>
            raise Name_Too_Long;
          when Dictio_Lib.No_Item =>
            return;
          when Data_Error =>
            raise;
          when others =>
            raise System_Error;
        end;
        Got := True;

      end Get;

      procedure Notify (Fifo_Name : in String; On : in Boolean) is
      begin
        Init;
        if Text_Handler.Length (Name_Suffix) + Fifo_Name'Length
         > Dictio_Lib.Max_Name_Len then
          raise Name_Too_Long;
        end if;
        Dictio_Lib.Notify (Text_Handler.Value (Name_Suffix) & Fifo_Name,
                           True, On);
      exception
        when Dictio_Lib.No_Dictio =>
          raise No_Dictio;
        when No_Dictio =>
          raise;
        when Name_Too_Long =>
          raise;
        when others =>
          raise System_Error;
      end Notify;

    end Dictio;

    -------------------------------------------------------------------------
    -- OPEN
    -------------------------------------------------------------------------
    -- May raise Name_Too_Long if Fifo_Name is longer than Max_Fifo_Name_Len
    -- May raise Yet_Open if Fifo_Name is already open (in any way)
    -- May raise System_Error on various error conditions
    function Open (Fifo_Name       : in String;
                   To_Remote       : in Boolean;
                   Connection_Cb   : in Connection_Callback_Access;
                   Reception_Cb    : in Reception_Callback_Access;
                   End_Overflow_Cb : in End_Overflow_Callback_Access) 
             return Fifo_Id is
      Rec : Fifo_Rec;
      Got : Boolean;
      Acc : Fifo_Access;
    begin
      -- Check name length
      if Fifo_Name'Length > Max_Fifo_Name_Len then
        raise Name_Too_Long;
      end if;

      -- Check that this fifo is not already open
      Rec.Len := Fifo_Name'Length;
      Rec.Name (1 .. Rec.Len) := Fifo_Name;
      if To_Remote then
        Rec.Kind := Connect;
      else
        Rec.Kind := Accepting;
      end if;

      if List.Search_By_Name (Rec.Kind, Fifo_Name) then
        raise Yet_Open;
      end if;

      -- Connect or accept
      Rec.Conn_Cb := Connection_Cb;
      Rec.Rece_Cb := Reception_Cb;
      Rec.Ovfl_Cb := End_Overflow_Cb;
      if To_Remote then
        -- Notify then try to get
        Dictio.Notify (Fifo_Name, True);
        Rec.Dscr := Socket.No_Socket;
        Rec.Port := (Kind => Tcp_Util.Port_Num_Spec, Num => 0);
        Dictio.Get (Fifo_Name, Got, Rec.Host.Name, Rec.Port.Num);
        if not Got then
          -- No item: wait for notification
          Rec.State := Waiting;
          return (Acc => List.Insert (Rec));
        else
          -- Try to connect on this data
          begin
            Host_Name2Id (Rec.Host);
          exception
            when Name_Error =>
              Rec.State := Waiting;
              return (Acc => List.Insert (Rec));
          end;
          Rec.State := Connecting;
          Acc := List.Insert (Rec);
          Connection.Connect (Acc);
        end if;
        return (Acc => Acc);
      else
        -- Accept
        Rec.State := Connected;
        Rec.Port.Name (1 .. Fifo_Name'Length) := Fifo_Name;
        Acc := List.Insert (Rec);
        Connection.Accepte (Acc);
        -- Set dictio
        Dictio.Set (Fifo_Name, Local_Host_Name, Acc.Port.Num);
        -- Save in list
        return (Acc => Acc);
      end if;

    end Open;

    -- Close a fifo by access
    procedure Close (Acc : Fifo_Access) is
      procedure Finish is
      begin
        Connection.Close (Acc);
        List.Del_Current;
      end Finish;
    begin
      -- Cancel notification for remote fifo
      if Acc.Kind = Connect then
        begin
          Dictio.Notify (Acc.Name(1 .. Acc.Len), False);
        exception
          when No_Dictio =>
            null;
          when others =>
            Finish;
            raise;
        end;
      end if;
      -- Disconnect and del record
      Finish;
    end Close;

    -- Close a connection to a remote
    -- or close a connection from remote
    -- or cease accepting connections from remotes
    -- May raise Not_Open if Fifo is not open
    procedure Close (Id : in out Fifo_Id) is
      use type Fifo_Access;
    begin
      if Id = No_Fifo or else not List.Search_By_Dscr (Id.Acc.Dscr) then
        raise Not_Open;
      end if;
      Close (Id.Acc);
      Id := No_Fifo;
    exception
      when others =>
        Id := No_Fifo;
        raise;
    end Close;

    -- Close all fifos of all kind this Message_Type
    procedure Close_All is
      Id : Fifo_Id;
    begin
      List.Rewind;
      while not List.Empty loop
        begin
          Close (List.Access_Current);
        exception
          when others =>
            null;
        end;
      end loop;
    end Close_All;

    -- Kind and state of a fifo
    -- May raise Not_Open if Fifo is not open
    function Fifo_Kind (Id : Fifo_Id) return Fifo_Kind_List is
      use type Fifo_Access;
    begin
      if Id = No_Fifo or else not List.Search_By_Dscr (Id.Acc.Dscr) then
        raise Not_Open;
      end if;
      return Id.Acc.Kind;
    end Fifo_Kind;

    function Fifo_State (Id : Fifo_Id) return Fifo_State_List is
      use type Fifo_Access;
    begin
      if Id = No_Fifo or else not List.Search_By_Dscr (Id.Acc.Dscr) then
        raise Not_Open;
      end if;
      return Id.Acc.State;
    end Fifo_State;


    procedure End_Overflow_Cb (Dscr : in Socket.Socket_Dscr) is
      Acc : Fifo_Access;
    begin
      if not List.Search_By_Dscr (Dscr) then
        Assertion.Assert (False, "end ovf of unknown fifo");
        return;
      end if;
      Acc := List.Access_Current;
      if Acc.Kind /= Connect then
        Assertion.Assert (False, "end ovf of " & Acc.Kind'Img & " fifo");
        return;
      end if;
      if Acc.State /= Connected then
        Assertion.Assert (False, "end ovf of " & Acc.State'Img & " fifo");
        return;
      end if;

      if Acc.Ovfl_Cb /= null then
        Acc.Ovfl_Cb (Id => (Acc => Acc) );
      end if;
    end End_Overflow_Cb;

    -- Send a message
    -- May raise Not_Open if Fifo is not open
    function Send (Id      : in Fifo_Id;
                   Message : in Message_Type;
                   Length  : in Message_Length := 0) return Send_Result_List is
    begin
      if Id = No_Fifo then
        raise Not_Open;
      end if;
      if Fifo_Send (Id.Acc.Dscr, End_Overflow_Cb'Unrestricted_Access,
                    Message, Length) then
        return Ok;
      else
        return Overflow;
      end if;
    exception
      when Not_Open =>
        raise;
      when Socket.Soc_Tail_Err =>
        raise In_Overflow;
      when Socket.Soc_Conn_Lost =>
        if not List.Search_By_Dscr (Id.Acc.Dscr) then
          raise Not_Open;
        else
          return Disconnected;
        end if;
      when Socket.Soc_Use_Err =>
        return Disconnected;
      when others =>
        return Error;
    end Send;

  end Fifo;

end Fifos;

