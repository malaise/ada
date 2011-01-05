with As.U, Assertion, String_Mng, Normal, Environ;
with Dictio_Lib;
package body Fifos is

  -------------------------------------------------------------------------
  -- COMMON
  -------------------------------------------------------------------------

  -- These are the natural host name and id
  Local_Host_Name : constant String := Socket.Local_Host_Name;
  Local_Host_Id : constant Tcp_Util.Host_Id := Socket.Local_Host_Id;

  procedure Host_Name2Id (Host : in out Tcp_Util.Remote_Host) is
    Id : Tcp_Util.Host_Id;
  begin
    Id := Socket.Host_Id_Of (Host.Name.Image);
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
        Fifo_List.Insert (Rec);
        return Access_Current;
      end Insert;

      -- Get access to current record
      function Access_Current return Fifo_Access is
      begin
        return Fifo_List.Access_Current;
      end Access_Current;

      -- Delete current record and move to next
      procedure Del_Current is
        Moved : Boolean;
      begin
        Fifo_List.Delete (Moved => Moved);
      end Del_Current;

      -- Move to first fifo in list
      procedure Rewind is
      begin
        Fifo_List.Rewind (False);
      end Rewind;

      -- Is list empty?
      function Empty return Boolean is
      begin
        return Fifo_List.Is_Empty;
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
        Res : Boolean;
      begin
        Rec.Kind := Kind;
        Rec.Len := Name'Length;
        Rec.Name (1 .. Rec.Len) := Name;
        Search_Name (Fifo_List, Res, Rec, From => Fifo_List_Mng.Absolute);
        return Res;
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
        Res : Boolean;
      begin
        Rec.Kind := Kind;
        Rec.Port := (Kind => Tcp_Util.Port_Num_Spec, Num => Port);

        Search_Port (Fifo_List, Res, Rec, From => Fifo_List_Mng.Absolute);
        return Res;
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
        Res : Boolean;
      begin
        Rec.Dscr := Dscr;
        Search_Dscr (Fifo_List, Res, Rec, From => Fifo_List_Mng.Absolute);
        return Res;
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
        Res : Boolean;
      begin
        Rec.Kind := Kind;
        Rec.Host := (Kind => Tcp_Util.Host_Id_Spec, Id => Host);
        Rec.Port := (Kind => Tcp_Util.Port_Num_Spec, Num => Port);

        Search_Addr (Fifo_List, Res, Rec, From => Fifo_List_Mng.Absolute);
        return Res;
      end Search_By_Addr;
    end List;

    -------------------------------------------------------------------------
    -- CONNECTION
    -------------------------------------------------------------------------
    package Connection is
      procedure Accepte (Fifo : in Fifo_Access);
      procedure Connect (Fifo : in Fifo_Access);
      procedure Close (Fifo : in Fifo_Access);
      procedure Activate (Fifo : in Fifo_Access);
    end Connection;

    package body Connection is

      package Fifo_Reception is new Tcp_Util.Reception (Message_Type);

      procedure Close_Socket (Dscr : in out Socket.Socket_Dscr;
                              May_Be_Sending : Boolean) is
      begin
        if May_Be_Sending then
          Tcp_Util.Abort_Send_And_Close (Dscr);
        else
          Dscr.Close;
        end if;
      exception
        when Tcp_Util.No_Such =>
          -- May_Be_Sending but was not
          Dscr.Close;
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
          -- Conn_Cb calls may have changed current: find again
          if not List.Search_By_Dscr (Dscr) then
            -- Conn_Cb removed diconnected fifo
            return;
          end if;
          -- "Close"
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
        pragma Unreferenced (Local_Dscr);
        Tmp_Dscr : Socket.Socket_Dscr;
        Rec : Fifo_Rec;
        Acc : Fifo_Access;
      begin
        -- Get accepting rec
        if not List.Search_By_Port (Accepting, Local_Port_Num) then
          Tmp_Dscr := New_Dscr;
          Close_Socket (Tmp_Dscr, False);
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
        -- Deactivate callbacks if requested
        if not Acc.Active then
          Fifo_Reception.Activate_Callbacks (New_Dscr, False);
        end if;

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
          -- Deactivate callbacks if requested
          if not Acc.Active then
            Fifo_Reception.Activate_Callbacks (Dscr, False);
          end if;
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
        pragma Unreferenced (Result);
        Protocol : Socket.Protocol_List;
        use type Socket.Host_Id, Tcp_Util.Remote_Host_List;
      begin
        if Fifo.Kind /= Connect then
          Assertion.Assert (False, "connecting a fifo of kind "
                          & Fifo.Kind'Img);
          return;
        end if;
        if Fifo.Host.Kind = Tcp_Util.Host_Id_Spec
        and then Fifo.Host.Id = Local_Host_Id then
          Protocol := Socket.Tcp_Header_Afux;
        else
          Protocol := Socket.Tcp_Header;
        end if;
        Result := Tcp_Util.Connect_To (
                       Protocol,
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
          Id : constant Fifo_Id := (Acc => Fifo);
        begin
          -- Call Connection_Cb
          if Fifo.Conn_Cb /= null then
            Fifo.Conn_Cb (Fifo.Name(1 .. Fifo.Len), Id, False);
          end if;
        end Call_Cb;
        use type Socket.Socket_Dscr;
      begin
        case Fifo.Kind is
          when Connect =>
            case Fifo.State is
              when Waiting =>
                null;
              when Connecting =>
                Tcp_Util.Abort_Connect (Fifo.Host, Fifo.Port);
              when Connected =>
                if Fifo.Active then
                  Fifo_Reception.Remove_Callbacks (Fifo.Dscr);
                end if;
                Close_Socket (Fifo.Dscr, True);
                Call_Cb;
            end case;
          when Accepting =>
            Tcp_Util.Abort_Accept (Socket.Tcp_Header, Fifo.Port.Num);
            Tcp_Util.Abort_Accept (Socket.Tcp_Header_Afux, Fifo.Port.Num);
          when Accepted =>
            if Fifo.Active then
              Fifo_Reception.Remove_Callbacks (Fifo.Dscr);
            end if;
            Close_Socket (Fifo.Dscr, True);
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
        -- Accept from Tcp Inet and afux
        Tcp_Util.Accept_From (Socket.Tcp_Header,
                              Port,
                              Acception_Cb'Unrestricted_Access,
                              Fifo.Dscr,
                              Port_Num);
        -- Link Afux on same port
        Port := (Kind => Tcp_Util.Port_Num_Spec,
                   Num  => Port_Num);
        Tcp_Util.Accept_From (Socket.Tcp_Header_Afux,
                              Port,
                              Acception_Cb'Unrestricted_Access,
                              Fifo.Afux_Dscr,
                              Port_Num);
        Fifo.Host := (Kind => Tcp_Util.Host_Id_Spec, Id => Local_Host_Id);
        Fifo.Port := (Kind => Tcp_Util.Port_Num_Spec, Num => Port_Num);
        Fifo.State := Connected;
      exception
        when Socket.Soc_Name_Not_Found =>
          raise Name_Error;
      end Accepte;

      procedure Activate (Fifo : in Fifo_Access) is
      begin
        Fifo_Reception.Activate_Callbacks (Fifo.Dscr, Fifo.Active);
      end Activate;

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

      Name_Prefix : As.U.Asu_Us;
      Default_Name_Prefix : constant String := "Fifo.";
      Name_Prefix_Name : constant String := "FIFO_NAME_SUFFIX";
      Separator : constant String := ":";

      Init_Done : Boolean := False;
      Dictio_State : Dictio_Lib.Dictio_State_List := Dictio_Lib.Unavailable;

      function Fifo_Name_Of (Item_Name : String) return String is
        Suff_Len : constant Integer := Name_Prefix.Length;
      begin
        if Item_Name'Length <= Suff_Len
        or else Item_Name(Item_Name'First .. Item_Name'First + Suff_Len - 1)
             /= Name_Prefix.Image then
          raise Data_Error;
        end if;
        return Item_Name(Suff_Len+1 .. Item_Name'Last);
      end Fifo_Name_Of;



      procedure Split (Data : in String; Host : out Tcp_Util.Host_Name;
                                         Port : out Tcp_Util.Port_Num) is
        Txt : As.U.Asu_Us;
        Sep_Index : Natural;
      begin
        Txt := As.U.Tus (Data);
        Sep_Index := String_Mng.Locate (Txt.Image, Separator);
        if Sep_Index <= 1 or else Sep_Index = Txt.Length then
          raise Data_Error;
        end if;

        begin
          Host := Txt.Uslice (1, Sep_Index - 1);
          Port := Tcp_Util.Port_Num'Value(
            Txt.Slice (Sep_Index + 1, Txt.Length) );
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
        pragma Unreferenced (Item);
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
        Acc.Host := (Kind => Tcp_Util.Host_Name_Spec, Name => As.U.Asu_Null);
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
        use type Dictio_Lib.Dictio_State_List;
      begin
        if Init_Done then
          return;
        end if;
        -- Init dictio
        Dictio_Lib.Dictio_State_Cb := State_Cb'Access;
        Dictio_Lib.Notify_Cb := Notify_Cb'Access;
        Dictio_Lib.Init;
        if Dictio_State = Dictio_Lib.Unavailable then
          raise No_Dictio;
        end if;

        -- Getenv name prefix
        if Environ.Is_Set (Name_Prefix_Name) then
          Environ.Get_Us (Name_Prefix_Name, Name_Prefix);
        else
          Name_Prefix := As.U.Tus (Default_Name_Prefix);
        end if;
        -- Check a name based on this prefix is accepted by dictio
        if not Dictio_Lib.Is_Valid_Item_Name (Name_Prefix.Image & "a") then
          Name_Prefix := As.U.Tus (Default_Name_Prefix);
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
        if Name_Prefix.Length + Fifo_Name'Length >
                     Dictio_Lib.Max_Name_Len then
          raise Name_Too_Long;
        end if;
        Dictio_Lib.Set (Name_Prefix.Image
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
        if Name_Prefix.Length + Fifo_Name'Length
               > Dictio_Lib.Max_Name_Len then
          raise Name_Too_Long;
        end if;

        begin
          Split (Dictio_Lib.Get (Name_Prefix.Image & Fifo_Name),
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
        if Name_Prefix.Length + Fifo_Name'Length
                  > Dictio_Lib.Max_Name_Len then
          raise Name_Too_Long;
        end if;
        Dictio_Lib.Notify (Name_Prefix.Image & Fifo_Name, True, On);
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
    function Open (Fifo_Name       : String;
                   To_Remote       : Boolean;
                   Connection_Cb   : Connection_Callback_Access;
                   Reception_Cb    : Reception_Callback_Access;
                   End_Overflow_Cb : End_Overflow_Callback_Access)
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
      Rec.Active := True;
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
        Rec.Port.Name := As.U.Tus (Fifo_Name);
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

    -- Activate or not the reception of messages
    -- May raise Not_Open if Fifo is not open
    procedure Activate (Id : Fifo_Id; Allow_Reception : in Boolean) is
      use type Fifo_Access;
    begin
      if Id = No_Fifo or else not List.Search_By_Dscr (Id.Acc.Dscr) then
        raise Not_Open;
      end if;
      -- Check if there is a change
      if Id.Acc.Active = Allow_Reception then
        return;
      end if;
      -- Store new activation (for accepting or connecting fifo)
      Id.Acc.Active := Allow_Reception;
      -- If fifo can already receive messages, also update now
      if Id.Acc.Kind = Accepted
      or else (Id.Acc.Kind = Connect and then Id.Acc.State = Connected) then
        Connection.Activate (Id.Acc);
      end if;
    end Activate;

    -- Check if reception is active on a fifo
    -- May raise Not_Open if Fifo is not open
    function Is_Active (Id : Fifo_Id) return Boolean is
    begin
      if Id = No_Fifo or else not List.Search_By_Dscr (Id.Acc.Dscr) then
        raise Not_Open;
      end if;
      return Id.Acc.Active;
    end Is_Active;

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

