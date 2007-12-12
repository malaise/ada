with My_Io;
with Dyn_Data;
pragma Elaborate (Dyn_Data);
package body Hash is

  package body Hash_Mng is

    Not_Found_Rec : constant Found_Rec
                  := (Found => False);

    -- To manage the linked cells
    package Dyn_Hash is new Dyn_Data (Cell_Rec, Cell_Access);


    function Hash_Func (Key : String) return Hash_Range is
      use type Crc_10.Max_Crc_Range;
    begin
      Crc_10.Rst;
      Crc_10.Add (Key);
      return Crc_10.Get rem Hash_Size;
    end Hash_Func;

    -- To store association Key <-> Index
    procedure Store (Table : in out Hash_Table;
                     Key   : in String;
                     Data  : in Data_Acess) is
      I : constant Hash_Range := Hash_Func(Key);
      Ca, N : Cell_Access;
    begin
      Ca := Dyn_Hash.Allocate ((Data => Data, Next  => null));

      -- Append
      if Table.Arr(I).First = null then
        Table.Arr(I).First := Ca;
      else
        N := Table.Arr(I).First;
        while N.Next /= null loop
          N := N.Next;
        end loop;
        N.Next := Ca;
      end if;

    end Store;

    -- To remove a stored association Key <-> Index
    procedure Remove (Table : in out Hash_Table;
                      Key   : in String) is
      I : constant Hash_Range := Hash_Func(Key);
      Ca : Cell_Access;
      Cu : Cell_Access;
    begin
      Cu := Table.Arr(I).Current;
      Ca := Table.Arr(I).First;
      -- Empty or not found
      if Ca = null or else Cu = null then
        raise Not_Found;
      end if;

      -- Reset current
      Table.Arr(I).Current := null;

      if Ca = Cu then
        -- Special case when current is first
        Table.Arr(I).First := Ca.Next;
      else
        -- Find previous of current
        while Ca.Next /= Cu loop
          Ca := Ca.Next;
        end loop;
        Ca.Next := Cu.Next;
      end if;

      -- Get rid of current
      Dyn_Hash.Free (Cu);

    end Remove;

    procedure Reset_Find (Table : in out Hash_Table;
                          Key   : in String) is
      I : constant Hash_Range := Hash_Func(Key);
    begin
      Table.Arr(I).Current := null;
    end Reset_Find;

    -- To get next Index matching Key
    procedure Find_Next (Table : in out Hash_Table;
                         Key   : in String;
                         Found : out Found_Rec) is
      I : constant Hash_Range := Hash_Func(Key);
      Cu : Cell_Access;
    begin
      if Table.Arr(I).Current = null then
        Cu := Table.Arr(I).First;
      else
        Cu := Table.Arr(I).Current.Next;
      end if;

      Table.Arr(I).Current := Cu;

      if Cu = null then
        Found := Not_Found_Rec;
      else
        Found := (Found => True, Data => Cu.Data);
      end if;
    end Find_Next;

    procedure Dump (Table : in Hash_Table;
                    Key   : in String) is
      I : constant Hash_Range := Hash_Func(Key);
      Ca : Cell_Access := Table.Arr(I).First;
    begin
      My_Io.Put_Line ("Hash " & Hash_Range'Image(I));
      if Ca = null then
        My_Io.Put_Line (" No data found");
      end if;
      while Ca /= null loop
        My_Io.Put (" Data found ");
        if Ca = Table.Arr(I).Current then
          My_Io.Put (" => ");
        else
          My_Io.Put (" -> ");
        end if;

        Dump (Ca.Data);
        My_Io.New_Line;
        Ca := Ca.Next;
      end loop;
    end Dump;

    procedure Clear_All (Table : in out Hash_Table) is
      Ca, Cn : Cell_Access;
    begin
      for I in Hash_Range loop
        Ca := Table.Arr(I).First;
        Table.Arr(I).First := null;
        Table.Arr(I).Current := null;

        while Ca /= null loop
          Cn := Ca.Next;
          Dyn_Hash.Free (Ca);
          Ca := Cn;
        end loop;
      end loop;
    end Clear_All;

  end Hash_Mng;

end Hash;

