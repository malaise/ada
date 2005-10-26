with My_Io;
with Dyn_Data;
package body Hash is

  package body Hash_Mng is

    -- Data organization:
    --  An array (0 .. Hash_Size) of First_Cell_Rec
    --  Each of them pointing possibly to a Cell_Rec which itself...
    type Cell_Rec;
    type Cell_Access is access Cell_Rec;

    type Cell_Rec is record
      Data : Data_Acess;
      Next : Cell_Access := null;
    end record;


    type First_Cell_Rec is record
      First     : Cell_Access := null;
      Current   : Cell_Access := null;
    end record;

    subtype Hash_Range is Max_Hash_Range range 0 .. Hash_Size;

    -- The entries of the table
    First_Array : array (Hash_Range) of First_Cell_Rec;

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
    procedure Store (Key : in String; Data : in Data_Acess) is
      I : constant Hash_Range := Hash_Func(Key);
      Ca, N : Cell_Access;
    begin
      Ca := Dyn_Hash.Allocate ((Data => Data, Next  => null));

      -- Append
      if First_Array(I).First = null then
        First_Array(I).First := Ca;
      else
        N := First_Array(I).First;
        while N.Next /= null loop
          N := N.Next;
        end loop;
        N.Next := Ca;
      end if;

    end Store;

    -- To remove a stored association Key <-> Index
    procedure Remove (Key : in String) is
      I : constant Hash_Range := Hash_Func(Key);
      Ca : Cell_Access;
      Cu : Cell_Access;
    begin
      Cu := First_Array(I).Current;
      Ca := First_Array(I).First;
      -- Empty or not found
      if Ca = null or else Cu = null then
        raise Not_Found;
      end if;

      -- Reset current
      First_Array(I).Current := null;

      if Ca = Cu then
        -- Special case when current is first
        First_Array(I).First := Ca.Next;
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

    procedure Reset_Find (Key : String) is
      I : constant Hash_Range := Hash_Func(Key);
    begin
      First_Array(I).Current := null;
    end Reset_Find;

    -- To get next Index matching Key
    function Find_Next (Key : String) return Found_Rec is
      I : constant Hash_Range := Hash_Func(Key);
      Cu : Cell_Access;
    begin
      if First_Array(I).Current = null then
        Cu := First_Array(I).First;
      else
        Cu := First_Array(I).Current.Next;
      end if;

      First_Array(I).Current := Cu;

      if Cu = null then
        return Not_Found_Rec;
      else
        return (Found => True, Data => Cu.Data);
      end if;

    end Find_Next;

    procedure Dump (Key : in String) is
      I : constant Hash_Range := Hash_Func(Key);
      Ca : Cell_Access := First_Array(I).First;
    begin
      My_Io.Put_Line ("Hash " & Hash_Range'Image(I));
      if Ca = null then
        My_Io.Put_Line (" No data found");
      end if;
      while Ca /= null loop
        My_Io.Put (" Data found ");
        if Ca = First_Array(I).Current then
          My_Io.Put (" => ");
        else
          My_Io.Put (" -> ");
        end if;

        Dump (Ca.Data);
        My_Io.New_Line;
        Ca := Ca.Next;
      end loop;
    end Dump;

    procedure Clear_All is
      Ca, Cn : Cell_Access;
    begin
      for I in Hash_Range loop
        Ca := First_Array(I).First;
        First_Array(I).First := null;
        First_Array(I).Current := null;

        while Ca /= null loop
          Cn := Ca.Next;
          Dyn_Hash.Free (Ca);
          Ca := Cn;
        end loop;
      end loop;
    end Clear_All;

  end Hash_Mng;

end Hash;

