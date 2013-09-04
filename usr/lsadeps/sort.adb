with Directory, Dynamic_List, Hashed_List.Unique;
with Debug;
package body Sort is

  function Make_Path (Path, Unit : As.U.Asu_Us) return As.U.Asu_Us is
  begin
    return As.U.Tus (Make_Path (Path, Unit));
  end Make_Path;
  function Make_Path (Path, Unit : As.U.Asu_Us) return String is
  begin
    return Make_Path (Path.Image, Unit.Image);
  end Make_Path;
  function Make_Path (Path, Unit : String) return As.U.Asu_Us is
  begin
    return As.U.Tus (Make_Path (Path, Unit));
  end Make_Path;
  function Make_Path (Path, Unit : String) return String is
  begin
    return Directory.Build_File_Name (Path, Unit, "");
  end Make_Path;

  -- Unique list of prios
  type Prio_Rec is record
    Path : As.U.Asu_Us;
    Prio : Positive;
  end record;
  type Prio_Access is access all Prio_Rec;
  procedure Set (To : out Prio_Rec; Val : in Prio_Rec) is
  begin
    To := Val;
  end Set;
  function "=" (L, R : Prio_Rec) return Boolean is
   use type As.U.Asu_Us;
  begin
    return L.Path = R.Path;
  end "=";
  function Image (Elt : Prio_Rec) return String is
  begin
    return Elt.Path.Image;
  end Image;
  package H_Prio_List_Mng is new Hashed_List (
       Prio_Rec, Prio_Access, Set, "=" , Image);
  package Prio_Ulist_Mng is new H_Prio_List_Mng.Unique;
  Prio_Ulist : Prio_Ulist_Mng.Unique_List_Type;

  function Less_Than (E1, E2 : Prio_Rec) return Boolean is
  begin
    return E1.Prio < E2.Prio;
  end Less_Than;
  package Prio_Dyn_List_Mng is new Dynamic_List (Prio_Rec);
  package Prio_List_Mng renames Prio_Dyn_List_Mng.Dyn_List;
  procedure Prio_Sort is new Prio_List_Mng.Sort (Less_Than);

  -- store Path with prio
  Current_Prio : Positive := 1;
  procedure Add_Path (Path : in As.U.Asu_Us) is
    R : Prio_Rec;
    L : Natural;
  begin
    -- Remove trailing '/' if not "/"
    R.Path := As.U.Tus (Directory.Make_Full_Path (Path.Image));
    R.Prio := Current_Prio;
    Current_Prio := Current_Prio + 1;
    L := R.Path.Length;
    if L > 1 and then R.Path.Element (L) = '/' then
      R.Path.Delete (L, L);
    end if;
    if L = 0 then
      return;
    end if;
    -- Insert if new (otherwise it already exists with better prio)
    Debug.Logger.Log_Debug ("Adding path " & R.Path.Image
                          & " with prio" & R.Prio'Img);
    Prio_Ulist.Insert_If_New (R);
  end Add_Path;

  function Get_Paths return As.U.Utils.Asu_Ua.Unb_Array is
    Result : As.U.Utils.Asu_Ua.Unb_Array;
    Prio : Prio_Rec;
    Moved : Boolean;
    List : Prio_List_Mng.List_Type;
  begin
    if Prio_Ulist.Is_Empty then
      return Result;
    end if;
    -- Build list and sort
    Prio_Ulist.Rewind;
    loop
      Prio_Ulist.Read_Next (Prio, Moved);
      List.Insert (Prio);
      exit when not Moved;
    end loop;
    Prio_Sort (List);
    -- Copy in array
    List.Rewind;
    loop
      List.Get (Prio, Moved => Moved);
      Result.Append (Prio.Path);
      exit when not Moved;
    end loop;
    return Result;
  end Get_Paths;

  -- Sort entries ([<path>/]<file>)
  --  First the entries without path
  --  Then in order of prio
  --  Then the entries without prio
  --  At each level in alpha order, but .ads then .adb
  function Less_Than (E1, E2 : As.U.Asu_Us) return Boolean is
    D1 : constant String := Directory.Dirname (E1.Image);
    D2 : constant String := Directory.Dirname (E2.Image);
    R1, R2 : Prio_Rec;
    -- Compare files: prefix then suffix (with specific .ads < .adb)
    function File_Less_Than return Boolean is
      F1 : constant String := Directory.Basename (E1.Image);
      F2 : constant String := Directory.Basename (E2.Image);
      P1 : constant String := Directory.File_Prefix (F1);
      P2 : constant String := Directory.File_Prefix (F2);
      S1 : constant String := Directory.File_Suffix (F1);
      S2 : constant String := Directory.File_Suffix (F2);
    begin
      if P1 < P2 then return True; end if;
      if P2 < P1 then return False; end if;
      if S1 = ".ads" and then S2 = ".adb" then return True; end if;
      if S2 = ".ads" and then S1 = ".adb" then return False; end if;
      return S1 < S2;
    end File_Less_Than;
    procedure Read (D : in String; R : in out Prio_Rec) is
      Found : Boolean;
    begin
     -- Search Dir without last '/'
      R.Path := As.U.Tus (D);
      R.Path.Delete (R.Path.Length, R.Path.Length);
      Prio_Ulist.Search (R, Found);
      if Found then
        Prio_Ulist.Read (R);
      else
        R.Prio := Positive'Last;
      end if;
    end Read;
  begin
    if D1 = "" and then D2 /= "" then return True; end if;
    if D1 /= "" and then D2 = "" then return False; end if;
    if D1 = D2 then return File_Less_Than; end if;
    -- Apply prio of path
    Read (D1, R1);
    Read (D2, R2);
    if R1.Prio /= R2.Prio then return R1.Prio < R2.Prio; end if;
    -- Same prio, order of path
    if D1 /= D2 then return D1 < D2; end if;
    -- Same path
    return File_Less_Than;
  end Less_Than;

  procedure Path_Sort is new As.U.Utils.Asu_Dyn_List_Mng.Sort (Less_Than);

  procedure Sort (List : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type) is
  begin
    Path_Sort (List);
  end Sort;

end Sort;

