with Definition, Scrambler_Factory, Io_Manager;
package body Coder is

  -- The coder config
  type Jammer_Array is array (Definition.Jammers_Index range <>)
                          of Scrambler_Factory.Jammer_Type;

  type Coder_T (Nb_Jammers : Definition.Jammers_Range := 0) is record
    First_Switch : Scrambler_Factory.Switch_Type;
    Jammers      : Jammer_Array (1 .. Nb_Jammers);
    Back         : Scrambler_Factory.Back_Type;
    Last_Switch  : Scrambler_Factory.Switch_Type;
  end record;
  The_Coder : Coder_T;
 
  -- Init enigma from arguments and config files
  procedure Init is
    Def : Definition.Def_Rec;
    Dummy_Jammer : Scrambler_Factory.Jammer_Type;
    Dummy_Back : Scrambler_Factory.Back_Type;
  begin
    -- Init the coder to Nb of jammers, switches are identity
    Definition.Read_Definition (Def);
    The_Coder := (Nb_Jammers => Def.Jammers.Nb_Jammers,
                  First_Switch => Scrambler_Factory.Create,
                  Jammers => (others => Dummy_Jammer),
                  Back => Dummy_Back,
                  Last_Switch => Scrambler_Factory.Create);
    -- Set to real values
    -- First switch
    for I in 1 .. Def.First_Switch.Nb_Switches loop
      Scrambler_Factory.Set (
         Scrambler   => The_Coder.First_Switch,
         Association => (E => Types.Id_Of (Def.First_Switch.Switch(I).E),
                         D => Types.Id_Of (Def.First_Switch.Switch(I).D)) );
    end loop;
    -- Jammers
    for I in 1 .. Def.Jammers.Nb_Jammers loop
      begin
        The_Coder.Jammers(I) := 
           Scrambler_Factory.Get (Def.Jammers.Jammers(I).Scrambler);
      exception
        when Scrambler_Factory.Unknown_Scrambler =>
          Io_Manager.Put_Line_Error ("ERROR. Unknown scrambler"
           & Def.Jammers.Jammers(I).Scrambler'Img
           & " for jammer" & I'Img & ".");
          raise Init_Failed;
        when Scrambler_Factory.Scrambler_In_Use =>
          Io_Manager.Put_Line_Error ("ERROR. Scrambler"
           & Def.Jammers.Jammers(I).Scrambler'Img
           & " for jammer" & I'Img & " already in use.");
          raise Init_Failed;
      end;
      Scrambler_Factory.Set_Offset (
         Jammer => The_Coder.Jammers(I),
         Offset => Types.Id_Of (Def.Jammers.Jammers(I).Offset) );
    end loop;
    -- Back
    begin
      The_Coder.Back := Scrambler_Factory.Get (Def.Back.Scrambler);
    exception
      when Scrambler_Factory.Unknown_Scrambler =>
        Io_Manager.Put_Line_Error ("ERROR. Unknown scrambler" 
           & Def.Back.Scrambler'Img & " for back.");
        raise Init_Failed;
      when Scrambler_Factory.Scrambler_In_Use =>
        Io_Manager.Put_Line_Error ("ERROR. Scrambler"
           & Def.Back.Scrambler'Img & " for back already in use.");
        raise Init_Failed;
      when Scrambler_Factory.Asymetric_Back =>
        Io_Manager.Put_Line_Error ("ERROR. Scrambler"
           & Def.Back.Scrambler'Img & " for back is not symetrical");
        raise Init_Failed;
    end;
    Scrambler_Factory.Set_Offset (
       Back   => The_Coder.Back,
       Offset => Types.Id_Of (Def.Back.Offset) );
    -- Last switch
    for I in 1 .. Def.Last_Switch.Nb_Switches loop
      Scrambler_Factory.Set (
         Scrambler   => The_Coder.Last_Switch,
         Association => (E => Types.Id_Of (Def.Last_Switch.Switch(I).E),
                         D => Types.Id_Of (Def.Last_Switch.Switch(I).D)) );
    end loop;
  exception
    when Definition.Invalid_Definition =>
      raise Init_Failed;
  end Init;

  -- Encode a letter
  function Encode (L : Types.Letter) return Types.Letter is
    X : Types.Lid;
    Carry : Boolean;
  begin
    X := Types.Id_Of(L);
    -- Encode through first switch
    X := Scrambler_Factory.Encode (The_Coder.First_Switch, X);
    -- Encode through the jammers
    for I in 1 .. The_Coder.Nb_Jammers loop
      X := Scrambler_Factory.Encode (The_Coder.Jammers(I), X);
    end loop;
    -- Encode through the back
    X := Scrambler_Factory.Encode (The_Coder.Back, X);
    -- Decode through the jammers
    for I in reverse 1 .. The_Coder.Nb_Jammers loop
      X := Scrambler_Factory.Decode (The_Coder.Jammers(I), X);
    end loop;
    -- Decode through last switch
    X := Scrambler_Factory.Decode (The_Coder.Last_Switch, X);

    -- Now increment the jammers as long as they raise the carry
    for I in 1 .. The_Coder.Nb_Jammers loop
      Scrambler_Factory.Increment (The_Coder.Jammers(I), Carry);
      exit when not Carry;
    end loop;

    -- Done
    return Types.Letter_Of (X);
  end Encode;

end Coder;

