with Sys_Calls, Normal;
with Definition, Scrambler_Factory, Io_Manager;
package body Coder is

  Debug : Boolean := False;

  -- The coder config
  type Jammer_Array is array (Definition.Jammers_Index range <>)
                          of Scrambler_Factory.Jammer_Type;

  type Coder_T (Nb_Jammers : Definition.Jammers_Range := 0) is record
    Switch : Scrambler_Factory.Switch_Type;
    Jammers      : Jammer_Array (1 .. Nb_Jammers);
    Back         : Scrambler_Factory.Back_Type;
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
                  Switch => Scrambler_Factory.Create,
                  Jammers => (others => Dummy_Jammer),
                  Back => Dummy_Back);
    -- Set to real values
    -- Switch
    for I in 1 .. Def.Switch.Nb_Switches loop
      Scrambler_Factory.Set (
         Scrambler   => The_Coder.Switch,
         Association => (E => Types.Id_Of (Def.Switch.Switch(I).E),
                         D => Types.Id_Of (Def.Switch.Switch(I).D)) );
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
      Scrambler_Factory.Set_Carry (
         Jammer => The_Coder.Jammers(I),
         Carry_Offset => Types.Id_Of (Def.Jammers.Jammers(I).Carry_Offset) );
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
  exception
    when Definition.Invalid_Definition =>
      raise Init_Failed;
  end Init;

  -- Image of a jammer number
  function Img (N : Definition.Scrambler_Range) return String is
  begin
    return Normal (Natural(N), 1);
  end Img;

  -- Encode a letter
  function Encode (L : Types.Letter) return Types.Letter is
    X : Types.Lid;
    Carry : Boolean;
  begin
    X := Types.Id_Of(L);
    if Debug then
      Sys_Calls.Put_Line_Error ("Encoding -> " & L);
    end if;
    -- Encode through switch
    X := Scrambler_Factory.Encode (The_Coder.Switch, X);
    if Debug then
      Sys_Calls.Put_Line_Error (" S  -> " & Types.Letter_Of (X));
    end if;
    -- Encode through the jammers
    for I in 1 .. The_Coder.Nb_Jammers loop
      X := Scrambler_Factory.Encode (The_Coder.Jammers(I), X);
      if Debug then
        Sys_Calls.Put_Line_Error (" J" & Img (I) & " -> " & Types.Letter_Of (X));
      end if;
    end loop;
    -- Encode through the back
    X := Scrambler_Factory.Encode (The_Coder.Back, X);
    if Debug then
      Sys_Calls.Put_Line_Error (" B  -> " & Types.Letter_Of (X));
    end if;
    -- Decode through the jammers
    for I in reverse 1 .. The_Coder.Nb_Jammers loop
      X := Scrambler_Factory.Decode (The_Coder.Jammers(I), X);
      if Debug then
        Sys_Calls.Put_Line_Error (" J" & Img (I) & " -> " & Types.Letter_Of (X));
      end if;
    end loop;
    -- Decode through switch
    X := Scrambler_Factory.Decode (The_Coder.Switch, X);
    if Debug then
      Sys_Calls.Put_Line_Error (" S  -> " & Types.Letter_Of (X));
    end if;

    -- Now increment the jammers as long as they raise the carry
    for I in 1 .. The_Coder.Nb_Jammers loop
      Scrambler_Factory.Increment (The_Coder.Jammers(I), Carry);
      if Debug then
        Sys_Calls.Put_Line_Error (" Inc " & Img(I) & " " & Carry'Img);
      end if;
      exit when not Carry;
    end loop;
    if Debug then
      Sys_Calls.Put_Line_Error ("Encoded as -> " & Types.Letter_Of (X));
    end if;

    -- Done
    return Types.Letter_Of (X);
  end Encode;

end Coder;

