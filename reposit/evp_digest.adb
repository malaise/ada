-- Binding (thin) to some OpenSSL EVP_Digest function
with Aski, Lower_Str;
package body Evp_Digest is

  -- Interfaces
  procedure Openssl_Add_All_Digests
    with Import => True, Convention => C,
         External_Name => "openssl_add_all_digests";

  function Evp_Max_Md_Size return Integer
    with Import => True, Convention => C, External_Name => "evp_max_md_size";

  function Evp_Get_Digestbyname (Name : System.Address) return System.Address
    with Import => True, Convention => C,
         External_Name => "evp_get_digestbyname";

  function Evp_Md_Ctx_Create return System.Address
    with Import => True, Convention => C, External_Name => "evp_md_ctx_create";

  function Evp_Digestinit_Ex (Ctx : in System.Address;
                              Md : in System.Address;
                              Engine : in System.Address) return Integer
    with Import => True, Convention => C, External_Name => "evp_digestinit_ex";

  function Evp_Digestupdate (Ctx : in System.Address;
                             Msg : in System.Address;
                             Len : in C_Types.Size_T) return Integer
    with Import => True, Convention => C, External_Name => "evp_digestupdate";

  function Evp_Digestfinal_Ex (Ctx : in System.Address;
                               Md  : in System.Address;
                               Len : in System.Address) return Integer
    with Import => True, Convention => C,
         External_Name =>  "evp_digestfinal_ex";

  function Evp_Md_Ctx_Cleanup (Ctx : in System.Address) return Integer
    with Import => True, Convention => C, External_Name => "evp_md_ctx_cleanup";

  procedure Evp_Md_Ctx_Destroy (Ctx : in System.Address)
    with Import => True, Convention => C, External_Name => "evp_md_ctx_destroy";

  -- Defined in C as as 64 (for SHA512), so this is a safe default value
  Max_Md_Size : Integer := 256;

  -- Global init
  Initialized : Boolean := False;
  procedure Init is
  begin
    if not Initialized then
      -- Load all digests only once
      Openssl_Add_All_Digests;
      Max_Md_Size := Evp_Max_Md_Size;
      Initialized := True;
    end if;
  end Init;

  procedure Init (Ctx : in out Context; Digest_Name : in String) is
    Str4C : constant String := Digest_Name & Aski.Nul;
    Dummy_Res : Integer;
    use type System.Address;
  begin
    -- Check that Ctx is not in use
    if Is_Init (Ctx) then
      raise Status_Error;
    end if;
    -- Global init
    Init;

    -- Get digest by name
    Ctx.Evp_Md := Evp_Get_Digestbyname (Str4C(Str4C'First)'Address);
    if Ctx.Evp_Md = System.Null_Address then
      raise Name_Error;
    end if;
    -- Init context
    Ctx.Evp_Md_Ctx := Evp_Md_Ctx_Create;
    Dummy_Res := Evp_Digestinit_Ex (Ctx.Evp_Md_Ctx, Ctx.Evp_Md,
                                    System.Null_Address);
    Ctx.Is_Clean := False;
  end Init;

  procedure Init (Ctx : in out Context; Digest : in Digest_List) is
  begin
    Init (Ctx, Lower_Str (Digest'Img));
  end Init;

  function Is_Init (Ctx : in Context) return Boolean is
    use type System.Address;
  begin
    return Ctx.Evp_Md_Ctx /= System.Null_Address;
  end Is_Init;

  -- Update the context with some text
  -- May raise Status_Error if Ctx is not init or finalized
  procedure Update (Ctx : in out Context; Text : in String) is
    Len : constant C_Types.Size_T := Text'Length;
    Dummy_Res : Integer;
  begin
    -- Check that Ctx is initialized
    if not Is_Init (Ctx) then
      raise Status_Error;
    end if;
    if Ctx.Is_Clean then
      Dummy_Res := Evp_Digestinit_Ex (Ctx.Evp_Md_Ctx, Ctx.Evp_Md,
                                      System.Null_Address);
      Ctx.Is_Clean := False;
    end if;

    Dummy_Res := Evp_Digestupdate (Ctx.Evp_Md_Ctx, Text(Text'First)'Address,
                                   Len);
  end Update;

  procedure Update (Ctx : in out Context; Bytes : in Byte_Array) is
    Len : constant C_Types.Size_T := Bytes'Length;
    Dummy_Res : Integer;
  begin
    -- Check that Ctx is initialized
    if not Is_Init (Ctx) then
      raise Status_Error;
    end if;
    if Ctx.Is_Clean then
      Dummy_Res := Evp_Digestinit_Ex (Ctx.Evp_Md_Ctx, Ctx.Evp_Md,
                                      System.Null_Address);
      Ctx.Is_Clean := False;
    end if;

    Dummy_Res := Evp_Digestupdate (Ctx.Evp_Md_Ctx, Bytes(Bytes'First)'Address,
                                   Len);
  end Update;

  -- Read the digest and let it ready for adding new updates
  -- May raise Status_Error if Ctx is not init or reset
  function Read (Ctx : in out Context) return Byte_Array is
    Md : Byte_Array (1 .. Max_Md_Size);
    Len : C_Types.Uint32;
    Dummy_Res : Integer;
  begin
    -- Check that Ctx is initialized
    if not Is_Init (Ctx) then
      raise Status_Error;
    end if;

    -- Get Md
    Dummy_Res := Evp_Digestfinal_Ex (Ctx.Evp_Md_Ctx, Md(Md'First)'Address,
                                     Len'Address);

    -- Return valid part
    return Md (Md'First .. Md'First + Integer (Len) - 1);
  end Read;

  -- Empty the Ctx for new calls to Update
  -- May raise Status_Error if Ctx is not init or reset
  procedure Clean (Ctx : in out Context) is
    Dummy_Res : Integer;
  begin
    -- Clean context
    Dummy_Res := Evp_Md_Ctx_Cleanup (Ctx.Evp_Md_Ctx);
    Ctx.Is_Clean := True;
  end Clean;

  -- Get the digest and empty the Ctx for new calls to Update
  -- May raise Status_Error if Ctx is not init or finalized
  function Get (Ctx : in out Context) return Byte_Array is
    Md : constant Byte_Array := Read (Ctx);
  begin
    Clean (Ctx);
    return Md;
  end Get;

  -- Reset the context for a new Init
  -- May raise Status_Error if Ctx is not init or already reset
  procedure Reset (Ctx : in out Context) is
  begin
    if not Is_Init (Ctx) then
      -- Context not init or already reset
      raise Status_Error;
    end if;
    Evp_Md_Ctx_Destroy (Ctx.Evp_Md_Ctx);
    Ctx.Evp_Md_Ctx := System.Null_Address;
  end Reset;

  -- Finalize the context
  overriding procedure Finalize (Ctx : in out Context) is
  begin
    -- Check that Ctx is initialized
    if Is_Init (Ctx) then
      Reset (Ctx);
    end if;
  end Finalize;

end Evp_Digest;

