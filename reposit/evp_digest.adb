-- Binding (thin) to some OpenSSL EVP_Digest function
with Aski;
package body Evp_Digest is

  -- Interfaces
  procedure Openssl_Add_All_Digests;
  pragma Import (C, Openssl_Add_All_Digests, "OpenSSL_add_all_digests");

  function Evp_Get_Digestbyname (Name : System.Address) return System.Address;
  pragma Import (C, Evp_Get_Digestbyname, "EVP_get_digestbyname");

  function Evp_Md_Ctx_Create return System.Address;
  pragma Import (C, Evp_Md_Ctx_Create, "EVP_MD_CTX_create");

  procedure Evp_Digestinit_Ex (Ctx : in System.Address;
                               Md : in System.Address;
                               Engine : in System.Address);
  pragma Import (C, Evp_Digestinit_Ex, "EVP_DigestInit_ex");

  procedure Evp_Digestupdate (Ctx : in System.Address;
                              Msg : in System.Address;
                              Len : in C_Types.Size_T);
  pragma Import (C, Evp_Digestupdate, "EVP_DigestUpdate");

  procedure Evp_Digestfinal_Ex (Ctx : in System.Address;
                                Md  : in System.Address;
                                Len : in System.Address);
  pragma Import (C, Evp_Digestfinal_Ex, "EVP_DigestFinal_ex");

  procedure Evp_Md_Ctx_Cleanup (Ctx : in System.Address);
  pragma Import (C, Evp_Md_Ctx_Cleanup, "EVP_MD_CTX_cleanup");

  procedure Evp_Md_Ctx_Destroy (Ctx : in System.Address);
  pragma Import (C, Evp_Md_Ctx_Destroy, "EVP_MD_CTX_destroy");

  -- Global init
  Initialized : Boolean := False;
  procedure Init is
  begin
    if not Initialized then
      -- Load all digests only once
      Openssl_Add_All_Digests;
      Initialized := True;
    end if;
  end Init;

  procedure Init (Ctx : in out Context; Name : in String) is
    Str4C : constant String := Name & Aski.Nul;
    use type System.Address;
  begin
    -- Check that Ctx is not in use
    if Ctx.Evp_Md_Ctx /= System.Null_Address then
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
    Evp_Digestinit_Ex (Ctx.Evp_Md_Ctx, Ctx.Evp_Md, System.Null_Address);
    Ctx.Clean := False;
  end Init;

  -- Update the context with some text
  -- May raise Status_Error if Ctx is not init or finalized
  procedure Update (Ctx : in out Context; Text : in String) is
    Len : constant C_Types.Size_T := C_Types.Size_T (Text'Length);
    use type System.Address;
  begin
    -- Check that Ctx is initialized
    if Ctx.Evp_Md_Ctx = System.Null_Address then
      raise Status_Error;
    end if;
    if Ctx.Clean then
      Evp_Digestinit_Ex (Ctx.Evp_Md_Ctx, Ctx.Evp_Md, System.Null_Address);
      Ctx.Clean := False;
    end if;

    Evp_Digestupdate (Ctx.Evp_Md_Ctx, Text(Text'First)'Address, Len);
  end Update;

  procedure Update (Ctx : in out Context; Bytes : in Byte_Array) is
    Len : constant C_Types.Size_T := C_Types.Size_T (Bytes'Length);
    use type System.Address;
  begin
    -- Check that Ctx is initialized
    if Ctx.Evp_Md_Ctx = System.Null_Address then
      raise Status_Error;
    end if;
    if not Ctx.Clean then
      Evp_Digestinit_Ex (Ctx.Evp_Md_Ctx, Ctx.Evp_Md, System.Null_Address);
      Ctx.Clean := False;
    end if;

    Evp_Digestupdate (Ctx.Evp_Md_Ctx, Bytes(Bytes'First)'Address, Len);
  end Update;

  -- Get the digest and empties the Ctx for new calls to Update
  -- May raise Status_Error if Ctx is not init or finalized
  function Get (Ctx : in out Context) return Byte_Array is
    Md : Byte_Array (1 .. Evp_Max_Md_Size);
    Len : C_Types.Uint32;
    use type System.Address;
  begin
    -- Check that Ctx is initialized
    if Ctx.Evp_Md_Ctx = System.Null_Address then
      raise Status_Error;
    end if;

    -- Get Md
    Evp_Digestfinal_Ex (Ctx.Evp_Md_Ctx, Md(Md'First)'Address, Len'Address);

    -- Clean context
    Evp_Md_Ctx_Cleanup (Ctx.Evp_Md_Ctx);
    Ctx.Clean := True;

    -- Return valid part
    return Md (Md'First .. Md'First + Integer (Len) - 1);
  end Get;

  -- Finalize the context
  -- May raise Status_Error if Ctx is not init or finalized
  procedure Finalize (Ctx : in out Context) is
    use type System.Address;
  begin
    -- Check that Ctx is initialized
    if Ctx.Evp_Md_Ctx /= System.Null_Address then
      Evp_Md_Ctx_Destroy (Ctx.Evp_Md_Ctx);
      Ctx.Evp_Md_Ctx := System.Null_Address;
    end if;
  end Finalize;

end Evp_Digest;

