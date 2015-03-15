-- Binding (thin) to some OpenSSL EVP_Digest function
with Ada.Unchecked_Deallocation;
with System;
with Aski;
package body Evp_Digest is

  -- Interfaces
  procedure Openssl_Add_All_Digests;
  pragma Import (C, Openssl_Add_All_Digests, "OpenSSL_add_all_digests");

  function Evp_Get_Digestbyname (Name : System.Address) return System.Address;
  pragma Import (C, Evp_Get_Digestbyname, "EVP_get_digestbyname");

  procedure Evp_Md_Ctx_Init (Ctx : in System.Address);
  pragma Import (C, Evp_Md_Ctx_Init, "EVP_MD_CTX_init");

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


  -- Global init
  Initialized : Boolean := False;
  procedure Init is
  begin
    if not Initialized then
      Openssl_Add_All_Digests;
      Initialized := True;
    end if;
  end Init;

  procedure Init (Ctx : in out Context; Name : in String) is
    Str4C : constant String := Name & Aski.Nul;
    Md : System.Address;
    use type System.Address;
  begin
    -- Check that Ctx is not in use
    if Ctx.Initialized then
      raise Status_Error;
    end if;
    -- Global init
    Init;

    -- Get digest by name
    Md := Evp_Get_Digestbyname (Str4C(Str4C'First)'Address);
    if Md = System.Null_Address then
      raise Name_Error;
    end if;
    -- Init context
    Ctx.Evp_Md_Ctx_Acc := new Evp_Md_Ctx;
    Evp_Md_Ctx_Init (Ctx.Evp_Md_Ctx_Acc.all'Address);
    Evp_Digestinit_Ex (Ctx.Evp_Md_Ctx_Acc.all'Address,
                       Md,
                       System.Null_Address);
    Ctx.Initialized := True;
  end Init;

  -- Update the context with some text
  -- May raise Status_Error if Ctx is not init or finalized
  procedure Update (Ctx : in out Context; Text : in String) is
    Len : constant C_Types.Size_T := C_Types.Size_T (Text'Length);
  begin
    -- Check that Ctx is initialized
    if not Ctx.Initialized then
      raise Status_Error;
    end if;
    Evp_Digestupdate (Ctx.Evp_Md_Ctx_Acc.all'Address,
                      Text(Text'First)'Address,
                      Len);
  end Update;

  procedure Update (Ctx : in out Context; Bytes : in Byte_Array) is
    Len : constant C_Types.Size_T := C_Types.Size_T (Bytes'Length);
  begin
    -- Check that Ctx is initialized
    if not Ctx.Initialized then
      raise Status_Error;
    end if;
    Evp_Digestupdate (Ctx.Evp_Md_Ctx_Acc.all'Address,
                      Bytes(Bytes'First)'Address,
                      Len);
  end Update;

  -- Finalize the context and get the digest
  -- May raise Status_Error if Ctx is not init or finalized
  procedure Free is new Ada.Unchecked_Deallocation
      (Evp_Md_Ctx, Evp_Md_Ctx_Access);
  function Final (Ctx : in out Context) return Byte_Array is
    Md : Byte_Array (1 .. Evp_Max_Md_Size);
    Len : C_Types.Uint32;
  begin
    -- Check that Ctx is initialized
    if not Ctx.Initialized then
      raise Status_Error;
    end if;
    -- Get Md
    Evp_Digestfinal_Ex (Ctx.Evp_Md_Ctx_Acc.all'Address,
                        Md(Md'First)'Address,
                        Len'Address);
    -- Free resources
    Finalize (Ctx);

    -- Return valid part
    return Md (Md'First .. Md'First + Integer (Len) - 1);
  end Final;

  -- Finalization
  procedure Finalize (Ctx : in out Context) is
  begin
    if Ctx.Initialized then
      Evp_Md_Ctx_Cleanup (Ctx.Evp_Md_Ctx_Acc.all'Address);
      Free (Ctx.Evp_Md_Ctx_Acc);
      Ctx.Initialized := False;
    end if;
  end Finalize;

end Evp_Digest;

