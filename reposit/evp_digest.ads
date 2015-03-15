-- Binding (thin) to some OpenSSL EVP_Digest function
with Ada.Finalization;
with C_Types;
package Evp_Digest is

  -- An EVP context
  type Context is tagged limited private;

  -- An array of bytes to add to context
  subtype Byte is C_Types.Byte;
  type Byte_Array is array (Positive range <>) of Byte;


  -- Initialize a context for use with a given digest kind
  -- Valid names should be at least (demending on implementation):
  --  md4 md5 sha sha1 dss1 sha224 sha256 sha384 sha512 ripemd160 whirlpool
  -- May raise:
  -- If Name does not correspond to a known digest kind (see man EVP_DigestInit)
  Name_Error : exception;
  -- If Ctx is already init (and not finalized)
  Status_Error : exception;
  procedure Init (Ctx : in out Context; Name : in String);

  -- Update the context with some text
  -- May raise Status_Error if Ctx is not init or finalized
  procedure Update (Ctx : in out Context; Text : in String);
  procedure Update (Ctx : in out Context; Bytes : in Byte_Array);

  -- Finalize the context and get the digest
  -- May raise Status_Error if Ctx is not init or finalized
  function Final (Ctx : in out Context) return Byte_Array;

private

  -- Struct env_md_ctx_st is 5 xxx* and a long
  -- So 21 Uint64 leaves some margin
  type Evp_Md_Ctx is array (1 .. 21) of C_Types.Uint64;
  type Evp_Md_Ctx_Access is access Evp_Md_Ctx;

  -- EVP_MAX_MD_SIZE is 64
  -- So 256 leaves some margin
  Evp_Max_Md_Size : constant := 256;

  type Context is new Ada.Finalization.Limited_Controlled with record
    Initialized : Boolean := False;
    Evp_Md_Ctx_Acc : Evp_Md_Ctx_Access := null;
  end record;

  procedure Finalize (Ctx : in out Context);

end Evp_Digest;

