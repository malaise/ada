-- Binding (thin) to some OpenSSL EVP_Digest function
private with System;
private with Ada.Finalization;
with C_Types;
package Evp_Digest is

  -- An EVP context
  type Context is tagged limited private;

  -- An array of bytes to add to context
  subtype Byte is C_Types.Byte;
  type Byte_Array is array (Positive range <>) of Byte;


  -- Initialize a context for use with a given digest kind
  -- Valid names should be (depending on implementation) the lowercase image of
  type Digest_List is (Md2, Md4, Md5, Mdc2, Dss, Dss1, Ripemd160, Whirlpool,
                       Sha, Sha1, Sha224, Sha256, Sha384, Sha512);
  -- May raise:
  -- If Name does not correspond to a known digest kind (see man EVP_DigestInit)
  Name_Error : exception;
  -- If Ctx is already init (and not reset)
  Status_Error : exception;
  procedure Init (Ctx : in out Context; Digest_Name : in String);
  procedure Init (Ctx : in out Context; Digest : in Digest_List);
  function Is_Init (Ctx : in Context) return Boolean;

  -- Update the context with some text
  -- May raise Status_Error if Ctx is not init or reset
  procedure Update (Ctx : in out Context; Text : in String);
  procedure Update (Ctx : in out Context; Bytes : in Byte_Array);

  -- Read the digest and let it ready for adding new updates
  -- May raise Status_Error if Ctx is not init or reset
  function Read (Ctx : in out Context) return Byte_Array;

  -- Empty the Ctx for new calls to Update
  -- May raise Status_Error if Ctx is not init or reset
  procedure Clean (Ctx : in out Context) ;

  -- Get the digest and empty the Ctx for new calls to Update
  -- May raise Status_Error if Ctx is not init or reset
  function Get (Ctx : in out Context) return Byte_Array;

  -- Reset the context for a new Init
  -- May raise Status_Error if Ctx is not init or already reset
  procedure Reset (Ctx : in out Context);

private

  -- EVP_MAX_MD_SIZE is 64
  -- So 256 leaves some margin
  Evp_Max_Md_Size : constant := 256;

  type Context is new Ada.Finalization.Limited_Controlled with record
    Is_Clean : Boolean := True;
    Evp_Md_Ctx : System.Address := System.Null_Address;
    Evp_Md : System.Address := System.Null_Address;
  end record;

  overriding procedure Finalize (Ctx : in out Context);

end Evp_Digest;

