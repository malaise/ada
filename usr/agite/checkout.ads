with Git_If;
package Checkout is


  -- Checkout a commit or tag. True if success
  function Handle (Root : String;
                   Text : String;
                   Hash : Git_If.Git_Hash;
                   Is_Commit : Boolean) return Boolean;

end Checkout;

