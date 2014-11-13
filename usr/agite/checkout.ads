with Git_If;
package Checkout is


  -- Checkout a commit or tag. True if success
  function Handle (Root : String;
                   Info : String;
                   Text : String;
                   Hash : Git_If.Git_Hash) return Boolean;

end Checkout;

