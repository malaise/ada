with Data_Base;
package Alias is

  -- Resolves an alias
  -- If item kind is alias, return it unchanged, otherwise
  -- Look for an alias for name
  --   extract word (see Names) and concat to previous words
  --   look for such alias
  --   until alias found or no more words 
  -- Done when no more alias can be found
  -- Set the item name to the result,
  -- No_Item if a loop of alias is detected
  --  or if temporary name becomes too long
  procedure Resolve (Item : in out Data_Base.Item_Rec);

end Alias;

