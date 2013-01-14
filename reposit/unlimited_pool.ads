-- Generic simple dynamic pool of (non limited) private objects
-- Simply push data in pool, and pop data as long as it is not empty
-- Default policy is Lifo (Last In First Out)
--  but Fifo (First In First Out) is also possible
with Limited_Pool;
generic
  type Data_Type is private;
  Lifo : Boolean := True;
package Unlimited_Pool is

  procedure Set (To : out Data_Type; Val : in Data_Type);

  package Upool is new Limited_Pool (Data_Type, Lifo, Set);

end Unlimited_Pool;

