-- Exclusive access to data in a pool
generic
  type Key_Type is private;
package Control_Pool is

  -- Get exclusive access to a data
  --  If delay is negative, wait until access is granted
  --  If delay is null, try and give up if not free
  --  If delay is positive, try during the specified delay
  function Get (Key : Key_Type;
                Waiting_Time : Duration) return Boolean;

  -- Release access to data
  -- Key_Not_Got is raised if the access was not got
  procedure Release (Key : in Key_Type);
  Key_Not_Got : exception;

end Control_Pool;

