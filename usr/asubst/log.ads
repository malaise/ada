with Trace;
use Trace;
package Log is
  -- Log a message with default logger
  procedure Def (Message : in String; Severity : Severities := Debug);
  -- Log a message with substit logger
  procedure Sub (Message : in String; Severity : Severities := Debug);
  -- Log a message with search pattern logger
  procedure Sea (Message : in String; Severity : Severities := Debug);
  -- Log a message with replace pattern logger
  procedure Rep (Message : in String; Severity : Severities := Debug);

  -- Is default logger Debug active?
  function Def_Debug return Boolean;
  -- Is substit logger Debug active?
  function Sub_Debug return Boolean;
  -- Is search logger Debug active?
  function Sea_Debug return Boolean;
  -- Is replace logger Debug active?
  function Rep_Debug return Boolean;
end Log;

