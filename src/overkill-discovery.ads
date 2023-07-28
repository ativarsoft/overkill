with Overkill.Plugin.Input;
use Overkill.Plugin.Input;
with Ada.Finalization;

package Overkill.Discovery is

   type Discovery_Type is new Ada.Finalization.Controlled with null record;

   type Discovery_Access is access all Discovery_Type;

   function New_Discovery
      return Discovery_Type;

   overriding procedure Finalize
      (Discovery : in out Discovery_Type);

   function Lookup_In_Plugin
      (Discovery : in out Discovery_Type;
       Filename : String)
       return In_Plugin_Access;

end Overkill.Discovery;
