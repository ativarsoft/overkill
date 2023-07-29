with Overkill.Plugin.Input;
use Overkill.Plugin.Input;
with Ada.Finalization;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

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

   Selected_Out_Module_Filename : Unbounded_String :=
      To_Unbounded_String ("OUT_WAVE.DLL");

end Overkill.Discovery;
