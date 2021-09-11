with Ada.Finalization;
use Ada.Finalization;
with Overkill.Interfaces;
use Overkill.Interfaces;

package Overkill.Tray is
   
   type Tray_Type is new Limited_Controlled and Tray_Interface with null record;
   
   overriding procedure Initialize (Tray : in out Tray_Type);
   overriding procedure Finalize (Tray : in out Tray_Type);
   overriding procedure Show (Tray : in out Tray_Type);
   overriding procedure Hide (Tray : in out Tray_Type);
   
end Overkill.Tray;
