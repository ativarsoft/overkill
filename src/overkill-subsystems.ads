with Overkill.Discovery;
use Overkill.Discovery;
with Overkill.Tray;
use Overkill.Tray;
with Overkill.Plugin.W32;
use Overkill.Plugin.W32;

package Overkill.Subsystems is

   pragma Elaborate_Body;

   Plugin_Manager : aliased W32_Plugin_Manager_Type;
   Discovery : aliased Discovery_Type;
   Tray : aliased Tray_Type;

end Overkill.Subsystems;
