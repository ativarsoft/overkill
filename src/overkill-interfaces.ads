package Overkill.Interfaces is

   pragma Elaborate_Body;

   type Tray_Interface is limited interface;

   procedure Show (Tray : in out Tray_Interface) is abstract;
   procedure Hide (Tray : in out Tray_Interface) is abstract;

end Overkill.Interfaces;
