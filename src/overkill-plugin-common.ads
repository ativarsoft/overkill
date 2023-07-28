package Overkill.Plugin.Common is

   pragma Elaborate_Body;

   type Configure_Type is access procedure;
   type About_Type is access procedure (hwndParent : Window_Type);
   type Init_Type is access procedure;
   type Quit_Type is access procedure;

end Overkill.Plugin.Common;

