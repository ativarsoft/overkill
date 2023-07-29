package Overkill.Plugin.Common is

   pragma Elaborate_Body;

   type Configure_Type is access procedure;
   pragma Convention (Stdcall, Configure_Type);
   type About_Type is access procedure (hwndParent : Window_Type);
   pragma Convention (Stdcall, About_Type);
   type Init_Type is access procedure;
   pragma Convention (Stdcall, Init_Type);
   type Quit_Type is access procedure;
   pragma Convention (Stdcall, Quit_Type);

end Overkill.Plugin.Common;

