with Overkill.Classic;
use Overkill.Classic;
with Overkill.Gui;
use Overkill.Gui;
with Overkill.Tray;
use Overkill.Tray;
with Overkill.Menus;
with Overkill.Discovery;
use Overkill.Discovery;
with Overkill.Debug;
use Overkill.Debug;

-- NOTE: Must include all platforms (e.g. Windows, X11, etc.)
with Overkill.Gui.W32;
use Overkill.Gui.W32;

with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C.Strings;
with System;
use System;

procedure Overkill.Main
is
   Discovery : Discovery_Type;
   Tray : Tray_Type;
begin
   Overkill.Gui.gui := w32_gui;
   Overkill.Gui.gui.init.all;
   --Overkill.Gui.Current_Skin := Skin'Access;
   New_Classic (Current_Skin.all, Overkill.Gui.gui);
   --main_window := Current_Skin.main_window;
   --Tray.Init;
   --Menus.Init;
   Discovery.New_Discovery; -- Must be called after New_Classic
   --Tray.Show;
   Run (Current_Skin.all);
   --Menus.Quit;
   --Tray.Quit;
   gui.gui.quit.all;
end Overkill.Main;

