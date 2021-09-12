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
   Skin : Classic_Skin_Type;
   Discovery : Discovery_Type;
   Tray : Tray_Type;
begin
   Overkill.Gui.gui := w32_gui;
   Overkill.Gui.gui.init.all;
   New_Classic (Skin, Overkill.Gui.gui);
   --Tray.Init;
   --Menus.Init;
   Discovery.New_Discovery; -- Must be called after New_Classic
   --Tray.Show;
   Skin.Run;
   --Menus.Quit;
   --Tray.Quit;
   gui.gui.quit.all;
end Overkill.Main;

