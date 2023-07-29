with Overkill.Playback;
with Overkill.Classic;
use Overkill.Classic;
with Overkill.Gui;
use Overkill.Gui;
with Overkill.Menus;
with Overkill.Debug;
use Overkill.Debug;

with Overkill.Subsystems;
use Overkill.Subsystems;
with Overkill.Discovery;

-- NOTE: Must include all platforms (e.g. Windows, X11, etc.)
with Overkill.Gui.W32;
use Overkill.Gui.W32;

with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C.Strings;
with System;
use System;
with Overkill.Subsystems;
use Overkill.Subsystems;

procedure Overkill.Main
is
begin
   Overkill.Playback.Initialize;
   Overkill.Gui.gui := w32_gui;
   Overkill.Gui.gui.init.all;
   --Overkill.Gui.Current_Skin := Skin'Access;
   New_Classic (Current_Skin.all, Overkill.Gui.gui);
   --main_window := Current_Skin.main_window;
   --Tray.Init;
   --Menus.Init;
   Overkill.Subsystems.Discovery :=
      Overkill.Discovery.New_Discovery; -- Must be called after New_Classic
   --Tray.Show;
   Run (Current_Skin.all,
      Overkill.Subsystems.Discovery'Access);
   --Menus.Quit;
   --Tray.Quit;
   gui.gui.quit.all;
end Overkill.Main;

