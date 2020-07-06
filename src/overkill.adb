with Classic;
with Gui;
use Gui;
with Tray;
with Menus;
with Discovery;

-- NOTE: Quick hack since this is the only platform supported for now.
with w32;

procedure Overkill is
begin
   gui.gui := w32.w32_gui;
   gui.gui.init.all;
   Classic.Init;
   Tray.Init;
   Menus.Init;
   Discovery.Init;
   gui.gui.event_handler.all;
   Discovery.Quit;
   Menus.Quit;
   Tray.Quit;
   Classic.Quit;
   gui.gui.quit.all;
end Overkill;

