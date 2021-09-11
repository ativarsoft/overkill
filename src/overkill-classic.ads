with Ada.Finalization;
with Overkill.GUI;
use Overkill.GUI;

package Overkill.Classic is

   type Classic_Skin_Type is new Ada.Finalization.Controlled with record
      GUI : Gui_Dispatch;
   end record;

   procedure New_Classic
     (Skin : in out Classic_Skin_Type;
      GUI : Gui_Dispatch);

   overriding procedure Finalize
     (Skin : in out Classic_Skin_Type);

   procedure Run
     (Skin : in out Classic_Skin_Type);
end Overkill.Classic;
