with Ada.Finalization;
with Overkill.GUI;
use Overkill.GUI;
with Overkill.Discovery;
use Overkill.Discovery;

package Overkill.Classic with
SPARK_Mode => On
is

   procedure New_Classic
     (Skin : in out Skin_Type;
      GUI : Gui_Dispatch);

   procedure Run
     (Skin : in out Skin_Type;
      Discovery : Discovery_Access);

   --overriding procedure Finalize
   --  (Skin : in out Classic_Skin_Type);

end Overkill.Classic;
