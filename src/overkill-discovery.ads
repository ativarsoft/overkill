with Ada.Finalization;

package Overkill.Discovery is

   type Discovery_Type is new Ada.Finalization.Limited_Controlled with null record;

   procedure New_Discovery
     (Discovery : in out Discovery_Type);

   overriding procedure Finalize
     (Discovery : in out Discovery_Type);

end Overkill.Discovery;
