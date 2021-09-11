with Ada.Text_IO;

package body Overkill.Debug is

   procedure Put_Line
     (Message : String)
   is
   begin
      if False then
         Ada.Text_IO.Put_Line(Message);
      end if;
   end Put_Line;

end Overkill.Debug;
