with System;

package Overkill.Platform is

   type Null_Record is null record;
   
   type Library_Type is access Null_Record;
   type Window_Type is new System.Address;

end Overkill.Platform;
