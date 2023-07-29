with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C.Strings;
with Overkill.Platform;
use Overkill.Platform;
with Overkill.Plugin.Common;
use Overkill.Plugin.Common;
with System;

package Overkill.Plugin.Output is

   pragma Elaborate_Body;

   type Configure_Type is access procedure (parent : Window_Type);
   type About is access procedure(parent : Window_Type);

   type Init_Type is access procedure;
   type Quit_Type is access procedure;

   type Open_Type is access function (rate : int; channels : int; bits : int; a : int; b : int) return int;
   type Close_Type is access procedure;

   type Write_Type is access function (buffer : chars_ptr; length : int) return int;
   type Can_Write_Type is access function return int;
   type Is_Playing_Type is access function return int;
   type Pause_Type is access function (pause : int) return int;

   type Set_Volume_Type is access procedure (volume : int);
   pragma Convention (Stdcall, Set_Volume_Type);
   type Set_Panning_Type is access procedure (pan : int);
   pragma Convention (Stdcall, Set_Panning_Type);

   type Flush_Type is access procedure (time : int);
   pragma Convention (Stdcall, Flush_Type);

   type Get_Output_Time_Type is access function return int;
   pragma Convention (Stdcall, Get_Output_Time_Type);
   type Get_Written_Time_Type is access function return int;
   pragma Convention (Stdcall, Get_Written_Time_Type);

   type Out_Plugin_Type is record
      Version : int;
      Description : chars_ptr;
      ID : int;

      Window : Window_Type;
      Instance : Library_Type;

      Configure : Configure_Type;
      About : About_Type;

      Init : Init_Type;
      Quit : Quit_Type;

      Open : Open_Type;
      Close : Close_Type;

      Write : Write_Type;
      Can_Write : Can_Write_Type;
      Is_Playing : Is_Playing_Type;
      Pause : Pause_Type;

      Set_Volume : Set_Volume_Type;
      Set_Panning : Set_Panning_Type;

      Flush : Flush_Type;

      Get_Output_Time : Get_Output_Time_Type;
      Get_Written_Time : Get_Written_Time_Type;
   end record;

   type Out_Plugin_Access is access all Out_Plugin_Type;

   Out_Module : System.Address;

end Overkill.Plugin.Output;
