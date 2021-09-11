with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C.Strings;

package out_plugin is

   type Null_Record is null record;
   type Window is access Null_Record;

   type Out_Configure_Func is access procedure (parent : Window);
   pragma Convention (Stdcall, Out_Configure_Func);
   type Out_About_Func is access procedure (parent : Window);
   pragma Convention (Stdcall, Out_About_Func);
   type Out_Init_Func is access procedure;
   pragma Convention (Stdcall, Out_Init_Func);
   type Out_Quit_Func is access procedure;
   pragma Convention (Stdcall, Out_Quit_Func);
   type Out_Open_Func is access function (rate : int; channels : int; bits : int; a : int; b : int) return int;
   pragma Convention (Stdcall, Out_Open_Func);
   type Out_Close_Func is access procedure;
   pragma Convention (Stdcall, Out_Close_Func);
   type Out_Write_Func is access function (buffer : char_array_access; length : int) return int;
   pragma Convention (Stdcall, Out_Write_Func);
   type Out_Can_Write_Func is access function return int;
   pragma Convention (Stdcall, Out_Can_Write_Func);
   type Out_Is_Playing_Func is access function return int;
   pragma Convention (Stdcall, Out_Is_Playing_Func);
   type Out_Pause_Func is access function return int;
   pragma Convention (Stdcall, Out_Pause_Func);
   type Out_Set_Volume_Func is access procedure (volume : int);
   pragma Convention (Stdcall, Out_Set_Volume_Func);
   type Out_Set_Panning_Func is access procedure (pan : int);
   pragma Convention (Stdcall, Out_Set_Panning_Func);
   type Out_Flush_Func is access procedure;
   pragma Convention (Stdcall, Out_Flush_Func);
   type Out_Get_Output_Time_Func is access function return int;
   pragma Convention (Stdcall, Out_Get_Output_Time_Func);
   type Out_Get_Written_Time_Func is access function return int;
   pragma Convention (Stdcall, Out_Get_Written_Time_Func);
   
   type Out_Plugin is record
      Version : int;
      Description : chars_ptr;
      Id : int;
      Configure : Out_Configure_Func;
      About : Out_About_Func;
      Init : Out_Init_Func;
      Quit : Out_Quit_Func;
      Open : Out_Open_Func;
      Close : Out_Close_Func;
      Write : Out_Write_Func;
      Can_Write : Out_Can_Write_Func;
      Is_Playing : Out_Is_Playing_Func;
      Pause : Out_Pause_Func;
      Set_Volume : Out_Set_Volume_Func;
      Set_Panning : Out_Set_Panning_Func;
      Flush : Out_Flush_Func;
      Get_Output_Time : Out_Get_Output_Time_Func;
      Get_Written_Time : Out_Get_Written_Time_Func;
   end record;
   
   type Get_Out_Module_Func is access function return Out_Plugin;

end out_plugin;
