with Overkill.Plugin;
use Overkill.Plugin;
with Overkill.Plugin.Common;
with Overkill.Plugin.Output;
with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C.Strings;
with System;

package Overkill.Plugin.Input is

   pragma Elaborate_Body;

   type Configure_Type is access procedure (Parent : Window_Type);
   pragma Convention (Stdcall, Configure_Type);
   type About_Type is access procedure (Parent : Window_Type);
   pragma Convention (Stdcall, About_Type);
   type Init_Type is access procedure;
   pragma Convention (Stdcall, Init_Type);
   type Quit_Type is access procedure;
   pragma Convention (Stdcall, Quit_Type);
   type Get_File_Info_Type is access procedure (File : chars_ptr; Title : chars_ptr; Length_In_Ms : System.Address);
   pragma Convention (Stdcall, Get_File_Info_Type);
   type Info_Box_Type is access procedure (File : chars_ptr; Parent : Window_Type);
   pragma Convention (Stdcall, Info_Box_Type);
   type Is_Our_File_Type is access function (Filename : chars_ptr) return int;
   pragma Convention (Stdcall, Is_Our_File_Type);
   type Play_Type is access function (Filename : chars_ptr) return int;
   pragma Convention (Stdcall, Play_Type);
   type Pause_Type is access procedure;
   pragma Convention (Stdcall, Pause_Type);
   type Unpause_Type is access procedure;
   pragma Convention (Stdcall, Unpause_Type);
   type Is_Paused_Type is access procedure;
   pragma Convention (Stdcall, Is_Paused_Type);
   type Stop_Type is access procedure;
   pragma Convention (Stdcall, Stop_Type);
   type Get_Length_Type is access procedure;
   pragma Convention (Stdcall, Get_Length_Type);
   type Get_Output_Time_Type is access procedure;
   pragma Convention (Stdcall, Get_Output_Time_Type);
   type Set_Output_Time_Type is access procedure (Miliseconds : int);
   pragma Convention (Stdcall, Set_Output_Time_Type);
   type Set_Volume_Type is access procedure (Value : int);
   pragma Convention (Stdcall, Set_Volume_Type);
   type Set_Panning_Type is access procedure (Value : int);
   pragma Convention (Stdcall, Set_Panning_Type);

   --
   -- Video Spectrum analyser
   --

   type Sa_Vsa_Init_Type is access procedure (latency : int; rate : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Sa_Vsa_Init_Type);

   type Sa_Vsa_Deinit_Type is access procedure;

   pragma Convention
      (Convention => Stdcall,
       Entity => Sa_Vsa_Deinit_Type);

   type Sa_Add_Pcm_Data_Type is access procedure (pcm_data : System.Address; Num_Channels : int; Bps : int; Time : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Sa_Add_Pcm_Data_Type);

   type Sa_Get_Mode_Type is access function return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Sa_Get_Mode_Type);

   type Sa_Add_Type is access function (data : System.Address; Time : int; T : int) return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Sa_Add_Type);

   type Vsa_Add_Pcm_Data_Type is access procedure (A : System.Address; B : int; C : int; D : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Vsa_Add_Pcm_Data_Type);

   type Vsa_Get_Mode_Type is access function (A : int; B : int) return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Vsa_Get_Mode_Type);

   type Vsa_Add_Type is access procedure (A : System.Address; B : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Vsa_Add_Type);

   type Vsa_Set_Info_Type is access procedure (A : int; B : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Vsa_Set_Info_Type);

   type Dsp_Is_Active_Type is access function return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Dsp_Is_Active_Type);

   type Do_Samples_Type is access function (Samples : Pcm_Data_Type; Num : int; Bits : int; Channels : int; Rate : int) return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Do_Samples_Type);

   --
   -- Equalizer
   --

   type Eq_Arr is new String (1 .. 10);
   type Eq_Set_Type is access procedure (A : int; B : System.Address; C : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Eq_Set_Type);

   type Set_Info_Type is access procedure (A, B, C, D : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Set_Info_Type);

   --
   --  Module type
   --

   type In_Plugin_Type is record
      Version : int; -- out
      Description : chars_ptr; -- out
      Window : Window_Type;
      Instance : Library_Type;
      File_Ext : chars_ptr;
      Is_Seekable : int;
      Flags : int;
      Configure : Configure_Type;
      About : About_Type;
      Init : Init_Type;
      Quit : Quit_Type;
      Get_File_Info : Get_File_Info_Type;
      Info_Box : Info_Box_Type;
      Is_Our_File : Is_Our_File_Type;
      Play : Play_Type;
      Pause : Pause_Type;
      Unpause : Unpause_Type;
      Is_Paused : Is_Paused_Type;
      Stop : Stop_Type;
      Get_Length : Get_Length_Type;
      Get_Output_Time : Get_Output_Time_Type;
      Set_Output_Time : Set_Output_Time_Type;
      Set_Volume : Set_Volume_Type;
      Set_Panning : Set_Panning_Type;
      --  Video Spectrum analyser
      Sa_Vsa_Init : Sa_Vsa_Init_Type;
      Sa_Vsa_Deinit : Sa_Vsa_Deinit_Type;
      Sa_Add_Pcm_Data : Sa_Add_Pcm_Data_Type;
      Sa_Get_Mode : Sa_Get_Mode_Type;
      Sa_Add : Sa_Add_Type;
      Vsa_Add_Pcm_Data : Vsa_Add_Pcm_Data_Type;
      Vsa_Get_Mode : Vsa_Get_Mode_Type;
      Vsa_Add : Vsa_Add_Type;
      Vsa_Set_Info : Vsa_Set_Info_Type;
      Dsp_Is_Active : Dsp_Is_Active_Type;
      Do_Samples : Do_Samples_Type;
      --  Equalizer
      Eq_Set : Eq_Set_Type;
      Set_Info : Set_Info_Type;
      --  Output plugin module
      Out_Module : System.Address;
   end record;

   type In_Plugin_Access is access all In_Plugin_Type;

end Overkill.Plugin.Input;

