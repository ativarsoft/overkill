with Overkill.Gui;
use Overkill.Gui;
with Overkill.Debug;
use Overkill.Debug;
with Interfaces.C.Strings;
use Interfaces.C.Strings;
with Ada.Exceptions;
with Overkill.Plugin.Input;
use Overkill.Plugin.Input;
with Overkill.Plugin.Output;
use Overkill.Plugin.Output;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Characters.Handling;
with System;

package body Overkill.Plugin.W32 is

   type Get_In_Module_Type is access function return In_Plugin_Access;

   pragma Convention
      (Convention => Stdcall,
       Entity => Get_In_Module_Type);

   type Get_Out_Module_Type is access function return Out_Plugin_Access;

   pragma Convention
      (Convention => Stdcall,
       Entity => Get_Out_Module_Type);
   
   type DWORD is new Interfaces.C.unsigned;
   
   function GetProcAddress
     (Module_Handle : Library_Type;
      lpProcName : System.Address)
      return Get_In_Module_Type;
   function GetProcAddress
     (Module_Handle : Library_Type;
      lpProcName : System.Address)
      return Get_Out_Module_Type;
   pragma Import (Stdcall, GetProcAddress, "GetProcAddress");
   
   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");
   
   function Get_In_Symbol
     (Library : Library_Type;
      Symbol : String)
      return Get_In_Module_Type
   is
      C_Symbol : char_array := To_C (Symbol);
      Get_In_Module : Get_In_Module_Type;
      RC : DWORD;
   begin
      Get_In_Module := GetProcAddress (Library, C_Symbol'Address);
      if Get_In_Module = null then
         RC := GetLastError;
         raise Program_Error with "Failed to get symbol " & Symbol & ". GetLastError returned " & RC'Image & ".";
      end if;
      return Get_In_Module;
   end Get_In_Symbol;
   
   function Get_Out_Symbol
     (Library : Library_Type;
      Symbol : String)
      return Get_Out_Module_Type
   is
      C_Symbol : char_array := To_C (Symbol);
      Get_Out_Module : Get_Out_Module_Type;
      RC : DWORD;
   begin
      Get_Out_Module := GetProcAddress (Library, C_Symbol'Address);
      if Get_Out_Module = null then
         RC := GetLastError;
         raise Program_Error with "Failed to get symbol " & Symbol & ". GetLastError returned " & RC'Image & ".";
      end if;
      return Get_Out_Module;
   end Get_Out_Symbol;

   procedure Sa_Vsa_Init
      (latency : int; rate : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Sa_Vsa_Init);

   procedure Sa_Vsa_Init
     (latency : int; rate : int)
   is
   begin
      null;
   end Sa_Vsa_Init;

   procedure Sa_Vsa_Deinit;

   pragma Convention
      (Convention => Stdcall,
       Entity => Sa_Vsa_Deinit);

   procedure Sa_Vsa_Deinit
   is
   begin
      null;
   end Sa_Vsa_Deinit;
   
   procedure Sa_Add_Pcm_Data
     (pcm_data : Pcm_Data_Type;
      Num_Channels : int;
      Bps : int;
      Time : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Sa_Add_Pcm_Data);

   procedure Sa_Add_Pcm_Data
     (pcm_data : Pcm_Data_Type;
      Num_Channels : int;
      Bps : int;
      Time : int)
   is
   begin
      null;
   end Sa_Add_Pcm_Data;

   function Sa_Get_Mode
     return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Sa_Get_Mode);

   function Sa_Get_Mode
     return int
   is
   begin
      return 0;
   end Sa_Get_Mode;
   
   function Sa_Add
     (data : Pcm_Data_Type;
      Time : int;
      T : int)
      return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Sa_Add);

   function Sa_Add
     (data : Pcm_Data_Type;
      Time : int;
      T : int)
      return int
   is
   begin
      return 0;
   end Sa_Add;

   procedure Vsa_Add_Pcm_Data
     (A : access Null_Record;
      B : int;
      C : int;
      D : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Vsa_Add_Pcm_Data);

   procedure Vsa_Add_Pcm_Data
     (A : access Null_Record;
      B : int;
      C : int;
      D : int)
   is
   begin
      null;
   end Vsa_Add_Pcm_Data;
   
   function Vsa_Get_Mode
     (A : int;
      B : int)
      return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Vsa_Get_Mode);

   function Vsa_Get_Mode
     (A : int;
      B : int)
      return int
   is
   begin
      return 0;
   end Vsa_Get_Mode;

   function Vsa_Add
     (A : access Null_Record;
      B : int)
      return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Vsa_Add);

   function Vsa_Add
     (A : access Null_Record;
      B : int)
      return int
   is
   begin
      return 0;
   end Vsa_Add;
   
   procedure Vsa_Set_Info
     (A : int;
      B : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Vsa_Set_Info);

   procedure Vsa_Set_Info
     (A : int;
      B : int)
   is
   begin
      null;
   end Vsa_Set_Info;
   
   function Dsp_Is_Active
     return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Dsp_Is_Active);

   function Dsp_Is_Active
     return int
   is
   begin
      return 1;
   end Dsp_Is_Active;
   
   function Do_Samples
     (Samples : Pcm_Data_Type;
      Num : int;
      Bits : int;
      Channels : int;
      Rate : int)
      return int;

   pragma Convention
      (Convention => Stdcall,
       Entity => Do_Samples);

   function Do_Samples
     (Samples : Pcm_Data_Type;
      Num : int;
      Bits : int;
      Channels : int;
      Rate : int)
      return int
   is
   begin
      return 576;
   end Do_Samples;
   
   procedure Set_Info
     (A, B, C, D : int);

   pragma Convention
      (Convention => Stdcall,
       Entity => Set_Info);

   procedure Set_Info
     (A, B, C, D : int)
   is
   begin
      null;
   end Set_Info;

   overriding procedure Load_Input_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type)
   is
      Get_In_Module : Get_In_Module_Type;
      In_Module : In_Plugin_Access;
   begin
      Get_In_Module := Get_In_Symbol (Library, "winampGetInModule2");
      if Get_In_Module = null then
         return;
      end if;
      In_Module := Get_In_Module.all;
      
      In_Module.Window := main_window;
      In_Module.Instance := Library;
      In_Module.Sa_Vsa_Init := Sa_Vsa_Init'Access;
      In_Module.Sa_Vsa_Deinit := Sa_Vsa_Deinit'Access;
      In_Module.Sa_Add_Pcm_Data := Sa_Add_Pcm_Data'Access;
      In_Module.Sa_Get_Mode := Sa_Get_Mode'Access;
      In_Module.Sa_Add := Sa_Add'Access;
      In_Module.Vsa_Add_Pcm_Data := Vsa_Add_Pcm_Data'Access;
      In_Module.Vsa_Get_Mode := Vsa_Get_Mode'Access;
      In_Module.Vsa_Add := Vsa_Add'Access;
      In_Module.Vsa_Set_Info := Vsa_Set_Info'Access;
      In_Module.Dsp_Is_Active := Dsp_Is_Active'Access;
      In_Module.Do_Samples := Do_Samples'Access;
      In_Module.Set_Info := Set_Info'Access;
      In_Module.Init.all;
      Put_Line ("Description: " & To_Ada (Value (In_Module.Description)));
      Put_Line ("Calling about function.");
      In_Module.About.all (main_window);
      Plugin_Manager.V.Append (In_Module);
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Load_Input_Plugin;
   
   procedure Load_Output_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type;
      Selected : Boolean)
   is
      Out_Module : Out_Plugin_Access;
      Get_Out_Module : Get_Out_Module_Type;
   begin
      Get_Out_Module := Get_Out_Symbol (Library, "winampGetOutModule");
      if Get_Out_Module = null then
         return;
      end if;
      Out_Module := Get_Out_Module.all;
      
      Out_Module.Window := main_window;
      Out_Module.Instance := Library;
      
      Out_Module.init.all;
      Out_Module.Set_Volume(255);
      Out_Module.Set_Panning(0);
      
      Put_Line ("Description: " & To_Ada (Value (Out_Module.Description)));

      Overkill.Plugin.Output.Out_Module :=
         Out_Module.all'Address;
   end Load_Output_Plugin;
   
   procedure Load_General_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type)
   is
   begin
      null;
   end Load_General_Plugin;
   
   procedure Load_DSP_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type)
   is
   begin
      null;
   end Load_DSP_Plugin;
   
   procedure Load_Visualization_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type)
   is
   begin
      null;
   end Load_Visualization_Plugin;
   
   procedure Load_Encoder_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type)
   is
   begin
      null;
   end Load_Encoder_Plugin;

   function Lookup_In_Plugin
      (Plugin_Manager : in out W32_Plugin_Manager_Type;
       Filename : String)
       return In_Plugin_Access
   is
      use Ada.Characters.Handling;
      use Ada.Directories;
      use Ada.Text_IO;
      use In_Plugin_Vectors;

      Filename_Extension : String := Extension (To_Lower (Filename));

      C_Filename, C_Filename_Extension : chars_ptr;

      function Check_Extension (Extensions : chars_ptr; Filename_Extension : chars_ptr)
         return int;
      pragma Import
         (Convention => C,
          Entity => Check_Extension,
          External_Name => "check_extension");
   begin
      Put ("Trying to play ");
      Put (Filename);
      Put (".");
      New_Line;

      Put (Plugin_Manager.V.Length'Image);
      Put (" plugins found.");
      New_Line;

      for I in 0 .. Plugin_Manager.V.Length - 1 loop
         Put ("Trying input plugin #");
         Put (I'Image);
         New_Line;

         C_Filename := New_String (Filename);

         if Plugin_Manager.V (I).Is_Our_File (C_Filename) /= 0 then
            Free (C_Filename);
            return Plugin_Manager.V (I);
         end if;

         C_Filename_Extension := New_String (Filename_Extension);
         if Check_Extension
            (Plugin_Manager.V (I).File_Ext, C_Filename_Extension) = 0
         then
            Free (C_Filename);
            Free (C_Filename_Extension);
            return Plugin_Manager.V (I);
         end if;

         Free (C_Filename);
         Free (C_Filename_Extension);
      end loop;
      raise Program_Error with "Unknown file format.";
   end Lookup_In_Plugin;

end Overkill.Plugin.W32;
