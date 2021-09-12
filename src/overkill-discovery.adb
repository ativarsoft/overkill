with Overkill.Debug;
use Overkill.Debug;
with Ada.Strings.Equal_Case_Insensitive;
use Ada.Strings;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Directories;
use Ada.Directories;
with Overkill.Plugin;
use Overkill.Plugin;
with Overkill.Plugin.Output;
with Overkill.Platform;
use Overkill.Platform;
with Ada.Command_Line;
with Overkill.Plugin.W32;
use Overkill.Plugin.W32;
with Interfaces.C.Strings;
use Interfaces.C.Strings;
with Ada.Exceptions;
with Interfaces.C;
use Interfaces.C;

package body Overkill.Discovery is
   
   LOAD_LIBRARY_EXCEPTION : exception;
   
   --
   -- Types
   --
   
   type In_Plugin_Type is null record;
   type Out_Plugin_Type is null record;
   type Gen_Plugin_Type is null record;
   type DSP_Plugin_Module_Type is null record;
   type Vis_Plugin_Module_Type is null record;
   
   --
   -- Variables
   --
   
   Plugin_Manager : W32_Plugin_Manager_Type;
   
   --
   -- Functions
   --
   
   type DWORD is new Interfaces.C.unsigned;
   
   DONT_RESOLVE_DLL_REFERENCES : constant := 16#00000001#;
   LOAD_LIBRARY_AS_DATAFILE : constant := 16#00000002#;
   
   function LoadLibraryExA
     (lpLibFileName : chars_ptr;
      hFile : access Null_Record;
      dwFlags : DWORD)
      return Library_Type;
   pragma Import (Stdcall, LoadLibraryExA, "LoadLibraryExA");
   
   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");
   
   function Load_Library
     (Path : String)
      return Library_Type
   is
      Library : Library_Type;
      RC : Natural;
   begin
      -- If references are not resolved, external functions like MessageBox will
      -- cause the program to crash
      --Library := LoadLibraryExA (New_String (Path), null, DONT_RESOLVE_DLL_REFERENCES);
      Library := LoadLibraryExA (New_String (Path), null, 0);
      if Library = null then
         RC := Natural (GetLastError);
         raise Program_Error with "Failed to load library " & Path & ". GetLastError is returning " & RC'Image & ".";
      end if;
      return Library;
   end Load_Library;
   
   procedure Load_Plugin
     (Path : String;
      Filename : String)
   is
      In_Plugin : In_Plugin_Type;
      Out_Plugin : Out_Plugin_Type;
      Gen_Plugin : Gen_Plugin_Type;
      Dsp_Plugin : DSP_Plugin_Module_Type;
      Vis_Plugin : Vis_Plugin_Module_Type;
      Library : Library_Type;
   begin
      if Equal_Case_Insensitive (Filename (Filename'First..Filename'First+2), "in_") then
         Put_Line ("Loading in plugin.");
         Library := Load_Library (Path);
         Load_Input_Plugin (Plugin_Manager, Library);
      elsif Equal_Case_Insensitive (Filename (Filename'First..Filename'First+4), "out_") then
         Library := Load_Library (Path);
         Load_Output_Plugin (Plugin_Manager, Library);
      elsif Equal_Case_Insensitive (Filename (Filename'First..Filename'First+4), "gen_") then
         Library := Load_Library (Path);
         Load_General_Plugin (Plugin_Manager, Library);
      elsif Equal_Case_Insensitive (Filename (Filename'First..Filename'First+4), "dsp_") then
         Library := Load_Library (Path);
         Load_DSP_Plugin (Plugin_Manager, Library);
      elsif Equal_Case_Insensitive (Filename (Filename'First..Filename'First+4), "vis_") then
         Library := Load_Library (Path);
         Load_Visualization_Plugin (Plugin_Manager, Library);
      elsif Equal_Case_Insensitive (Filename (Filename'First..Filename'First+4), "enc_") then
         Library := Load_Library (Path);
         Load_Encoder_Plugin (Plugin_Manager, Library);
      end if;
   exception
      when LOAD_LIBRARY_EXCEPTION =>
         Put_Line ("Error loading library.");
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
         return;
   end Load_Plugin;
   
   procedure Traverse_Dir
     (Directory : String)
   is
      Pattern   : String := "*.dll";
      Search  : Search_Type;
      Dir_Ent : Directory_Entry_Type;
   begin
      Start_Search (Search, Directory, Pattern);
 
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Dir_Ent);
         Put_Line ("Loading " & Simple_Name (Dir_Ent));
         Load_Plugin (Full_Name (Dir_Ent), Simple_Name (Dir_Ent));
      end loop;
 
      End_Search (Search);
   end Traverse_Dir;
   
   procedure New_Discovery
     (Discovery : in out Discovery_Type)
   is
      use Ada.Command_Line;
      Plugin_Dir : String := Compose
        (Containing_Directory => Containing_Directory (Command_Name),
         Name => "plugins",
         Extension => "");
   begin
      Traverse_Dir (Plugin_Dir);
   end New_Discovery;
   
   procedure Finalize
     (Discovery : in out Discovery_Type)
   is
   begin
      null;
   end Finalize;

end Overkill.Discovery;
