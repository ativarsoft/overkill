with Ada.Containers;
use Ada.Containers;
with Ada.Containers.Vectors;
with Overkill.Plugin;
use Overkill.Plugin;
with Overkill.Plugin.Input;
use Overkill.Plugin.Input;

package Overkill.Plugin.W32 is

   package In_Plugin_Vectors is new Ada.Containers.Vectors
      (Count_Type, In_Plugin_Access);

   type W32_Plugin_Manager_Type is new Plugin_Manager_Type with record
      V : In_Plugin_Vectors.Vector;
   end record;
   
   overriding procedure Load_Input_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type);
   
   overriding procedure Load_Output_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type;
      Selected : Boolean);
   
   overriding procedure Load_General_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type);
   
   overriding procedure Load_DSP_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type);
   
   overriding procedure Load_Visualization_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type);
   
   overriding procedure Load_Encoder_Plugin
     (Plugin_Manager : in out W32_Plugin_Manager_Type;
      Library : Library_Type);

   function Lookup_In_Plugin
      (Plugin_Manager : in out W32_Plugin_Manager_Type;
       Filename : String)
       return In_Plugin_Access;

end Overkill.Plugin.W32;

