with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C.Strings;
with Overkill.Platform;
use Overkill.Platform;

package Overkill.Plugin is

   pragma Elaborate_Body;
   
   type Pcm_Data_Type is access unsigned_short;
   
   type Plugin_Manager_Type is interface;

   procedure Load_Input_Plugin
     (Plugin_Manager : in out Plugin_Manager_Type;
      Library : Library_Type)
   is abstract;

   procedure Load_Output_Plugin
     (Plugin_Manager : in out Plugin_Manager_Type;
      Library : Library_Type)
   is abstract;
   
   procedure Load_General_Plugin
     (Plugin_Manager : in out Plugin_Manager_Type;
      Library : Library_Type)
   is abstract;
   
   procedure Load_DSP_Plugin
     (Plugin_Manager : in out Plugin_Manager_Type;
      Library : Library_Type)
   is abstract;
   
   procedure Load_Visualization_Plugin
     (Plugin_Manager : in out Plugin_Manager_Type;
      Library : Library_Type)
   is abstract;
   
   procedure Load_Encoder_Plugin
     (Plugin_Manager : in out Plugin_Manager_Type;
      Library : Library_Type)
   is abstract;

end Overkill.Plugin;
