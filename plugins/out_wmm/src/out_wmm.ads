with binding;
use binding;
with out_plugin;
use out_plugin;

package out_wmm is
   
   --procedure Wmm_Error;
   --procedure Callback;
   
   procedure WMM_Configure(parent : Window);
   procedure WMM_About(parent : Window);
   procedure WMM_Init;
   procedure WMM_Quit;
   function WMM_Open(rate : Integer; channels : Integer; bits : Integer; a : Integer; b : Integer) return Integer;
   procedure WMM_Close;
   function WMM_Write(buffer : String; length : Integer) return Integer;
   function WMM_Can_Write return Integer;
   function WMM_Is_Playing return Integer;
   function WMM_Pause return Integer;
   procedure WMM_Set_Volume(volume : Integer);
   procedure WMM_Set_Panning(pan : Integer);
   procedure WMM_Flush;
   function WMM_Get_Output_Time return Integer;
   function WMM_Get_Written_Time return Integer;

end out_wmm;
