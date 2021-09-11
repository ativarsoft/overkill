with out_plugin;
use out_plugin;

package body out_wmm is
   
   procedure WMM_Configure(parent : Window) is
   begin
      null;
   end WMM_Configure;
   
   procedure WMM_About(parent : Window) is
   begin
      null;
   end WMM_About;
   
   procedure WMM_Init is
   begin
      null;
   end WMM_Init;
   
   procedure WMM_Quit is
   begin
      null;
   end WMM_Quit;
   
   function WMM_Open(rate : Integer; channels : Integer; bits : Integer; a : Integer; b : Integer) return Integer is
   begin
      return 0;
   end WMM_Open;
   
   procedure WMM_Close is
   begin
      null;
   end WMM_Close;
   
   function WMM_Write(buffer : String; length : Integer) return Integer is
   begin
      return 0;
   end WMM_Write;
   
   function WMM_Can_Write return Integer is
   begin
      return 0;
   end WMM_Can_Write;
   
   function WMM_Is_Playing return Integer is
   begin
      return 0;
   end WMM_Is_Playing;
   
   function WMM_Pause return Integer is
   begin
      return 0;
   end WMM_Pause;
   
   procedure WMM_Set_Volume(volume : Integer) is
   begin
      null;
   end WMM_Set_Volume;
   
   procedure WMM_Set_Panning(pan : Integer) is
   begin
      null;
   end WMM_Set_Panning;
   
   procedure WMM_Flush is
   begin
      null;
   end WMM_Flush;
   
   function WMM_Get_Output_Time return Integer is
   begin
      return 0;
   end WMM_Get_Output_Time;
   
   function WMM_Get_Written_Time return Integer is
   begin
      return 0;
   end WMM_Get_Written_Time;

end out_wmm;
