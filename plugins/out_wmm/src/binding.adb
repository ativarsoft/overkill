with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C.Strings;
with out_plugin;
use out_plugin;
with out_wmm;
use out_wmm;

package body binding is

   -- These functions are called by the host program
   procedure Configure(parent : Window);
   pragma Convention (Stdcall, Configure);
   procedure About(parent : Window);
   pragma Convention (Stdcall, About);
   procedure Init;
   pragma Convention (Stdcall, Init);
   procedure Quit;
   pragma Convention (Stdcall, Quit);
   function Open(rate : Int; channels : Int; bits : Int; a : Int; b : Int) return Int;
   pragma Convention (Stdcall, Open);
   procedure Close;
   pragma Convention (Stdcall, Close);
   
   function Write(buffer : char_array_access; length : Int) return Int;
   pragma Convention (Stdcall, Write);
   function Can_Write return Int;
   pragma Convention (Stdcall, Can_Write);
   function Is_Playing return Int;
   pragma Convention (Stdcall, Is_Playing);
   function Pause return Int;
   pragma Convention (Stdcall, Pause);
   procedure Set_Volume(volume : Int);
   pragma Convention (Stdcall, Set_Volume);
   procedure Set_Panning(pan : Int);
   pragma Convention (Stdcall, Set_Panning);
   procedure Flush;
   pragma Convention (Stdcall, Flush);
   function Get_Output_Time return Int;
   pragma Convention (Stdcall, Get_Output_Time);
   function Get_Written_Time return Int;
   pragma Convention (Stdcall, Get_Written_Time);
   
   procedure Configure(parent : Window) is
   begin
      WMM_Configure(parent);
   end Configure;
   
   procedure About(parent : Window) is
   begin
      WMM_About(parent);
   end About;
   
   procedure Init is
   begin
      WMM_Init;
   end Init;
   
   procedure Quit is
   begin
      WMM_Quit;
   end Quit;
   
   function Open(rate : Int; channels : Int; bits : Int; a : Int; b : Int) return Int is
      r : Integer;
   begin
      r := WMM_Open(Integer(rate), Integer(channels), Integer(bits), Integer(a), Integer(b));
      return int(r);
   end Open;
   
   procedure Close is
   begin
      null;
   end Close;
   
   function Write(buffer : char_array_access; length : Int) return Int is
      ada_length : Natural := Natural(length);
      ada_buffer : String(0..ada_length-1);
      r : Integer;
   begin
      To_Ada(buffer.all, ada_buffer, ada_length, False);
      r := WMM_Write(ada_buffer, Integer(length));
      return int(r);
   end Write;
   
   function Can_Write return Int is
   begin
      return 0;
   end Can_Write;
   
   function Is_Playing return Int is
   begin
      return 0;
   end Is_Playing;
   
   function Pause return Int is
   begin
      return 0;
   end Pause;
   
   procedure Set_Volume(volume : Int) is
   begin
      null;
   end Set_Volume;
   
   procedure Set_Panning(pan : Int) is
   begin
      null;
   end Set_Panning;
   
   procedure Flush is
   begin
      null;
   end Flush;
   
   function Get_Output_Time return Int is
   begin
      return 0;
   end Get_Output_Time;
   
   function Get_Written_Time return Int is
   begin
      return 0;
   end Get_Written_Time;
   
   out_wmm : out_plugin.Out_Plugin := (
                            version => 16#10#,
                            description => Interfaces.C.Strings.New_Char_Array(To_C("WinMM")),
                            id => 0,
                            configure => Configure'Access,
                            about => About'Access,
                            init => Init'Access,
                            quit => Quit'Access,
                            open => Open'Access,
                            close => Close'Access,
                            write => Write'Access,
                            can_write => Can_Write'Access,
                            is_playing => Is_Playing'Access,
                            pause => Pause'Access,
                            set_volume => Set_Volume'Access,
                            set_panning => Set_Panning'Access,
                            flush => Flush'Access,
                            get_output_time => Get_Output_Time'Access,
                            get_written_time => Get_Written_Time'Access
                           );
   
   function winampGetOutModule return out_plugin.Out_Plugin is
   begin
      return out_wmm;
   end winampGetOutModule;

end binding;
