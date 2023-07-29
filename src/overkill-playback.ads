package Overkill.Playback is

   procedure Initialize;

   procedure Previous;
   procedure Play;
   procedure Pause;
   procedure Stop;
   procedure Next;

   function Get_Minutes return Natural;
   function Get_Seconds return Natural;

   function Is_Playing return Boolean;

end Overkill.Playback;
