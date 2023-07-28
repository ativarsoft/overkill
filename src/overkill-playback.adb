with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Overkill.Plugin.Input;
use Overkill.Plugin.Input;
with Overkill.Discovery;
use Overkill.Discovery;

package body Overkill.Playback is

   type Playlist_Element is record
      Path: Unbounded_String;
      Title: Unbounded_String;
      Artist: Unbounded_String;
   end record;

   package Playlist_Vectors is new Ada.Containers.Vectors
     (Element_Type => Playlist_Element,
      Index_Type => Natural);
      
   type Playlist_Type is record
      Entries : Playlist_Vectors.Vector;
      Current_Position : Natural;
   end record;
   
   type Playback_Type is record
      In_Plugin : Overkill.Plugin.Input.In_Plugin_Type;
   end record;

   Playlist : Playlist_Type;
   Playback : Playback_Type;

   procedure Previous is
   begin
      if Playlist.Current_Position > 0 then
         Playlist.Current_Position := Playlist.Current_Position - 1;
      else
         Playlist.Current_Position := Integer (Playlist.Entries.Length);
      end if;
      Play;
   end Previous;
   
   procedure Play is
   begin
      Playback.In_Plugin.Play.all;
   end Play;
   
   procedure Pause is
   begin
      Playback.In_Plugin.Pause.all;
   end Pause;
   
   procedure Stop is
   begin
      Playback.In_Plugin.Stop.all;
   end Stop;
   
   procedure Next is
   begin
      if Playlist.Current_Position = Playlist.Current_Position then
         Playlist.Current_Position := 0;
      else
         Playlist.Current_Position := Playlist.Current_Position + 1;
         Play;
      end if;
      Play;
   end Next;

   function Lookup_In_Plugin
      (Filename : String;
       Discovery : Discovery_Access)
       return In_Plugin_Access
   is
   begin
      return Discovery.Lookup_In_Plugin (Filename);
   end Lookup_In_Plugin;

   function New_Playback
      (Filename : String;
       Discovery : Discovery_Type)
       return Playback_Type
   is
      --  In_Plugin : In_Plugin_Type := Lookup_In_Plugin (Discovery, Filename);
      In_Plugin : In_Plugin_Type;
      Playback : constant Playback_Type :=
         (In_Plugin => In_Plugin);
   begin
      return Playback;
   end New_Playback;

end Overkill.Playback;
