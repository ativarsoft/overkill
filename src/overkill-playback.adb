with Ada.Containers;
use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Directories;
with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C.Strings;
with Overkill.Plugin.Input;
use Overkill.Plugin.Input;
with Overkill.Plugin.Output;
use Overkill.Plugin.Output;
with Overkill.Discovery;
use Overkill.Discovery;
with Overkill.Subsystems;
with Overkill.Debug;
use Overkill.Debug;
with System;
use System;

package body Overkill.Playback is

   Empty_Playlist     : exception;
   Invalid_Out_Module : exception;

   --Default_Path : constant String := "mateus016-11-2-finished.mp3";
   Default_Path : constant String := "tone://800";
   Default_Title : constant String := "Mental Asylum";
   Default_Artist : constant String := "Mateus";

   type Playlist_Entry_Type is record
      Path: Unbounded_String;
      Title: Unbounded_String;
      Artist: Unbounded_String;
   end record;

   package Playlist_Vectors is new Ada.Containers.Vectors
     (Element_Type => Playlist_Entry_Type,
      Index_Type => Count_Type);
      
   type Playlist_Type is tagged record
      Entries : Playlist_Vectors.Vector;
      Current_Position : Count_Type;
   end record;
   
   type Playback_Type is record
      In_Plugin : Overkill.Plugin.Input.In_Plugin_Access;
      Minutes, Seconds : Natural;
   end record;

   Playlist : Playlist_Type;
   Playback : Playback_Type;

   procedure Previous is
   begin
      if Playlist.Current_Position > 0 then
         Playlist.Current_Position := Playlist.Current_Position - 1;
      else
         Playlist.Current_Position := Playlist.Entries.Length;
      end if;
      Play;
   end Previous;

   function Get_Current_Entry
      (Playlist : in out Playlist_Type)
       return Playlist_Entry_Type
   is
   begin
      if Playlist.Entries.Length = 0 then
         raise Empty_Playlist;
      end if;
      return Playlist.Entries (Playlist.Current_Position);
   end Get_Current_Entry;

   function Try_To_Open
      (Filename : String)
       return Boolean
   is
      use Ada.Directories;
   begin
      return Exists (Filename);
   exception
      when others =>
         return False;
   end Try_To_Open;

   procedure Play
   is
      Current_Entry : Playlist_Entry_Type := Playlist.Get_Current_Entry;
      Filename : String := To_String (Current_Entry.Path);
      File_Exists : Boolean := Try_To_Open (Filename);
      C_Filename : chars_ptr;
      R : int;
   begin
      if File_Exists then
         Put_Line ("File not found.");
         return;
      else
         Put_Line ("File found.");
      end if;
      Playback.In_Plugin := Overkill.Subsystems.Discovery.Lookup_In_Plugin (Filename);
      Put_Line ("Playing file.");
      if Out_Module = Null_Address then
         raise Invalid_Out_Module;
      end if;
      Playback.In_Plugin.Out_Module := Out_Module;
      C_Filename := New_String (Filename);
      R := Playback.In_Plugin.Play (C_Filename);
      --  Free (C_Filename);
      if R /= 0 then
         raise Program_Error with "Playback failed.";
      end if;
   exception
      when Empty_Playlist =>
         Put_Line ("Playlist is empty.");
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
      return Playback_Type
   is
      In_Plugin : In_Plugin_Access := null;
      Playback : constant Playback_Type :=
         (In_Plugin => In_Plugin,
          Minutes   => 0,
          Seconds   => 0);
   begin
      return Playback;
   end New_Playback;

   function New_Playlist
      return Playlist_Type
   is

      Path_Unbounded : Unbounded_String :=
         To_Unbounded_String (Default_Path);
      Title_Unbounded : Unbounded_String :=
         To_Unbounded_String (Default_Title);
      Artist_Unbounded : Unbounded_String :=
         To_Unbounded_String (Default_Artist);

      Default_Playlist_Entry : constant Playlist_Entry_Type :=
         (Path   => Path_Unbounded,
          Title  => Title_Unbounded,
          Artist => Artist_Unbounded);

      Playlist : Playlist_Type;

   begin
      Playlist.Entries.Append (Default_Playlist_Entry);
      Playlist.Current_Position := 0;
      return Playlist;
   end New_Playlist;

   procedure Initialize
   is
   begin
      Playback := New_Playback;
      Playlist := New_Playlist;
   end Initialize;

   function Get_Minutes
      return Natural
   is
   begin
      return Playback.Minutes;
   end Get_Minutes;

   function Get_Seconds
      return Natural
   is
   begin
      return Playback.Seconds;
   end Get_Seconds;

   function Is_Playing
      return Boolean
   is
   begin
      return True;
   end Is_Playing;

end Overkill.Playback;
