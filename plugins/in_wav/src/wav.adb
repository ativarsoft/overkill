with Ada.Streams.Stream_IO;
use Ada.Streams.Stream_IO;
with Interfaces;
use Interfaces;

package body WAV is

   procedure Decode is
      File : File_Type;
      ckID : String (1..4);
      cksize : Unsigned_32;
      WAVEID : String (1..4);
   begin
      Open
        (File => File,
         Mode => In_File,
         Name => "mateus_-_autumn_visit.wav");
      String'Read (Stream (File), ckID);
      if ckID /= "RIFF" then
         raise Constraint_Error;
      end if;
      Unsigned_32'Read (Stream (File), cksize);
      String'Read (Stream (File), WAVEID);
      if WAVEID /= "WAVE" then
         raise Constraint_Error;
      end if;
      Close (File);
   exception
      when End_Error =>
         if Is_Open (File) then
            Close (File);
         end if;
   end Decode;

end WAV;
