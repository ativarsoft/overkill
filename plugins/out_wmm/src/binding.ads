with Interfaces.C;
with Interfaces.C.Strings;
with out_plugin;

package binding is
   
   type Null_Record is null record;
   type Void_Ptr is access Null_Record;
   type WORD is new Interfaces.C.unsigned_short;
   type DWORD is new Interfaces.C.unsigned_long;
   type UINT is new Interfaces.C.unsigned;
   type LPSTR is new Interfaces.C.Strings.chars_ptr;
   type MMRESULT is new UINT;
   type HWAVEOUT is access Null_Record;
   
   -- Out plugin interface functions
   function winampGetOutModule return out_plugin.Out_Plugin;
   pragma Export (Stdcall, winampGetOutModule, "winampGetOutModule");
   
   -- WMM Constants
   MMSYSERR_NOERROR : constant MMRESULT := 0;
   WHDR_DONE : constant DWORD := 1;
   
   -- WMM Records
   type WAVEHDR is record
      lpData : access String;
      dwBufferLength : DWORD;
      dwBytesRecorded : DWORD;
      dwUser : access DWORD;
      dwFlags : DWORD;
      dwLoops : DWORD;
      lpNext : access WAVEHDR;
      reserved : Void_Ptr;
   end record;
   
   type WAVEFORMATEX is record
      wFormatTag : WORD;
      nChannels : WORD;
      nSamplesPerSec : DWORD;
      nAvgBytesPerSec : DWORD;
      nBlockAlign : WORD;
      wBitsPerSample : WORD;
      cbSize : WORD;
   end record;
   
   -- WMM Functions
   function waveOutGetErrorText(
                                mmrError : MMRESULT;
                                pszText : LPSTR;
                                cchText : UINT
                               ) return MMRESULT;
   pragma Import (Stdcall, waveOutGetErrorText, "waveOutGetErrorText");
   
   function MessageBoxA(
                        win : out_plugin.Window;
                        lpText : LPSTR;
                        lpCaption : LPSTR;
                        uType : UINT
                       ) return Interfaces.C.int;
   pragma Import (Stdcall, MessageBoxA, "MessageBoxA");
   
   function waveOutOpen(
                        phwo : access HWAVEOUT;
                        uDeviceID : UINT;
                        pwfx : access WAVEFORMATEX;
                        dwCallback : access DWORD;
                        dwInstance : access DWORD;
                        fdOpen : DWORD
                       ) return MMRESULT;
   pragma Import (Stdcall, waveOutOpen, "waveOutOpen");
   
   function waveOutPrepareHeader(
                                 hwo : HWAVEOUT;
                                 pwh : access WAVEHDR;
                                 cbwh : UINT
                                ) return MMRESULT;
   pragma Import (Stdcall, waveOutPrepareHeader, "waveOutPrepareHeader");
   
   function waveOutClose(
                         hwo : HWAVEOUT
                        ) return MMRESULT;
   pragma Import (Stdcall, waveOutClose, "waveOutClose");
   
   function waveOutWrite(
                         hwo : HWAVEOUT;
                         pwh : access WAVEHDR;
                         cbwh : UINT
                        ) return MMRESULT;
   pragma Import (Stdcall, waveOutWrite, "waveOutWrite");
   
   function waveOutSetVolume(
                             hwo : HWAVEOUT;
                             dwVolume : DWORD
                            ) return MMRESULT;
   pragma Import (Stdcall, waveOutSetVolume, "waveOutSetVolume");

end binding;
