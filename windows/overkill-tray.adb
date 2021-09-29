with Overkill.Gui;
use Overkill.Gui;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
with Interfaces.C;
use Interfaces.C;
with Overkill.Platform;
use Overkill.Platform;
with Overkill.Debug;
use Overkill.Debug;

package body Overkill.Tray is
   
   type HWND is access all Null_Record;
   type HICON is access Null_Record;
   type HMODULE is access Null_Record;
   type HINSTANCE is access all Null_Record;
   type LPCSTR is access Null_Record;
   type DWORD is new unsigned;
   type BOOL is new int;
   type UINT is new unsigned;
   
   type NOTIFYICONDATAA is record
      cbSize : DWORD;
      window : HWND;
      uID : UINT;
      uFlags : UINT;
      uCallbackMessage : UINT;
      icon : HICON;
      szTip : String (1..64);
   end record;
   
   NIM_ADD : constant := 16#00000000#;
   
   NIF_ICON : constant := 16#00000002#;
   NIF_TIP : constant := 16#00000004#;
   
   --function Window_To_HWND is new Ada.Unchecked_Conversion(Window, HWND);
   
   function GetModuleHandleA(lpModuleName : LPCSTR) return HMODULE;
   pragma Import (Stdcall, GetModuleHandleA, "GetModuleHandleA");
   
   function LoadIconA
     (instance : HINSTANCE;
      lpIconName : size_t)
      return HICON;
   pragma Import (Stdcall, LoadIconA, "LoadIconA");
   
   function Shell_NotifyIconA
     (dwMessage : DWORD;
      lpData : in out NOTIFYICONDATAA)
      return BOOL;
   pragma Import (Stdcall, Shell_NotifyIconA, "Shell_NotifyIconA");

   overriding procedure Initialize
     (Tray : in out Tray_Type)
   is
   begin
      null;
   end Initialize;
   
   overriding procedure Finalize
     (Tray : in out Tray_Type)
   is
   begin
      Put_Line ("Finalizing tray.");
   end Finalize;
   
   overriding procedure Show
     (Tray : in out Tray_Type)
   is
      Icon_Data : NOTIFYICONDATAA;
      Instance : HINSTANCE := HINSTANCE (GetModuleHandleA (null));
      Ret : BOOL;
   begin
      Put_Line ("Initializing tray.");
      Icon_Data.cbSize := NOTIFYICONDATAA'Size / 8;
      Icon_Data.Window := HWND (Overkill.Gui.main_window);
      Icon_Data.uID := 0;
      Icon_Data.uFlags := NIF_ICON or NIF_TIP;
      Icon_Data.icon := LoadIconA (Instance, 1);
      Icon_Data.szTip := Overwrite (Icon_Data.szTip, 1, "Overkill");
      Ret := Shell_NotifyIconA (NIM_ADD, Icon_Data);
      if Ret = 0 then
         raise Program_Error with "Failed to create tray icon.";
      end if;
   end Show;
   
   overriding procedure Hide
     (Tray : in out Tray_Type)
   is
   begin
      null;
   end Hide;

end Overkill.Tray;
