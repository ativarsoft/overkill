with Interfaces;
use Interfaces;
with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Pointers;
with Ada.Strings;
with Ada.Unchecked_Conversion;
with System;
use System;
with System.Address_Image;
with Overkill.Debug;
use Overkill.Debug;
with Interfaces.C.Strings;
with Overkill.Platform;
use Overkill.Platform;

package body Overkill.Gui.W32 is
   
   --
   -- Constants
   --
   COLOR_BNTFACE : constant := 15;
   
   -- Load image
   IMAGE_BITMAP        : constant := 0;
   IMAGE_ICON          : constant := 1;
   IMAGE_CURSOR        : constant := 2;
   LR_LOADFROMFILE     : constant := 16#10#;
   LR_CREATEDIBSECTION : constant := 16#2000#;
   
   -- Window styles
   WS_MINIMIZEBOX : constant := 16#00020000#;
   WS_POPUP : constant := 16#80000000#;
   
   -- Window messages
   WM_CLOSE : constant := 16#0010#;
   WM_DESTROY : constant := 16#0002#;
   WM_ERASEBKGND : constant := 16#0014#;
   WM_SETCURSOR : constant := 16#0020#;
   WM_PAINT : constant := 16#000F#;
   WM_LBUTTONDOWN : constant := 16#0201#;
   WM_LBUTTONUP : constant := 16#0202#;
   WM_MOUSEMOVE : constant := 16#0200#;
   WM_SIZE : constant := 16#0005#;
   WM_SETFOCUS : constant := 16#0007#;
   WM_KILLFOCUS : constant := 16#0008#;
   WM_RBUTTONUP : constant := 16#0205#;
   
   -- GetWindowLongPtr
   GWLP_USERDATA : constant := -21;
   
   -- BitBlt
   SRCCOPY : constant := 16#00CC0020#;
   
   -- RedrawWindow
   RDW_INVALIDATE : constant := 16#1#;
   
   SWP_NOSIZE : constant := 16#0001#;
   SWP_NOZORDER : constant := 16#0004#;
   SWP_NOMOVE : constant := 16#0002#;
   
   ANSI_VAR_FONT : constant := 12;
   
   --
   -- Types
   --
   
   type HANDLE is new System.Address;
   type HWND is new Window_Type;
   type LPCSTR is new System.Address;
   type LPSTR is new System.Address;
   type HMENU is new System.Address;
   type HINSTANCE is new System.Address;
   --type HINSTANCE2 is access all Null_Record;
   type HFONT is new System.Address;
   type LPVOID is new System.Address;
   type WORD is new Interfaces.C.unsigned_short;
   type DWORD is new Interfaces.C.unsigned;
   type BOOL is new Interfaces.C.int;
   type UINT is new Interfaces.C.unsigned;
   type ATOM is new System.Address;
   type WNDPROC is new System.Address;
   type HICON is new System.Address;
   type HCURSOR is new System.Address;
   type HBITMAP is new System.Address;
   type HBRUSH is new System.Address;
   type HMODULE is new System.Address;
   type LRESULT is new Interfaces.C.unsigned;
   type WPARAM is new Interfaces.C.unsigned;
   type LPARAM is new Interfaces.C.unsigned;
   type LPMSG is new System.Address;
   type HDC is new System.Address;
   type HGDIOBJ is new System.Address;
   type LONG_PTR is access Null_Record;
   type COLORREF is new DWORD;
   type LPOFNHOOKPROC is access all Null_Record;
   type LPEDITMENU is access all Null_Record;
   
   --
   -- Records
   --
   type POINT is record
      x : LONG;
      y : LONG;
   end record;
   
   type MSG is record
      handle : HWND;
      message : UINT;
      w : WPARAM;
      l : LPARAM;
      time : DWORD;
      pt : POINT;
      lPrivate : DWORD;
   end record;
   
   type WNDCLASSA is record
      style : UINT;
      lpfnWndProc : WNDPROC;
      cbClsExtra : Interfaces.C.int;
      cbWndExtra : Interfaces.C.int;
      instance : HINSTANCE;
      icon : HICON;
      cursor : HCURSOR;
      hbrBackground : HBRUSH;
      lpszMenuName : LPCSTR;
      lpszClassName : LPCSTR;
   end record;
   
   type RECT is record
      left, top, right, bottom : LONG;
   end record;
   
   type PAINTSTRUCT is record
      dc : HDC;
      fErase : BOOL;
      rcPaint : RECT;
      fRestore : BOOL;
      fIncUpdate : BOOL;
      rgbReserved : String(1..32);
   end record;
   
   type OPENFILENAMEA is record
      lStructSize : DWORD;
      hwndOwner : HWND;
      Instance : HINSTANCE;
      lpstrFilter : LPCSTR;
      lpstrCustomFilter : LPSTR;
      nMaxCustFilter : DWORD;
      nFilterIndex : DWORD;
      lpstrFile : LPSTR;
      nMaxFile : DWORD;
      lpstrFileTitle : LPSTR;
      nMaxFileTitle : DWORD;
      lpstrInitialDir : LPCSTR;
      lpstrTitle : LPCSTR;
      Flags : DWORD;
      nFileOffset : WORD;
      nFileExtension : WORD;
      lpstrDefExt : LPCSTR;
      lCustData : LPARAM;
      lpfnHook : LPOFNHOOKPROC;
      lpTemplateName : LPCSTR;
      --lpEditInfo : LPEDITMENU;
      --lpstrPrompt : LPCSTR;
      pvReserved : System.Address;
      dwReserved : DWORD;
      FlagsEx : DWORD;
   end record;
   
   --
   -- Global variables
   --
   class_name : String := "classic_window";
   current_cursor : HCURSOR;
   mem_dc, bmp_dc : HDC;
   mem_bmp : HBITMAP;
   cur_window : HWND;
   p : PAINTSTRUCT;
   
   --
   -- Functions
   --
   function RegisterClassA(
                           lpWndClass : System.Address
                          ) return ATOM;
   pragma Import (Stdcall, RegisterClassA, "RegisterClassA");
   
   function UnregisterClassA(
                            lpClassName : LPCSTR;
                            instance : HINSTANCE
                            ) return BOOL;
   pragma Import (Stdcall, UnregisterClassA, "UnregisterClassA");
   
   function CreateWindowExA(
                            dwExStyle : DWORD;
                            lpClassName : LPCSTR;
                            lpWindowName : LPCSTR;
                            dwStyle : DWORD;
                            X : Interfaces.C.int;
                            Y : Interfaces.C.int;
                            nWidth: Interfaces.C.int;
                            nHeight : Interfaces.C.int;
                            hWndParent : HWND;
                            menu : HMENU;
                            instance : HINSTANCE;
                            lpParam : in Skin_Callbacks
                           ) return HWND;
   pragma Import (Stdcall, CreateWindowExA, "CreateWindowExA");
   
   function DestroyWindow(handle : HWND) return BOOL;
   pragma Import (Stdcall, DestroyWIndow, "DestroyWindow");
   
   function ShowWindow(handle : HWND; cmd : Interfaces.C.int) return BOOL;
   pragma Import (Stdcall, ShowWindow, "ShowWindow");
   
   SW_HIDE : constant := 0;
   SW_SHOW : constant := 5;
   
   IDI_APPLICATION : constant := 32512;
   
   IDC_ARROW : constant := 32512;
   
   --function HWND_To_Window is new Ada.Unchecked_Conversion(HWND, Window);
   --function Window_To_HWND is new Ada.Unchecked_Conversion(Window, HWND);
   function MAKEINTRESOURCE is new Ada.Unchecked_Conversion(System.Address, LPCSTR);
   
   function HANDLE_To_Pixmap is new Ada.Unchecked_Conversion(HANDLE, Pixmap);
   function HANDLE_To_Cursor is new Ada.Unchecked_Conversion(HANDLE, Cursor);
   
   function LPCSTR_To_chars_ptr is new Ada.Unchecked_Conversion(LPCSTR, Interfaces.C.Strings.chars_ptr);
   
   function GetModuleHandleA(lpModuleName : LPCSTR) return HMODULE;
   pragma Import (Stdcall, GetModuleHandleA, "GetModuleHandleA");
   
   function LoadIconA
     (instance : HINSTANCE;
      lpIconName : LPCSTR)
      return HICON;
   
   function LoadIconA
     (instance : HINSTANCE;
      lpIconName : size_t)
      return HICON;
   
   pragma Import (Stdcall, LoadIconA, "LoadIconA");
   
   function LoadCursorA
     (instance : HINSTANCE;
      lpCursorName : LPCSTR)
      return HCURSOR;
   pragma Import (Stdcall, LoadCursorA, "LoadCursorA");
   
   function LoadBitmapA
     (instance : HINSTANCE;
      lpBitmapName : LPCSTR)
      return HBITMAP;
   pragma Import (Stdcall, LoadBitmapA, "LoadBitmapA");
   
   function LoadImageA
     (instance : HINSTANCE;
      name : LPCSTR;
      imgtype : UINT;
      cx : Interfaces.C.int;
      cy : Interfaces.C.int;
      fuLoad : UINT)
      return HANDLE;
   pragma Import (Stdcall, LoadImageA, "LoadImageA");
   
   function GetLastError return DWORD;
   pragma Import (Stdcall, GetLastError, "GetLastError");
   
   FORMAT_MESSAGE_ALLOCATE_BUFFER : constant := 16#00000100#;
   FORMAT_MESSAGE_FROM_SYSTEM : constant := 16#00001000#;
   FORMAT_MESSAGE_IGNORE_INSERTS : constant := 16#00000200#;
   
   function FormatMessageA
     (dwFlags : DWORD;
      lpSource : LPVOID;
      dwMessageId : DWORD;
      dwLanguageId : DWORD;
      lpBuffer : out LPCSTR;
      nSize : DWORD)
      return DWORD;
   pragma Import (Stdcall, FormatMessageA, "FormatMessageA");
   
   function MessageBoxA
     (window : HWND;
      lpText : LPCSTR;
      lpCaption : LPCSTR;
      uType : UINT)
      return Interfaces.C.int;
   pragma Import (Stdcall, MessageBoxA, "MessageBoxA");
   
   MB_ICONERROR : constant := 16#00000010#;
   MB_OK : constant := 16#00000000#;
   
   function DefWindowProcA
     (handle : HWND;
      uMsg : UINT;
      w : WPARAM;
      l : LPARAM)
      return LRESULT;
   pragma Import (Stdcall, DefWindowProcA, "DefWindowProcA");
   
   function GetMessageA
     (msg : LPMSG;
      handle : HWND;
      wMsgFilterMin : UINT;
      wMsgFilterMax : UINT) return BOOL;
   pragma Import (Stdcall, GetMessageA, "GetMessageA");
   
   function TranslateMessage(msg : LPMSG) return BOOL;
   pragma Import (Stdcall, TranslateMessage, "TranslateMessage");
   
   function DispatchMessageA(msg : LPMSG) return LRESULT;
   pragma Import (Stdcall, DispatchMessageA, "DispatchMessageA");
   
   procedure PostQuitMessage(nExitCode : Interfaces.C.int);
   pragma Import (Stdcall, PostQuitMessage, "PostQuitMessage");
   
   function SetCursor(cursor : HCURSOR) return HCURSOR;
   pragma Import (Stdcall, SetCursor, "SetCursor");
   
   function CreateCompatibleDC(dc : HDC) return HDC;
   pragma Import (Stdcall, CreateCompatibleDC, "CreateCompatibleDC");
   
   function DeleteDC(dc : HDC) return BOOL;
   pragma Import (Stdcall, DeleteDC, "DeleteDC");
   
   function RedrawWindow
     (h : Window_Type;
      lprcUpdate : System.Address;
      hrgnUpdate : System.Address;
      flags : UINT)
      return BOOL;
   pragma Import (Stdcall, RedrawWindow, "RedrawWindow");
   
   function SetWindowLongPtrA
     (win : HWND;
      nIndex : Interfaces.C.int;
      dwNewLong : in Skin_Callbacks)
      return LONG_PTR;
   pragma Import (Stdcall, SetWindowLongPtrA, "SetWindowLongA");
   
   function GetWindowLongPtrA
     (window : HWND;
      nIndex : Interfaces.C.int)
      return access Skin_Callbacks;
   pragma Import (Stdcall, GetWindowLongPtrA, "GetWindowLongA");
   
   function GET_X_LPARAM(l : LPARAM) return Integer is
      Value : LPARAM := l and 16#FFFF#;
      Ret : Integer;
   begin
      if (Value and 16#8000#) = 0 then
         Ret := Integer (Value);
      else
         Ret := - Integer ((not Value) and 16#7FFF#);
      end if;
      return Ret;
   end GET_X_LPARAM;
   
   function GET_Y_LPARAM(l : LPARAM) return Integer is
      Value : LPARAM := LPARAM (Interfaces.Shift_Right(Unsigned_32 (l), 16));
      Ret : Integer;
   begin
      if (Value and 16#8000#) = 0 then
         Ret := Integer (Value);
      else
         Ret := - Integer ((not Value) and 16#7FFF#);
      end if;
      return Ret;
   end GET_Y_LPARAM;
   
   function LOWORD(v : DWORD) return Integer is
   begin
      return Integer(v and 16#FFFF#);
   end LOWORD;
   
   function HIWORD(v : DWORD) return Integer is
   begin
      return Integer(Interfaces.Shift_Right(Interfaces.Unsigned_32(v and 16#FFFF0000#), 16));
   end HIWORD;
   
   function BeginPaint
     (win : HWND;
      lpPaint : out PAINTSTRUCT)
      return HDC;
   pragma Import (Stdcall, BeginPaint, "BeginPaint");
   
   function EndPaint
     (win : HWND;
      lpPaint : access constant PAINTSTRUCT)
      return BOOL;
   pragma Import (Stdcall, EndPaint, "EndPaint");
   
   function GetWindowRect
     (win : HWND;
      lpRect : out RECT)
      return BOOL;
   pragma Import (Stdcall, GetWindowRect, "GetWindowRect");
   
   function CreateCompatibleBitmap
     (dc : HDC;
      cx, cy : Interfaces.C.int)
      return HBITMAP;
   pragma Import (Stdcall, CreateCompatibleBitmap, "CreateCompatibleBitmap");
   
   function SelectObject
     (dc : HDC;
      h : HGDIOBJ)
      return HGDIOBJ;
   pragma Import (Stdcall, SelectObject, "SelectObject");
   
   function BitBlt
     (dc : HDC;
      x, y, cx, cy : Interfaces.C.int;
      hdcSrc : HDC;
      x1, y1 : Interfaces.C.int;
      rop : DWORD)
      return BOOL;
   pragma Import (Stdcall, BitBlt, "BitBlt");
   
   function ClientToScreen
     (h : HWND;
      p : in out POINT)
      return BOOL;
   pragma Import (Stdcall, ClientToScreen, "ClientToScreen");
   
   function SetWindowPos
     (w : HWND;
      after : HWND;
      X, Y, cx, cy : int;
      uFlags : UINT)
      return BOOL;
   pragma Import (Stdcall, SetWindowPos, "SetWindowPos");
   
   function SetCapture
     (handle : HWND)
      return HWND;
   pragma Import (Stdcall, SetCapture, "SetCapture");
   
   function DrawText
     (Handle : HDC;
      lpchText : LPCSTR;
      cchText : Interfaces.C.int;
      lprc : access RECT;
      format : UINT)
      return int;
   pragma Import (Stdcall, DrawText, "DrawTextA");
   
   function GetStockObject
     (i : int)
      return HGDIOBJ;
   pragma Import (Stdcall, GetStockObject, "GetStockObject");
   
   function SetBkColor
     (Handle : HDC;
      Color : COLORREF)
      return COLORREF;
   pragma Import (Stdcall, SetBkColor, "SetBkColor");
   
   function SetTextColor
     (Handle : HDC;
      Color : COLORREF)
      return COLORREF;
   pragma Import (Stdcall, SetTextColor, "SetTextColor");
   
   function GetOpenFileName (p : access OPENFILENAMEA) return BOOL;
   pragma Import (Stdcall, GetOpenFileName, "GetOpenFileNameA");
   
   function callback
     (handle : HWND;
      uMsg : UINT;
      w : WPARAM;
      l : LPARAM) 
      return LRESULT;
   pragma Convention (Stdcall, callback);
   
   function callback
     (handle : HWND;
      uMsg : UINT;
      w : WPARAM;
      l : LPARAM)
      return LRESULT is
      sc : access Skin_Callbacks;
      previous_cursor : HCURSOR;
   begin
      sc := GetWindowLongPtrA(handle, GWLP_USERDATA);
      if sc /= null then
         Put_Line("Skin callback is " & System.Address_Image(sc.all'Address));
      else
         Put_Line ("Skin callback is null.");
      end if;
      
      case uMsg is
         when WM_CLOSE =>
            Put_Line ("WM_CLOSE");
            null;
         when WM_DESTROY =>
            Put_Line ("WM_DESTROY");
            if handle = HWND (main_window) then
               PostQuitMessage(0);
               return 0;
            end if;
         when WM_ERASEBKGND => -- prevent flickering
            return 1;
         when WM_SETCURSOR =>
            if current_cursor /= HCURSOR (Null_Address) then
               previous_cursor := SetCursor(current_cursor);
            end if;
            return 1;
         when others =>
            null;
      end case;
      -- sometimes w is null
      if sc /= null then
         case uMsg is
            when WM_PAINT =>
               sc.draw.all (Current_Skin.all);
            when WM_LBUTTONDOWN =>
               if sc.mouse_down /= null then
                  sc.mouse_down(Current_Skin.all, GET_X_LPARAM(l), GET_Y_LPARAM(l));
               end if;
            when WM_LBUTTONUP =>
               if sc.mouse_up /= null then
                  sc.mouse_up(Current_Skin.all, GET_X_LPARAM(l), GET_Y_LPARAM(l));
               end if;
            when WM_MOUSEMOVE =>
               if sc.mouse_move /= null then
                  sc.mouse_move(Current_Skin.all, GET_X_LPARAM(l), GET_Y_LPARAM(l));
               end if;
            when WM_SIZE =>
               if sc.resize /= null then
                  sc.resize(Current_Skin.all, LOWORD(DWORD(l)), HIWORD(DWORD(l)));
               end if;
            when WM_SETFOCUS =>
               if sc.focus /= null then
                  sc.focus(Current_Skin.all, True);
               end if;
            when WM_KILLFOCUS =>
               if sc.focus /= null then
                  sc.focus(Current_Skin.all, False);
               end if;
            when WM_RBUTTONUP =>
               null;
            when others =>
               null;
         end case;
      end if;
      return DefWindowProcA(handle, uMsg, w, l);
   end callback;
   
   procedure error(last_error : DWORD) is
      --buffer : LPCSTR;
      --r1 : DWORD;
      --r2 : Interfaces.C.int;
      --flags : Interfaces.Unsigned_32;
      --len : Interfaces.C.size_t;
      msg_ptr : Interfaces.C.Strings.chars_ptr;
      --msg : access Interfaces.C.char_array;
   begin
      Put_Line("The error function contains an error.");
      return;
      
      --Put("Error: ");
      -- This function is messing with the stack.
      --flags := FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS;
      --r1 := FormatMessageA(
      --                     DWORD(flags),
      --                     LPVOID(Null_Address),
      --                     last_error,
      --                     0,
      --                     buffer,
      --                     0
      --                    );
      --if r1 = 0 then
      --   raise Program_Error;
      --end if;
      
      --if buffer = LPCSTR(Null_Address) then
      --   Put_Line ("null error message");
      --   raise Program_Error;
      --end if;
      
      --msg_ptr := LPCSTR_To_chars_ptr(buffer);
      --len := Interfaces.C.Strings.Strlen(msg_ptr);
      --msg := new Interfaces.C.char_array'(Interfaces.C.Strings.Value(msg_ptr, len));
      --Put_Line(Interfaces.C.To_Ada(msg.all, True));
      
      --flags := MB_ICONERROR or MB_OK;
      --r2 := MessageBoxA(
      --                  HWND(Null_Address),
      --                  buffer,
      --                  LPCSTR(Null_Address),
      --                  UINT(flags)
      --                 );
      --if r2 = 0 then
      --   raise Program_Error;
      --end if;
   end error;

   procedure W32_Init is
      class : WNDCLASSA;
      instance : HINSTANCE := HINSTANCE(GetModuleHandleA(LPCSTR(Null_Address)));
      c_class_name : Interfaces.C.char_array := Interfaces.C.To_C(class_name);
   begin
      Put_Line ("Initializing W32 GUI.");
      class.style := 0;
      class.cbClsExtra := Interfaces.C.int(0);
      class.cbWndExtra := Interfaces.C.int(0);
      class.lpszMenuName := LPCSTR(Null_Address);
      
      class.hbrBackground := HBRUSH(System'To_Address(COLOR_BNTFACE + 1));
      class.lpszClassName := LPCSTR(c_class_name'Address);
      class.lpfnWndProc := WNDPROC(callback'Address);
      if class.lpfnWndProc = WNDPROC(Null_Address) then
         -- ???
         Put_Line ("RegisterClass fails silently if lpfnWndProc is null.");
         raise Program_Error;
      end if;
      class.instance := instance;
      if class.instance = HINSTANCE(Null_Address) then
         -- TODO: what GetModuleHandle returns on error?
         Put_Line ("null instance");
         raise Program_Error;
      end if;
      class.icon := LoadIconA(Instance, 1);
      if class.icon = HICON(Null_Address) then
         -- error(GetLastError);
         Put_Line ("Error loading default window icon.");
         raise Program_Error;
      end if;
      class.cursor := LoadCursorA(HINSTANCE(Null_Address), MAKEINTRESOURCE(System'To_Address(IDC_ARROW)));
      if class.cursor = HCURSOR (Null_Address) then
         -- error(GetLastError);
         Put_Line ("Error loading default window cursor.");
      end if;
      
      if RegisterClassA(class'Address) = ATOM(Null_Address) then
         Put_Line ("error registering classic window class: ");
         error(GetLastError);
         raise Program_Error;
      end if;
      
      mem_dc := CreateCompatibleDC(HDC(System'To_Address(0)));
      if mem_dc = HDC(Null_Address) then
         Put_Line ("error creating memory drawing context: ");
         error(GetLastError);
         raise Program_Error;
      end if;
      bmp_dc := CreateCompatibleDC(HDC(System'To_Address(0)));
      if mem_dc = HDC(Null_Address) then
         Put_Line ("error creating bitmap drawing context: ");
         error(GetLastError);
         raise Program_Error;
      end if;
   end W32_Init;
   
   procedure W32_Quit is
      r : BOOL;
      c_class_name : Interfaces.C.char_array := Interfaces.C.To_C(class_name);
   begin
      r := DeleteDC(mem_dc);
      if r = 0 then
         raise Program_Error;
      end if;
      
      r := DeleteDC(bmp_dc);
      if r = 0 then
         raise Program_Error;
      end if;
      
      r := UnregisterClassA(LPCSTR(c_class_name'Address), HINSTANCE(Null_Address));
      if r = 0 then
         raise Program_Error;
      end if;
   end W32_Quit;
   
   function W32_Create_Window(x : Integer; y: Integer; w : Integer; h : Integer; title : String; callbacks : Skin_Callbacks) return Window_Type is
      c_class_name : Interfaces.C.char_array := Interfaces.C.To_C(class_name);
      c_title : Interfaces.C.char_array := Interfaces.C.To_C(title);
      c_x : Interfaces.C.int := Interfaces.C.int(x);
      c_y : Interfaces.C.int := Interfaces.C.int(y);
      c_w : Interfaces.C.int := Interfaces.C.int(w);
      c_h : Interfaces.C.int := Interfaces.C.int(h);
      r : HWND;
      dwStyle : DWORD;
      ret1 : LONG_PTR;
   begin
      dwStyle := WS_MINIMIZEBOX or WS_POPUP;
      r := CreateWindowExA
        (0,
         LPCSTR (c_class_name'Address),
         LPCSTR (c_title'Address),
         dwStyle,
         c_x,
         c_y,
         c_w,
         c_h,
         HWND (main_window),
         HMENU (Null_Address),
         HINSTANCE(Null_Address),
         callbacks);
      if r = HWND(Null_Address) then
         error(GetLastError);
         raise Program_Error;
      end if;
      ret1 := SetWindowLongPtrA(r, GWLP_USERDATA, callbacks);
      return Window_Type (r);
   end W32_Create_Window;
   
   procedure W32_Destroy_Window(w : Window_Type) is
      r : BOOL;
   begin
      r := DestroyWindow (HWND (w));
      if r = 0 then
         Put_Line ("Window could not be destroyed.");
      end if;
   end W32_Destroy_Window;
   
   procedure W32_Event_Handler is
      message : MSG;
      r1 : BOOL;
      r2 : LRESULT;
   begin
      loop
         r1 := GetMessageA(LPMSG(message'Address), HWND(Null_Address), 0, 0);
         exit when r1 = 0;
         r1 := TranslateMessage(LPMSG(message'Address));
         r2 := DispatchMessageA(LPMSG(message'Address));
      end loop;
   end W32_Event_Handler;
   
   procedure W32_Show_Window(w : Window_Type) is
      r : BOOL;
   begin
      r := ShowWindow(HWND(w), SW_SHOW);
      if r = 0 then
         --error(GetLastError);
         null;
      end if;
   end W32_Show_Window;
   
   procedure W32_Hide_Window (w : Window_Type) is
      r : BOOL;
   begin
      r := ShowWindow (HWND (w), SW_HIDE);
      if r = 0 then
         null;
      end if;
   end W32_Hide_Window;
   
   procedure W32_Move_Window(w : Window_Type; x : Integer; y : Integer) is
      --pos : aliased POINT;
      r : aliased RECT;
      ret : BOOL;
   begin
      --pos.x := long (x);
      --pos.y := long (y);
      --ret := ClientToScreen (Window_To_HWND (w), pos);
      ret := GetWindowRect (HWND (w), r);
      r.left := r.left + long (x);
      r.top := r.top + long (y);
      ret := SetWindowPos
        (HWND (w), HWND(Null_Address),
         int (r.left), int (r.top), 0, 0,
         SWP_NOSIZE or SWP_NOZORDER);
   end W32_Move_Window;
   
   procedure W32_Redraw_Window(w : Window_Type) is
      r : BOOL;
   begin
      r := RedrawWindow (w, Null_Address, Null_Address, RDW_INVALIDATE);
   end W32_Redraw_Window;
   
   procedure W32_Set_Topmost(w : Window_Type) is
   begin
      null;
   end W32_Set_Topmost;
   
   procedure W32_Set_Not_Topmost(w : Window_Type) is
   begin
      null;
   end W32_Set_Not_Topmost;
   
   procedure W32_Resize_Window(w : Window_Type; width : Integer; height : Integer)
   is
      Ret : BOOL;
   begin
      Ret := SetWindowPos (HWND (w), HWND(Null_Address), 0, 0, int (Width), int (Height), SWP_NOMOVE or SWP_NOZORDER);
   end W32_Resize_Window;
   
   procedure W32_Get_Window_Rect(rect : Color) is
   begin
      null;
   end W32_Get_Window_Rect;
   
   procedure W32_Minimize_Window(w : Window_Type) is
   begin
      null;
   end W32_Minimize_Window;
   
   function W32_Load_Image(filename : String) return Pixmap is
      bitmap : HANDLE;
      c_filename : Interfaces.C.char_array := Interfaces.C.To_C(filename);
      Load_Image_Alpha : constant Boolean := True;
   begin
      if Load_Image_Alpha then
         bitmap := LoadImageA(HINSTANCE(Null_Address), LPCSTR(c_filename'Address), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE or LR_CREATEDIBSECTION);
      else
         bitmap := LoadImageA(HINSTANCE(Null_Address), LPCSTR(c_filename'Address), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE);
      end if;
      return HANDLE_To_Pixmap(bitmap);
   end W32_Load_Image;
   
   procedure W32_Unload_Image(image : Pixmap) is
   begin
      null;
   end W32_Unload_Image;
   
   procedure W32_Begin_Drawing(w : Window_Type) is
      r : RECT;
      width, height : Interfaces.C.int;
      ret1 : HDC;
      ret2 : HGDIOBJ;
      ret3 : BOOL;
   begin
      ret1 := BeginPaint (HWND (w), p);
      cur_window := HWND (w);
      
      ret3 := GetWindowRect (HWND (w), r);
      width := Interfaces.C.int(r.right - r.left);
      height := Interfaces.C.int(r.bottom - r.top);
      mem_bmp := CreateCompatibleBitmap(p.dc, width, height);
      ret2 := SelectObject(mem_dc, HGDIOBJ(mem_bmp));
   end W32_Begin_Drawing;
   
   procedure W32_Draw_Image(p : Pixmap; dst_x : Integer; dst_y : Integer; w : Integer; h : Integer; src_x : Integer; src_y : Integer)
   is
      ret1 : BOOL;
      ret2 : HGDIOBJ;
   begin
      ret2 := SelectObject(bmp_dc, HGDIOBJ(p));
      ret1 := BitBlt
        (mem_dc,
         Interfaces.C.int(dst_x),
         Interfaces.C.int(dst_y),
         Interfaces.C.int(w),
         Interfaces.C.int(h),
         bmp_dc,
         Interfaces.C.int(src_x),
         Interfaces.C.int(src_y),
         SRCCOPY);
      Put_Line("dst_x=" & dst_x'Image & " dst_y=" & dst_y'Image & " w=" & w'Image & " h=" & h'Image & " src_x=" & src_x'Image & " src_y=" & src_y'Image);
   end W32_Draw_Image;
   
   procedure W32_Draw_Image_Double(p : Pixmap; dst_x : Integer; dst_y : Integer; w : Integer; h : Integer; src_x : Integer; src_y : Integer) is
   begin
      null;
   end W32_Draw_Image_Double;
   
   procedure W32_Draw_Filled_Rectangle(x : Integer; y : Integer; w : Integer; h : Integer; c : Color) is
   begin
      null;
   end W32_Draw_Filled_Rectangle;
   
   procedure W32_Draw_Text
     (x, y, w, h : Integer;
      Text : String)
   is
      Font : HFONT := HFONT (GetStockObject (ANSI_VAR_FONT));
      C_Text : aliased char_array := To_C (Text);
      Rectangle : aliased RECT := (long (x), long (y), long (x + w), long (y + h));
      Ret : int;
      hOldFont : HFONT;
      Old_Color : COLORREF;
   begin
      Old_Color := SetBkColor (mem_dc, 16#00000000#);
      Old_Color := SetTextColor (mem_dc, 16#00FFFFFF#);
      hOldFont := HFONT (SelectObject(mem_dc, HGDIOBJ (Font)));
      Ret := DrawText
        (Handle => mem_dc,
         lpchText => LPCSTR (C_Text'Address),
         cchText => Text'Length,
         lprc => Rectangle'Access,
         format => 0);
   end W32_Draw_Text;
   
   procedure W32_End_Drawing
   is
      r : RECT;
      width, height : Interfaces.C.int;
      ret1 : BOOL;
   begin
      ret1 := GetWindowRect(cur_window, r);
      width := Interfaces.C.int(r.right - r.left);
      height := Interfaces.C.int(r.bottom - r.top);
      ret1 := BitBlt(p.dc, 0, 0, width, height, mem_dc, 0, 0, SRCCOPY);
      ret1 := EndPaint(cur_window, new PAINTSTRUCT'(p));
   end W32_End_Drawing;
   
   procedure W32_Capture_Mouse(w : Window_Type) is
      Previous : HWND;
   begin
      Previous := SetCapture(HWND(w));
   end W32_Capture_Mouse;
   
   procedure W32_Release_Mouse is
      function ReleaseCapture return BOOL;
      pragma Import (Stdcall, ReleaseCapture, "ReleaseCapture");
      
      R : BOOL;
   begin
      R := ReleaseCapture;
   end W32_Release_Mouse;
   
   function W32_Load_Cursor(filename : String) return Cursor is
      c_filename : Interfaces.C.char_array := Interfaces.C.To_C(filename);
      cursor : HANDLE;
   begin
      cursor := LoadImageA(HINSTANCE(Null_Address), LPCSTR(c_filename'Address), IMAGE_CURSOR, 0, 0, LR_LOADFROMFILE);
      return HANDLE_To_Cursor(cursor);
   end W32_Load_Cursor;
   
   procedure W32_Unload_Cursor(p : Cursor) is
   begin
      null;
   end W32_Unload_Cursor;
   
   procedure W32_Set_Cursor(p : Cursor) is
   begin
      current_cursor := HCURSOR(p);
   end W32_Set_Cursor;

   function W32_Check_Glue(a : Window_Type; b : Window_Type; x : Integer; y : Integer) return Boolean is
   begin
      return False;
   end W32_Check_Glue;

   procedure W32_Open_File_Dialog
   is
      package C renames Interfaces.C;
      ofn : aliased OPENFILENAMEA :=
        (lStructSize => 0,
         hwndOwner => HWND(Null_Address),
         Instance => HINSTANCE (Null_Address),
         lpstrFilter => LPCSTR (Null_Address),
         lpstrCustomFilter => LPSTR (Null_Address),
         nMaxCustFilter => 0,
         nFilterIndex => 0,
         lpstrFile => LPSTR (Null_Address),
         nMaxFile => 0,
         lpstrFileTitle => LPSTR (Null_Address),
         nMaxFileTitle => 0,
         lpstrInitialDir => LPCSTR (Null_Address),
         lpstrTitle => LPCSTR (Null_Address),
         Flags => 0,
         nFileOffset => 0,
         nFileExtension => 0,
         lpstrDefExt => LPCSTR (Null_Address),
         lCustData => 0,
         lpfnHook => LPOFNHOOKPROC'(null),
         lpTemplateName => LPCSTR (Null_Address),
           ----lpEditInfo : LPEDITMENU;
         ----lpstrPrompt : LPCSTR;
         pvReserved => Null_Address,
         dwReserved => 0,
         FlagsEx => 0);
      Ret : BOOL;
      szFile : aliased char_array (1..256) := (others => C.nul);
      Filter : aliased char_array := "All" & C.nul & "*.*" & C.nul & C.nul & C.nul;
      Initial_Dir : aliased char_array (1..256) := (1 => '.', others => C.nul);
   begin
      ofn.lStructSize := OPENFILENAMEA'Size / 8;
      ofn.hwndOwner := HWND (main_window);
      ofn.lpstrFile := LPSTR (szFile'Address);
      ofn.nMaxFile := szFile'Length;
      ofn.lpstrFilter := LPCSTR (Filter'Address);
      ofn.nFilterIndex := 0;
      Ret := GetOpenFileName (ofn'Access);
   end W32_Open_File_Dialog;
   
   procedure W32_Open_Dir_Dialog is
   begin
      null;
   end W32_Open_Dir_Dialog;

end Overkill.Gui.W32;
