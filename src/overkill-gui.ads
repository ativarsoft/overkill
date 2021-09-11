with Interfaces.C.Pointers;
with Overkill.Platform;
use Overkill.Platform;

package Overkill.Gui is
   
   --type Window is access Null_Record;
   type Pixmap is access Null_Record;
   type Cursor is access Null_Record;
   type Color is array (Integer range 1..3) of Integer range 0..255;
   
   --
   -- Skin Callbacks
   --
   type SC_Mouse_Down is access procedure(x, y : Integer);
   type SC_Mouse_Up is access procedure(x, y : Integer);
   type SC_Mouse_Move is access procedure(x, y : Integer);
   type SC_Draw is access procedure;
   type SC_Resize is access procedure(w, h : Integer);
   type SC_Focus is access procedure(focus : Boolean);
   
   type Skin_Callbacks is record
      mouse_down : SC_Mouse_Down;
      mouse_up : SC_Mouse_Up;
      mouse_move : SC_Mouse_Move;
      draw : SC_Draw;
      resize : SC_Resize;
      focus : SC_Focus;
   end record;
   
   --
   -- GUI
   --
   type Gui_Init is access procedure;
   type Gui_Quit is access procedure;
   
   type Gui_Create_Window is access function (x : Integer; y: Integer; w : Integer; h : Integer; title : String; callbacks : access Skin_Callbacks) return Window_Type;
   type Gui_Destroy_Window is access procedure (w : Window_Type);
   type Gui_Event_Handler is access procedure;
   type Gui_Show_Window is access procedure (w : Window_Type);
   type Gui_Hide_Window is access procedure (w : Window_Type);
   type Gui_Move_Window is access procedure (w : Window_Type; x : Integer; y : Integer);
   type Gui_Redraw_Window is access procedure (w : Window_Type);
   type Gui_Set_Topmost is access procedure (w : Window_Type);
   type Gui_Set_Not_Topmost is access procedure (w : Window_Type);
   type Gui_Resize_Window is access procedure (w : Window_Type; width : Integer; height : Integer);
   type Gui_Get_Window_Rect is access procedure (rect : Color);
   type Gui_Minimize_Window is access procedure (w : Window_Type);
   
   type Gui_Load_Image is access function (filename : String) return Pixmap;
   type Gui_Unload_Image is access procedure (image : Pixmap);
   type Gui_Begin_Drawing is access procedure (w : Window_Type);
   type Gui_Draw_Image is access procedure (p : Pixmap; dst_x : Integer; dst_y : Integer; w : Integer; h : Integer; src_x : Integer; src_y : Integer);
   type Gui_Draw_Image_Double is access procedure (p : Pixmap; dst_x : Integer; dst_y : Integer; w : Integer; h : Integer; src_x : Integer; src_y : Integer);
   type Gui_Draw_Filled_Rectangle is access procedure (x : Integer; y : Integer; w : Integer; h : Integer; c : Color);
   type Gui_End_Drawing is access procedure;
   
   type Gui_Capture_Mouse is access procedure (w : Window_Type);
   type Gui_Release_Mouse is access procedure;
   type Gui_Load_Cursor is access function(filename : String) return Cursor;
   type Gui_Unload_Cursor is access procedure (p : Cursor);
   type Gui_Set_Cursor is access procedure (p : Cursor);

   type Gui_Check_Glue is access function (a : Window_Type; b : Window_Type; x : Integer; y : Integer) return Boolean;

   type Gui_Open_File_Dialog is access procedure;
   type Gui_Open_Dir_Dialog is access procedure;

   type Gui_Dispatch is record
      init : Gui_Init;
      quit : Gui_Quit;
      
      create_window : Gui_Create_Window;
      destroy_window : Gui_Destroy_Window;
      event_handler : Gui_Event_Handler;
      show_window : Gui_Show_Window;
      hide_window : Gui_Hide_Window;
      move_window : Gui_Move_Window;
      redraw_window : Gui_Redraw_Window;
      set_topmost : Gui_Set_Topmost;
      set_not_topmost : Gui_Set_Not_Topmost;
      resize_window : Gui_Resize_Window;
      get_window_rect : Gui_Get_Window_Rect;
      minimize_window : Gui_Minimize_Window;

      load_image : Gui_Load_Image;
      unload_image : Gui_Unload_Image;
      begin_drawing : Gui_Begin_Drawing;
      draw_image : Gui_Draw_Image;
      draw_image_double : Gui_Draw_Image_Double;
      draw_filled_rectangle : Gui_Draw_Filled_Rectangle;
      end_drawing : Gui_End_Drawing;

      capture_mouse : Gui_Capture_Mouse;
      release_mouse : Gui_Release_Mouse;
      load_cursor : Gui_Load_Cursor;
      unload_cursor : Gui_Unload_Cursor;
      set_cursor : Gui_Set_Cursor;

      check_glue : Gui_Check_Glue;

      open_file_dialog : Gui_Open_File_Dialog;
      open_dir_dialog : Gui_Open_Dir_Dialog;
   end record;

   gui : Gui_Dispatch;
   main_window : Window_Type;

end Overkill.Gui;
