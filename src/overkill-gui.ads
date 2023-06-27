with Interfaces.C.Pointers;
with Overkill.Platform;
use Overkill.Platform;
with System;

package Overkill.Gui is

   pragma Elaborate_Body;
   
   --type Window is access Null_Record;
   type Pixmap is new System.Address;
   type Cursor is new System.Address;
   type Color is array (Integer range 1..3) of Integer range 0..255;
   
   type Skin is (Classic_Skin, Modern_Skin);
   type Skin_Type;
   type String_Access is access constant String;
   
   --
   -- Classic Skin
   --
   
   type Bitmap_Type is
     (BMP_BALANCE,
      BMP_CBUTTONS,
      BMP_EQ_EX,
      BMP_EQMAIN,
      BMP_EQMAIN_ISO,
      BMP_GEN,
      BMP_GENEX,
      BMP_MAIN,
      BMP_MB,
      BMP_MONOSTER,
      BMP_NUMBERS,
      BMP_NUMS_EX,
      BMP_PLAYPAUS,
      BMP_PLEDIT,
      BMP_POSBAR,
      BMP_SHUFREP,
      BMP_TEXT,
      BMP_TITLEBAR,
      BMP_VIDEO,
      BMP_VOLUME);

   type Cursor_Type is
     (CURSOR_CLOSE,
      CURSOR_EQCLOSE,
      CURSOR_EQNORMAL,
      CURSOR_EQSLID,
      CURSOR_EQTITLE,
      CURSOR_MAINMENU,
      CURSOR_MIN,
      CURSOR_NORMAL,
      CURSOR_PCLOSE,
      CURSOR_PNORMAL,
      CURSOR_POSBAR,
      CURSOR_PSIZE,
      CURSOR_PTBAR,
      CURSOR_PVSCROLL,
      CURSOR_PWINBUT,
      CURSOR_PWSNORM,
      CURSOR_PWSSIZE,
      CURSOR_SONGNAME,
      CURSOR_TITLEBAR,
      CURSOR_VOLBAL,
      CURSOR_VOLBAR,
      CURSOR_WINBUT,
      CURSOR_WSCLOSE,
      CURSOR_WSMIN,
      CURSOR_WSNORMAL,
      CURSOR_WSPOSBAR,
      CURSOR_WSWINBUT);

   type Bitmap_Array_Type is array (Bitmap_Type) of gui.Pixmap;
   type Cursor_Array_Type is array (Cursor_Type) of gui.Cursor;
   
   --
   -- Records
   --
   type Subbitmap is record
      bmp : Bitmap_Type;
      X, Y : Natural;
   end record;

   type Subbitmap_Access is access constant Subbitmap;

   type Subbitmap_Array is array (Natural range <>) of Subbitmap_Access;

   type Subbitmap_Array_Access is access constant Subbitmap_Array;

   type Rect is array (1..4) of Natural;

   type Widget_Type is
     (Background_Widget,
      Button_Widget,
      Checkbox_Widget,
      Slider_Widget,
      Clutterbar_Widget,
      Song_Title_Widget,
      Scroll_Widget,
      Menu_Widget,
      Resizeable_Background_Widget,
      Pledit_Widget);

   type Button_Action_Func is access procedure (Skin : in out Skin_Type);
   type Checkbox_Action_Func is access procedure (Skin : in out Skin_Type; checked : Boolean);
   type Slider_Action_Func is access procedure (Skin : in out Skin_Type; value : Integer);

   type Clutterbar_O is access procedure;
   type Clutterbar_A is access procedure (a : Window_Type; b : Boolean);
   type Clutterbar_I is access procedure;
   type Clutterbar_D is access procedure (a : Boolean);
   type Clutterbar_V is access procedure;

   type Widget(T : Widget_Type := Background_Widget) is record
      r : aliased Rect;
      c : Cursor_Type;
      case T is
         when Background_Widget =>
            subbmp : Subbitmap_Access;
            move_window : Boolean;
         when Button_Widget =>
            button_up : Subbitmap_Access;
            button_down : Subbitmap_Access;
            button_action : Button_Action_Func;
         when Checkbox_Widget =>
            checkbox_on_up : Subbitmap_Access;
            checkbox_on_down : Subbitmap_Access;
            checkbox_off_up : Subbitmap_Access;
            checkbox_off_down : Subbitmap_Access;
            checkbox_checked : Boolean;
            checkbox_action : Checkbox_Action_Func;
         when Slider_Widget =>
            slider_background : Subbitmap_Array_Access;
            slider_up : Subbitmap_Access;
            slider_down : Subbitmap_Access;
            slider_horizontal : Boolean;
            slider_min : Natural;
            slider_max : Natural;
            slider_value : Natural;
            slider_action : Slider_Action_Func;
         when Clutterbar_Widget =>
            clutterbar_a_value : Boolean;
            clutterbar_d_value : Boolean;
            clutterbar_mouse_down : Integer;
            clutterbar_set_o : Clutterbar_O;
            clutterbar_set_a : Clutterbar_A;
            clutterbar_set_i : Clutterbar_I;
            clutterbar_set_d : Clutterbar_D;
            clutterbar_set_v : Clutterbar_V;
         when Song_Title_Widget =>
            song_title_string : String_Access;
            song_title_offset : Integer;
         when Scroll_Widget =>
            scroll_background : Subbitmap_Access;
            scroll_bar_up : Subbitmap_Access;
            scroll_bar_down : Subbitmap_Access;
            scroll_bar_length : Natural;
            scroll_value : Natural;
         when Menu_Widget =>
            Menu_Num_Buttons : Natural;
            Menu_Buttons_Up : Subbitmap_Array_Access;
            Menu_Buttons_Down : Subbitmap_Array_Access;
            Menu_Bar : Subbitmap_Access;
         when Resizeable_Background_Widget =>
            RB_top_left : Subbitmap_Access;
            RB_title : Subbitmap_Access;
            RB_top : Subbitmap_Access;
            RB_top_right : Subbitmap_Access;
            RB_left : Subbitmap_Access;
            RB_right : Subbitmap_Access;
            RB_bottom : Subbitmap_Access;
            RB_bottom_left : Subbitmap_Access;
            RB_bottom_right : Subbitmap_Access;
            RB_length_top : Natural;
            RB_length_left : Natural;
            RB_length_right : Natural;
            RB_length_bottom : Natural;
         when Pledit_Widget =>
            null;
      end case;
   end record;

   type Widget_Access is access Widget;

   type Template is array (Positive range <>) of Widget;
   
   --
   -- Callbacks
   --
   
   type Classic_Mouse_Down is access procedure (Skin : in out Skin_Type; x, y : Integer);
   type Classic_Mouse_Up is access procedure (Skin : in out Skin_Type; x, y : Integer);
   type Classic_Mouse_Move is access procedure (Skin : in out Skin_Type; x, y : Integer);
   type Classic_Draw is access procedure (Skin : in out Skin_Type);
   type Classic_Resize is access procedure (Skin : in out Skin_Type; w, h : Integer);
   type Classic_Focus is access procedure (Skin : in out Skin_Type; focus : Boolean);
   
   type Skin_Callbacks (T : Skin) is record
      case T is
         when Classic_Skin =>
            mouse_down : Classic_Mouse_Down;
            mouse_up: Classic_Mouse_Up;
            mouse_move: Classic_Mouse_Move;
            draw: Classic_Draw;
            focus: Classic_Focus;
            resize: Classic_Resize;
         when Modern_Skin =>
            null;
      end case;
   end record;
   
   --
   -- GUI
   --
   type Gui_Init is access procedure;
   type Gui_Quit is access procedure;
   
   type Gui_Create_Window is access function (x : Integer; y: Integer; w : Integer; h : Integer; title : String; callbacks : in Skin_Callbacks) return Window_Type;
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
   type Gui_Draw_Text is access procedure (x, y, w, h : Integer; Text : String);
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
      Draw_Text : Gui_Draw_Text;
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
   
   type Window_ID is (MAIN_WINDOW_ID, EQUALIZER_WINDOW_ID, PLAYLIST_WINDOW_ID);
   
   type Window_Array is array (Window_ID) of Window_Type;
   type Template_Array is array (Window_ID) of access Template;
   
   type Skin_Type (T : Skin) is record
      GUI : Gui_Dispatch;
      --main_window : Window_Type;
      
      case T is
         when Classic_Skin =>
            -- These are writable so they can't be global on SPARK.
            Windows : Window_Array;
            Templates : Template_Array;
            
            bmps : Bitmap_Array_Type;
            cursors : Cursor_Array_Type;
            double_size : Boolean := False;
            easymove : Boolean := True;
            last_x : Natural;
            last_y : Natural;
            capture : Natural;
            main_shade : Boolean := False;
            eq_shade : Boolean := False;
            pl_shade : Boolean := False;
         when Modern_Skin =>
            null;
      end case;
   end record;
   
   gui : Gui_Dispatch;
   main_window : Window_Type;
   Skin1 : aliased Skin_Type (T => Classic_Skin);
   Current_Skin : access Skin_Type := Skin1'Access;

end Overkill.Gui;
