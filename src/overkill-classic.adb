with Overkill.Debug;
use Overkill.Debug;
with Overkill.Playback;
with Overkill.Platform;
use Overkill.Platform;

package body Overkill.Classic is
   --
   -- Enumerations
   --
   type Bitmap is
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
      BMP_VOLUME,
      NUM_BMPS);

   type Cursor is
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
      CURSORWINBUT,
      CURSOR_WSCLOSE,
      CURSOR_WSMIN,
      CURSOR_WSNORMAL,
      CURSOR_WSPOSBAR,
      CURSOR_WSWINBUT,
      NUM_CURSORS);

   --
   -- Records
   --
   type Subbitmap is record
      bmp : Natural;
      X, Y : Natural;
   end record;

   type Subbitmap_Array is array (Natural range <>) of access Subbitmap;

   type Rect is array (1..4) of Natural;

   type Handler;

   type Widget_Type is
     (Background_Widget,
      Button_Widget,
      Checkbox_Widget,
      Slider_Widget,
      Clutterbar_Widget,
      Song_Title_Widget,
      Scroll_Widget);

   type Button_Action_Func is access procedure;
   type Checkbox_Action_Func is access procedure (checked : Boolean);
   type Slider_Action_Func is access procedure (value : Integer);

   type Clutterbar_O is access procedure;
   type Clutterbar_A is access procedure (a : Window_Type; b : Boolean);
   type Clutterbar_I is access procedure;
   type Clutterbar_D is access procedure (a : Boolean);
   type Clutterbar_V is access procedure;

   type Widget(T : Widget_Type := Background_Widget) is record
      r : Rect;
      c : Cursor;
      control : access constant Handler;
      case T is
         when Background_Widget =>
            subbmp : access constant Subbitmap;
            move_window : Boolean;
         when Button_Widget =>
            button_up : access Subbitmap;
            button_down : access Subbitmap;
            button_action : Button_Action_Func;
         when Checkbox_Widget =>
            checkbox_on_up : access Subbitmap;
            checkbox_on_down : access Subbitmap;
            checkbox_off_up : access Subbitmap;
            checkbox_off_down : access Subbitmap;
            checkbox_checked : Boolean;
            checkbox_action : Checkbox_Action_Func;
         when Slider_Widget =>
            slider_background : access constant Subbitmap_Array;
            slider_up : access Subbitmap;
            slider_down : access Subbitmap;
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
            song_title_string : access String;
            song_title_offset : Integer;
         when Scroll_Widget =>
            scroll_background : access Subbitmap;
            scroll_bar_up : access Subbitmap;
            scroll_bar_down : access Subbitmap;
            scroll_bar_length : Natural;
            scroll_value : Natural;
      end case;
   end record;

   type Mouse_Down_Handler_Func is access procedure (wid : access Widget; win : Window_Type; X, Y : Integer);
   type Mouse_Up_Handler_Func is access procedure (wid : access Widget; win : Window_Type; X, Y : Integer);
   type Mouse_Move_Handler_Func is access procedure (wid : access Widget; win : Window_Type; X, Y : Integer);
   type Draw_Handler_Func is access procedure (wid : access Widget; win : Window_Type);

   type Handler is record
	mouse_down : Mouse_Down_Handler_Func;
	mouse_up : Mouse_Up_Handler_Func;
	mouse_move : Mouse_Move_Handler_Func;
	draw : Draw_Handler_Func;
   end record;

   type Template is array (Positive range <>) of aliased Widget;

   type Resizeable_Background_Data is record
      top_left : access Subbitmap;
      title : access Subbitmap;
      top : access Subbitmap;
      top_right : access Subbitmap;
      left : access Subbitmap;
      right : access Subbitmap;
      bottom : access Subbitmap;
      bottom_left : access Subbitmap;
      bottom_right : access Subbitmap;
      length_top : Natural;
      length_left : Natural;
      length_right : Natural;
      length_bottom : Natural;
   end record;

   type Byte is range 0..255;
   type Color is array (1..3) of Byte;

   type Pledit_Data is record
      normal_color : Color;
      current_color : Color;
      cur_line : Natural;
   end record;

   type Menu_Data is record
      num_buttons : Integer;
      cur_button : Integer;
      buttons_up : access Subbitmap;
      buttons_down : access Subbitmap;
      bar : access Subbitmap;
   end record;

   type Grip_Data is record
      min_width : Natural;
      min_heigth : Natural;
   end record;

   type Scroll_Data is record
      background : access Subbitmap;
      bar_up : access Subbitmap;
      bar_down : access Subbitmap;
      bar_length : Natural;
      value : Integer;
   end record;

   bmp_files : array (Natural range <>) of access String :=
     (new String'("BALANCE.BMP"),
      new String'("CBUTTONS.BMP"),
      new String'("EQ_EX.BMP"),
      new String'("EQMAIN.BMP"),
      new String'("EQMAIN_ISO.BMP"),
      new String'("GEN.BMP"),
      new String'("GENEX.BMP"),
      new String'("MAIN.BMP"),
      new String'("MB.BMP"),
      new String'("MONOSTER.BMP"),
      new String'("NUMBERS.BMP"),
      new String'("NUMS_EX.BMP"),
      new String'("PLAYPAUS.BMP"),
      new String'("PLEDIT.BMP"),
      new String'("POSBAR.BMP"),
      new String'("SHUFREP.BMP"),
      new String'("TEXT.BMP"),
      new String'("TITLEBAR.BMP"),
      new String'("VIDEO.BMP"),
      new String'("VOLUME.BMP"));

   cursor_files : constant array (Natural range <>) of access constant String :=
     (new String'("CLOSE.CUR"),
      new String'("EQCLOSE.CUR"),
      new String'("EQNORMAL.CUR"),
      new String'("EQSLID.CUR"),
      new String'("EQTITLE.CUR"),
      new String'("MAINMENU.CUR"),
      new String'("MIN.CUR"),
      new String'("NORMAL.CUR"),
      new String'("PCLOSE.CUR"),
      new String'("PNORMAL.CUR"),
      new String'("POSBAR.CUR"),
      new String'("PSIZE.CUR"),
      new String'("PTBAR.CUR"),
      new String'("PVSCROLL.CUR"),
      new String'("PWINBUT.CUR"),
      new String'("PWSNORM.CUR"),
      new String'("PWSSIZE.CUR"),
      new String'("SONGNAME.CUR"),
      new String'("TITLEBAR.CUR"),
      new String'("VOLBAL.CUR"),
      new String'("VOLBAR.CUR"),
      new String'("WINBUT.CUR"),
      new String'("WSCLOSE.CUR"),
      new String'("WSMIN.CUR"),
      new String'("WSNORMAL.CUR"),
      new String'("WSPOSBAR.CUR"),
      new String'("WSWINBUT.CUR"));

   w1 : Window_Type;
   w2 : Window_Type;
   w3 : Window_Type;

   bmps : array (0..Bitmap'Pos(NUM_BMPS)) of gui.Pixmap;
   cursors : array (0..Cursor'Pos(NUM_CURSORS)) of gui.Cursor;
   double_size : Boolean := False;
   easymove : Boolean := True;
   last_x : Natural;
   last_y : Natural;
   capture : access Widget;
   main_shade : Boolean := False;
   eq_shade : Boolean := False;
   pl_shade : Boolean := False;

   subbmp_main : constant Subbitmap := (Bitmap'Pos(BMP_MAIN), 0, 0);
   subbmp_title_bar_on : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 27, 0);
   subbmp_title_bar_off : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 27, 15);
   subbmp_title_bar_shade_on : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 27, 29);
   subbmp_title_bar_shade_off : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 27, 42);
   subbmp_title_bar_easter_on : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 27, 57);
   subbmp_title_bar_easter_off : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 27, 72);
   subbmp_a : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 304, 0);
   subbmp_mono_on : constant Subbitmap := (Bitmap'Pos(BMP_MONOSTER), 29, 0);
   subbmp_mono_off : constant Subbitmap := (Bitmap'Pos(BMP_MONOSTER), 29, 12);
   subbmp_stereo_on : constant Subbitmap := (Bitmap'Pos(BMP_MONOSTER), 0, 0);
   subbmp_stereo_off : constant Subbitmap := (Bitmap'Pos(BMP_MONOSTER), 0, 12);
   subbmp_previous_up : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 0, 0);
   subbmp_previous_down : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 0, 18);
   subbmp_play_up : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 22+23*0, 0);
   subbmp_play_down : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 22+23*0, 18);
   subbmp_pause_up : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 22+23*1, 0);
   subbmp_pause_down : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 22+23*1, 18);
   subbmp_stop_up : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 22+23*2, 0);
   subbmp_stop_down : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 22+23*2, 18);
   subbmp_next_up : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 22+23*3, 0);
   subbmp_next_down : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 22+23*3, 18);
   subbmp_eject_up : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 22+23*4, 0);
   subbmp_eject_down : constant Subbitmap := (Bitmap'Pos(BMP_CBUTTONS), 22+23*4, 16);
   subbmp_status : constant Subbitmap := (Bitmap'Pos(BMP_PLAYPAUS), 9*2, 0);
   subbmp_volume_bg1 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*0);
   subbmp_volume_bg2 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*1);
   subbmp_volume_bg3 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*2);
   subbmp_volume_bg4 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*3);
   subbmp_volume_bg5 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*4);
   subbmp_volume_bg6 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*5);
   subbmp_volume_bg7 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*6);
   subbmp_volume_bg8 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*7);
   subbmp_volume_bg9 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*8);
   subbmp_volume_bg10 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*9);
   subbmp_volume_bg11 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*10);
   subbmp_volume_bg12 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*11);
   subbmp_volume_bg13 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*12);
   subbmp_volume_bg14 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*13);
   subbmp_volume_bg15 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*14);
   subbmp_volume_bg16 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*15);
   subbmp_volume_bg17 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*16);
   subbmp_volume_bg18 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*17);
   subbmp_volume_bg19 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*18);
   subbmp_volume_bg20 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*19);
   subbmp_volume_bg21 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*20);
   subbmp_volume_bg22 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*21);
   subbmp_volume_bg23 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*22);
   subbmp_volume_bg24 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*23);
   subbmp_volume_bg25 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*24);
   subbmp_volume_bg26 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*25);
   subbmp_volume_bg27 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*26);
   subbmp_volume_bg28 : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 15*27);
   subbmp_volume_bar_up : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 15, 422);
   subbmp_volume_bar_down : constant Subbitmap := (Bitmap'Pos(BMP_VOLUME), 0, 422);
   subbmp_balance_bg1 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*0);
   subbmp_balance_bg2 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*1);
   subbmp_balance_bg3 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*2);
   subbmp_balance_bg4 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*3);
   subbmp_balance_bg5 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*4);
   subbmp_balance_bg6 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*5);
   subbmp_balance_bg7 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*6);
   subbmp_balance_bg8 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*7);
   subbmp_balance_bg9 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*8);
   subbmp_balance_bg10 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*9);
   subbmp_balance_bg11 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*10);
   subbmp_balance_bg12 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*11);
   subbmp_balance_bg13 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*12);
   subbmp_balance_bg14 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*13);
   subbmp_balance_bg15 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*14);
   subbmp_balance_bg16 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*15);
   subbmp_balance_bg17 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*16);
   subbmp_balance_bg18 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*17);
   subbmp_balance_bg19 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*18);
   subbmp_balance_bg20 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*19);
   subbmp_balance_bg21 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*20);
   subbmp_balance_bg22 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*21);
   subbmp_balance_bg23 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*22);
   subbmp_balance_bg24 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*23);
   subbmp_balance_bg25 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*24);
   subbmp_balance_bg26 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*25);
   subbmp_balance_bg27 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*26);
   subbmp_balance_bg28 : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 9, 15*27);
   subbmp_balance_bar_up : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 15, 422);
   subbmp_balance_bar_down : constant Subbitmap := (Bitmap'Pos(BMP_BALANCE), 0, 422);
   subbmp_eq_off_up : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 0, 61);
   subbmp_eq_off_down : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 46, 61);
   subbmp_eq_on_up : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 0, 73);
   subbmp_eq_on_down : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 46, 73);
   subbmp_pl_off_up : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 23, 61);
   subbmp_pl_off_down : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 70, 61);
   subbmp_pl_on_up : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 23, 73);
   subbmp_pl_on_down : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 70, 73);
   subbmp_shuffle_off_up : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 29, 0);
   subbmp_shuffle_off_down : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 29, 15);
   subbmp_shuffle_on_up : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 29, 15*2);
   subbmp_shuffle_on_down : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 29, 15*3);
   subbmp_repeat_off_up : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 0, 0);
   subbmp_repeat_off_down : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 0, 15);
   subbmp_repeat_on_up : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 0, 15*2);
   subbmp_repeat_on_down : constant Subbitmap := (Bitmap'Pos(BMP_SHUFREP), 0, 15*3);
   subbmp_clutterbar_off : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 304, 0);
   subbmp_clutterbar_disabled : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 312, 0);
   subbmp_clutterbar_off_o : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 304, 44);
   subbmp_clutterbar_off_a : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 312, 44);
   subbmp_clutterbar_off_i : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 320, 44);
   subbmp_clutterbar_off_d : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 328, 44);
   subbmp_clutterbar_off_v : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 336, 44);
   subbmp_clutterbar_o : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 304, 45);
   subbmp_clutterbar_a : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 312, 53);
   subbmp_clutterbar_i : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 320, 61);
   subbmp_clutterbar_d : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 328, 69);
   subbmp_clutterbar_v : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 336, 77);
   subbmp_status_play : constant Subbitmap := (Bitmap'Pos(BMP_PLAYPAUS), 0, 0);
   subbmp_status_pause : constant Subbitmap := (Bitmap'Pos(BMP_PLAYPAUS), 9, 0);
   subbmp_status_stop : constant Subbitmap := (Bitmap'Pos(BMP_PLAYPAUS), 18, 0);
   subbmp_status_red_on : constant Subbitmap := (Bitmap'Pos(BMP_PLAYPAUS), 0, 0);
   subbmp_status_red_off : constant Subbitmap := (Bitmap'Pos(BMP_PLAYPAUS), 0, 0);
   subbmp_status_green_on : constant Subbitmap := (Bitmap'Pos(BMP_PLAYPAUS), 0, 0);
   subbmp_status_green_off : constant Subbitmap := (Bitmap'Pos(BMP_PLAYPAUS), 0, 0);
   subbmp_options_up : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 0, 0);
   subbmp_options_down : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 0, 9);
   subbmp_minimize_up : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 9, 0);
   subbmp_minimize_down : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 9, 9);
   subbmp_close_up : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 18, 0);
   subbmp_close_down : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 18, 9);
   subbmp_maximize_normal_up : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 0, 18);
   subbmp_maximize_normal_down : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 9, 18);
   subbmp_maximize_ws_up : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 0, 27);
   subbmp_maximize_ws_down : constant Subbitmap := (Bitmap'Pos(BMP_TITLEBAR), 9, 27);
   subbmp_posbar_background : constant Subbitmap := (Bitmap'Pos(BMP_POSBAR), 0, 0);
   subbmp_posbar_bar_up : constant Subbitmap := (Bitmap'Pos(BMP_POSBAR), 248, 0);
   subbmp_posbar_bar_down : constant Subbitmap := (Bitmap'Pos(BMP_POSBAR), 277, 0);

   subbmp_eq_background : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 0, 0);
   subbmp_eq_title_bar_on : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 0, 134);
   subbmp_eq_title_bar_off : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 0, 149);
   subbmp_eq_on_on_up : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 69, 119);
   subbmp_eq_on_on_down : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 187, 119);
   subbmp_eq_on_off_up : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 10, 119);
   subbmp_eq_on_off_down : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 128, 119);
   subbmp_eq_auto_on_up : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 95, 119);
   subbmp_eq_auto_on_down : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 213, 119);
   subbmp_eq_auto_off_up : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 36, 119);
   subbmp_eq_auto_off_down : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 154, 119);
   subbmp_eq_presets_up : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 224, 164);
   subbmp_eq_presets_down : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 224, 176);
   subbmp_eq_preamp : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13, 164);
   subbmp_eq_slider_bg1 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*0, 164);
   subbmp_eq_slider_bg2 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*1, 164);
   subbmp_eq_slider_bg3 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*2, 164);
   subbmp_eq_slider_bg4 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*3, 164);
   subbmp_eq_slider_bg5 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*4, 164);
   subbmp_eq_slider_bg6 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*5, 164);
   subbmp_eq_slider_bg7 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*6, 164);
   subbmp_eq_slider_bg8 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*7, 164);
   subbmp_eq_slider_bg9 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*8, 164);
   subbmp_eq_slider_bg10 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*9, 164);
   subbmp_eq_slider_bg11 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*10, 164);
   subbmp_eq_slider_bg12 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*11, 164);
   subbmp_eq_slider_bg13 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*12, 164);
   subbmp_eq_slider_bg14 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*13, 164);
   subbmp_eq_slider_bg15 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*0, 229);
   subbmp_eq_slider_bg16 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*1, 229);
   subbmp_eq_slider_bg17 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*2, 229);
   subbmp_eq_slider_bg18 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*3, 229);
   subbmp_eq_slider_bg19 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*4, 229);
   subbmp_eq_slider_bg20 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*5, 229);
   subbmp_eq_slider_bg21 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*6, 229);
   subbmp_eq_slider_bg22 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*7, 229);
   subbmp_eq_slider_bg23 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*8, 229);
   subbmp_eq_slider_bg24 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*9, 229);
   subbmp_eq_slider_bg25 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*10, 229);
   subbmp_eq_slider_bg26 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*11, 229);
   subbmp_eq_slider_bg27 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*12, 229);
   subbmp_eq_slider_bg28 : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 13+15*13, 229);
   subbmp_eq_slider_up : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 0, 164);
   subbmp_eq_slider_down : constant Subbitmap := (Bitmap'Pos(BMP_EQMAIN), 0, 176);

   subbmp_pl_top_left_on : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 0, 0);
   subbmp_pl_title_on : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 26, 0);
   subbmp_pl_top_on : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 127, 0);
   subbmp_pl_top_right_on : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 153, 0);
   subbmp_pl_top_left_off : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 0, 21);
   subbmp_pl_title_off : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 26, 21);
   subbmp_pl_top_off : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 127, 21);
   subbmp_pl_top_right_off : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 153, 21);
   subbmp_pl_left : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 0, 42);
   subbmp_pl_right : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 26, 42);
   subbmp_pl_bottom : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 179, 0);
   subbmp_pl_bottom_left : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 0, 72);
   subbmp_pl_bottom_right : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 126, 72);
   subbmp_pl_menu_add_url_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 0, 111);
   subbmp_pl_menu_add_dir_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 0, 130);
   subbmp_pl_menu_add_file_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 0, 149);
   subbmp_pl_menu_add_url_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 23, 111);
   subbmp_pl_menu_add_dir_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 23, 130);
   subbmp_pl_menu_add_file_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 23, 149);
   subbmp_pl_menu_add_bar : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 48, 111);
   subbmp_pl_menu_rem_all_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 54, 111);
   subbmp_pl_menu_rem_crop_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 54, 130);
   subbmp_pl_menu_rem_sel_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 54, 149);
   subbmp_pl_menu_rem_misc_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 54, 168);
   subbmp_pl_menu_rem_all_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 77, 111);
   subbmp_pl_menu_rem_crop_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 77, 130);
   subbmp_pl_menu_rem_sel_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 77, 149);
   subbmp_pl_menu_rem_misc_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 77, 168);
   subbmp_pl_menu_rem_bar : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 100, 111);
   subbmp_pl_menu_sel_inv_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 104, 111);
   subbmp_pl_menu_sel_zero_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 104, 130);
   subbmp_pl_menu_sel_all_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 104, 149);
   subbmp_pl_menu_sel_inv_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 127, 111);
   subbmp_pl_menu_sel_zero_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 127, 130);
   subbmp_pl_menu_sel_all_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 127, 149);
   subbmp_pl_menu_sel_bar : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 150, 111);
   subbmp_pl_menu_misc_sort_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 154, 111);
   subbmp_pl_menu_misc_inf_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 154, 130);
   subbmp_pl_menu_misc_opts_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 154, 149);
   subbmp_pl_menu_misc_sort_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 177, 111);
   subbmp_pl_menu_misc_inf_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 177, 130);
   subbmp_pl_menu_misc_opts_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 177, 149);
   subbmp_pl_menu_misc_bar : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 200, 111);
   subbmp_pl_menu_list_new_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 204, 111);
   subbmp_pl_menu_list_save_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 204, 130);
   subbmp_pl_menu_list_load_up : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 204, 149);
   subbmp_pl_menu_list_new_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 227, 111);
   subbmp_pl_menu_list_save_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 227, 130);
   subbmp_pl_menu_list_load_down : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 227, 149);
   subbmp_pl_menu_list_bar : constant Subbitmap := (Bitmap'Pos(BMP_PLEDIT), 250, 111);

   -- TODO: bitmap font.

   --
   -- Widget Actions
   --

   procedure Test_Button is
   begin
      Put_Line("Test Button");
   end Test_Button;

   procedure Test_Checkbox(state : Boolean) is
   begin
      Put_Line("Test Checkbox: " & state'Image);
   end Test_Checkbox;

   procedure Cmd_Main_Minimize is
   begin
      gui.gui.minimize_window(w1);
      gui.gui.hide_window(w2);
      gui.gui.hide_window(w3);
   end Cmd_Main_Minimize;

   procedure Cmd_Main_Maximize is
   begin
      main_shade := not main_shade;
      if main_shade then
         gui.gui.resize_window(w1, 275,14);
      else
         gui.gui.resize_window(w1, 275, 116);
      end if;
   end Cmd_Main_Maximize;

   procedure Cmd_Main_Close is
   begin
      gui.gui.destroy_window(w1);
   end Cmd_Main_Close;

   procedure Cmd_Main_Previous is
   begin
      Playback.Previous;
   end Cmd_Main_Previous;

   procedure Cmd_Main_Play is
   begin
      Playback.Play;
   end Cmd_Main_Play;

   procedure Cmd_Main_Pause is
   begin
      Playback.Pause;
   end Cmd_Main_Pause;

   procedure Cmd_Main_Stop is
   begin
      Playback.Stop;
   end Cmd_Main_Stop;

   procedure Cmd_Main_Next is
   begin
      Playback.Next;
   end Cmd_Main_Next;

   procedure Cmd_Main_Eject is
   begin
      gui.gui.open_file_dialog.all;
   end Cmd_Main_Eject;

   procedure Cmd_Main_Eq(checked : Boolean) is
   begin
      if checked then
         gui.gui.show_window(w2);
      else
         gui.gui.hide_window(w2);
      end if;
   end Cmd_Main_Eq;

   procedure Cmd_Main_Pl(checked : Boolean) is
   begin
      if checked then
         gui.gui.show_window(w3);
      else
         gui.gui.hide_window(w3);
      end if;
   end Cmd_Main_Pl;

   procedure Clutterbar_Set_O is
   begin
      null;
   end Clutterbar_Set_O;

   procedure Clutterbar_Set_A(a : Window_Type; b : Boolean) is
   begin
      null;
   end Clutterbar_Set_A;

   procedure Clutterbar_Set_I is
   begin
      null;
   end Clutterbar_Set_I;

   procedure Clutterbar_Set_D(a : Boolean) is
   begin
      null;
   end Clutterbar_Set_D;

   procedure Clutterbar_Set_V is
   begin
      null;
   end Clutterbar_Set_V;

   --
   -- Event handlers
   --

   procedure Draw_Time(
                       minutes, seconds : Integer;
                       remaining_time : Boolean
                      )
   is
      bmp : Pixmap;
   begin
      bmp := bmps(Bitmap'Pos(BMP_NUMS_EX));
      gui.gui.draw_image(bmp, 48, 26, 9, 13, 9 * (minutes / 10), 0);
      gui.gui.draw_image(bmp, 60, 26, 9, 13, 9 * (minutes mod 10), 0);
      gui.gui.draw_image(bmp, 78, 26, 9, 13, 9 * (seconds / 10), 0);
      gui.gui.draw_image(bmp, 90, 26, 9, 13, 9 * (seconds mod 10), 0);
   end Draw_Time;

   procedure Capture_Mouse(win : Window_Type; wid : access Widget) is
   begin
      gui.gui.capture_mouse(win);
      capture := wid;
   end Capture_Mouse;

   procedure Release_Mouse is
   begin
      gui.gui.release_mouse.all;
      capture := null;
   end Release_Mouse;

   procedure Draw_Pixmap
     (subbmp : access Subbitmap;
      dst_x, dst_y : Integer;
      w, h : Integer)
   is
      id : Integer;
   begin
      if subbmp /= null then
         id := subbmp.bmp;
         gui.gui.draw_image(bmps(id), dst_x, dst_y, w, h, subbmp.X, subbmp.Y);
      end if;
   end Draw_Pixmap;

   procedure Draw_Pixmap_Double
     (subbmp : access constant Subbitmap;
      dst_x, dst_y : Integer;
      w, h : Integer)
   is
      id : Integer;
   begin
      if subbmp /= null then
         id := subbmp.bmp;
         if double_size = False then
            gui.gui.draw_image(bmps(id), dst_x, dst_y, w, h, subbmp.X, subbmp.Y);
         else
            gui.gui.draw_image_double(bmps(id), dst_x, dst_y, w, h, subbmp.X, subbmp.Y);
         end if;
      end if;
   end Draw_Pixmap_Double;

   procedure Draw_Pixmap_Loop_Horizontal
     (subbmp : access Subbitmap;
      dst_x, dst_y : Integer;
      src_w, src_h : Integer;
      dst_w : Integer)
   is
      I : Integer := 0;
   begin
      while I < dst_w loop
         Draw_Pixmap(subbmp, dst_x + I, dst_y, src_w, src_h);
         I := I + src_w;
      end loop;
   end Draw_Pixmap_Loop_Horizontal;

   procedure Draw_Pixmap_Loop_Vertical
     (subbmp : access Subbitmap;
      dst_x, dst_y : Integer;
      src_w, src_h : Integer;
      dst_h : Integer)
   is
      I : Integer := 0;
   begin
      while I < dst_h loop
         Draw_Pixmap(subbmp, dst_x, dst_y + I, src_w, src_h);
         I := I + src_h;
      end loop;
   end Draw_Pixmap_Loop_Vertical;

   procedure Background_Mouse_Down
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer) is
   begin
      if Y < 16 or easymove then
         Capture_Mouse(win, wid);
         last_x := X;
         last_y := Y;
      end if;
   end Background_Mouse_Down;

   procedure Background_Mouse_Up
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer) is
   begin
      if Y < 16 or easymove then
         Release_Mouse;
      end if;
   end Background_Mouse_Up;

   procedure Background_Mouse_Move
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer)
   is
      A, B : Integer;
   begin
      if capture = wid then
         if gui.gui.check_glue(win, w1, X - last_x, Y - last_y) = False then
            gui.gui.move_window(win, x - last_x, y - last_y);
            A := X;
            B := Y;
         end if;

         if win = w1 then
            gui.gui.move_window(w2, X - last_x, Y - last_y);
            gui.gui.move_window(w3, X - last_x, Y - last_y);
         end if;
      end if;
   end Background_Mouse_Move;

   procedure Background_Draw(wid : access Widget; win : Window_Type) is
      r : access constant Rect;
   begin
      r := new Rect'(wid.r);
      Draw_Pixmap_Double(wid.subbmp, r(1), r(2), r(3), r(4));
   end Background_Draw;

   procedure Button_Mouse_Down
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer) is
   begin
      Capture_Mouse (win, wid);
      gui.gui.redraw_window (win);
   end Button_Mouse_Down;

   procedure Button_Mouse_Up
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer) is
   begin
      if Capture = wid then
         Release_Mouse;
         gui.gui.redraw_window (win);
         if X > wid.r (1) and X < wid.r (1) + wid.r (3) and
           Y > wid.r (2) and Y < wid.r (2) + wid.r (4) then
            if wid.button_action /= null then
               wid.button_action.all;
            end if;
         end if;
      end if;
   end Button_Mouse_Up;

   procedure Button_Mouse_Move
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer) is
   begin
      null;
   end Button_Mouse_Move;

   procedure Button_Draw(wid : access Widget; win : Window_Type)
   is
      r : Rect := wid.r;
      subbmp : access Subbitmap;
   begin
      if capture = wid then
         subbmp := wid.button_down;
      else
         subbmp := wid.button_up;
      end if;
      Draw_Pixmap_Double(subbmp, r(1), r(2), r(3), r(4));
   end Button_Draw;

   procedure Checkbox_Mouse_Down
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer) is
   begin
      Capture_Mouse (win, wid);
      gui.gui.redraw_window (win);
   end Checkbox_Mouse_Down;

   procedure Checkbox_Mouse_Up
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer) is
   begin
      if Capture = wid then
         Release_Mouse;
         gui.gui.redraw_window (win);
         if X > wid.r (1) and X < wid.r (1) + wid.r (3) and
           Y > wid.r (2) and Y < wid.r (2) + wid.r (4) then
            wid.checkbox_checked := not wid.checkbox_checked;
            if wid.checkbox_action /= null then
               wid.checkbox_action (wid.checkbox_checked);
            end if;
         end if;
      end if;
   end Checkbox_Mouse_Up;

   procedure Checkbox_Mouse_Move(
                                 wid : access Widget;
                                   win : Window_Type;
                                   X, Y : Integer
                                ) is
   begin
      null;
   end Checkbox_Mouse_Move;

   procedure Checkbox_Draw(wid : access Widget; win : Window_Type)
   is
      r : Rect;
      subbmp : access Subbitmap;
   begin
      r := wid.r;
      if wid.checkbox_checked then
         if capture = wid then
            subbmp := wid.checkbox_on_down;
         else
            subbmp := wid.checkbox_on_up;
         end if;
      else
         if capture = wid then
            subbmp := wid.checkbox_off_down;
         else
            subbmp := wid.checkbox_off_up;
         end if;
      end if;

      Draw_Pixmap_Double(subbmp, r(1), r(2), r(3), r(4));
   end Checkbox_Draw;

   procedure Slider_Mouse_Down
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer) is
   begin
      Capture_Mouse (win, wid);
      Last_X := X;
      Last_Y := Y;
      gui.gui.Redraw_Window (win);
   end Slider_Mouse_Down;

   procedure Slider_Mouse_Up
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer) is
   begin
      Release_Mouse;
      gui.gui.Redraw_Window (win);
   end Slider_Mouse_Up;

   procedure Slider_Mouse_Move
     (wid : access Widget;
      win : Window_Type;
      X, Y : Integer)
   is
      Difference : Integer;
      Value : Integer;
   begin
      Put_Line ("wid.slider_value " & wid.slider_value'Image);
      if Capture = wid then
         if wid.slider_horizontal = True then
            Difference := X - Last_X;
            Value := wid.slider_value + Difference;
         else
            Difference := Y - Last_Y;
            Value := wid.slider_value - Difference;
         end if;

         if Value < wid.slider_min then
            wid.slider_value := wid.slider_min;
         elsif Value > wid.slider_max then
            wid.slider_value := wid.slider_max;
         else
            Last_X := X;
            Last_Y := Y;
            wid.slider_value := Value;
         end if;

         Put_Line ("value: " & wid.slider_value'Image);

         gui.gui.redraw_window (win);
      end if;
   end Slider_Mouse_Move;

   procedure Slider_Draw(wid : access Widget; win : Window_Type) is
      r : Rect;
      bg, bar : access Subbitmap;
      n, slider_range : Natural;
   begin
      Put_Line ("Slider_Draw: wid.slider_value: " & wid.slider_value'Image);
      r := wid.r;

      -- select the background
      -- there are 28 possible bg images
      if wid.slider_horizontal then
         slider_range := wid.slider_max - wid.slider_min;
         n := wid.slider_value * 28 / slider_range;
         if n > 27 then
            n := 27;
         end if;
      else
         slider_range := wid.slider_max - wid.slider_min;
         n := wid.slider_value * 28 / slider_range;
         if n > 27 then
            n := 27;
         end if;
      end if;
      bg := wid.slider_background(n);

      if capture = wid then
         bar := wid.slider_down;
      else
         bar := wid.slider_up;
      end if;

      -- Draw the background
      Draw_Pixmap_Double(bg, r(1), r(2), r(3), r(4));
      -- Draw the bar
      if wid.slider_horizontal = True then
         Draw_Pixmap_Double(bar, r(1) + wid.slider_value + wid.slider_min, r(2) + 1, 14, 11);
         null;
      else
         Draw_Pixmap_Double(bar, r(1) + 1, r(2) + r(4) - 13 - wid.slider_value, 11, 11);
      end if;
   end Slider_Draw;

   procedure Clutterbar_Mouse_Down(
                                   wid : access Widget;
                                   win : Window_Type;
                                   X, Y : Integer
                                  ) is
   begin
      null;
   end Clutterbar_Mouse_Down;

   procedure Clutterbar_Mouse_Up(
                                 wid : access Widget;
                                   win : Window_Type;
                                   X, Y : Integer
                                ) is
   begin
      null;
   end Clutterbar_Mouse_Up;

   procedure Clutterbar_Mouse_Move(
                                   wid : access Widget;
                                   win : Window_Type;
                                   X, Y : Integer
                                  ) is
   begin
      null;
   end Clutterbar_Mouse_Move;

   procedure Clutterbar_Draw(wid : access Widget; win : Window_Type) is
   begin
      null;
   end Clutterbar_Draw;

   procedure Song_Title_Mouse_Down(
                                  wid : access Widget;
                                   win : Window_Type;
                                   X, Y : Integer
                                  ) is
   begin
      null;
   end Song_Title_Mouse_Down;

   procedure Song_Title_Mouse_Up(
                                wid : access Widget;
                                   win : Window_Type;
                                 X, Y : Integer
                                ) is
   begin
      null;
   end Song_Title_Mouse_Up;

   procedure Song_Title_Mouse_Move(
                                  wid : access Widget;
                                   win : Window_Type;
                                   X, Y : Integer
                                  ) is
   begin
      null;
   end Song_Title_Mouse_Move;

   procedure Song_Title_Draw(wid : access Widget; win : Window_Type) is
   begin
      null;
   end Song_Title_Draw;

   procedure Scroll_Mouse_Down(
                              wid : access Widget;
                                   win : Window_Type;
                               X, Y : Integer
                              ) is
   begin
      null;
   end Scroll_Mouse_Down;

   procedure Scroll_Mouse_Up(
                            wid : access Widget;
                                   win : Window_Type;
                             X, Y : Integer
                            ) is
   begin
      null;
   end Scroll_Mouse_Up;

   procedure Scroll_Mouse_Move(
                              wid : access Widget;
                                   win : Window_Type;
                               X, Y : Integer
                              ) is
   begin
      null;
   end Scroll_Mouse_Move;

   procedure Scroll_Draw(wid : access Widget; win : Window_Type) is
   begin
      null;
   end Scroll_Draw;

   background_handlers : constant Handler := (
                                              mouse_down => Background_Mouse_Down'Access,
                                              mouse_up => Background_Mouse_Up'Access,
                                              mouse_move => Background_Mouse_Move'Access,
                                              draw => Background_Draw'Access
                                             );

   button_handlers : constant Handler := (
                                          mouse_down => Button_Mouse_Down'Access,
                                          mouse_up => Button_Mouse_Up'Access,
                                          mouse_move => Button_Mouse_Move'Access,
                                          draw => Button_Draw'Access
                                         );

   checkbox_handlers : constant Handler := (
                                            mouse_down => Checkbox_Mouse_Down'Access,
                                            mouse_up => Checkbox_Mouse_Up'Access,
                                            mouse_move => Checkbox_Mouse_Move'Access,
                                            draw => Checkbox_Draw'Access
                                           );

   slider_handlers : constant Handler := (
                                          mouse_down => Slider_Mouse_Down'Access,
                                          mouse_up => Slider_Mouse_Up'Access,
                                          mouse_move => Slider_Mouse_Move'Access,
                                          draw => Slider_Draw'Access
                                         );

   clutterbar_handlers : constant Handler := (
                                              mouse_down => Clutterbar_Mouse_Down'Access,
                                              mouse_up => Clutterbar_Mouse_Up'Access,
                                              mouse_move => Clutterbar_Mouse_Move'Access,
                                              draw => Clutterbar_Draw'Access
                                             );

   song_title_handlers : constant Handler := (
                                              mouse_down => Song_Title_Mouse_Down'Access,
                                              mouse_up => Song_Title_Mouse_Up'Access,
                                              mouse_move => Song_Title_Mouse_Move'Access,
                                              draw => Song_Title_Draw'Access
                                             );

   scroll_handlers : constant Handler := (
                                          mouse_down => Scroll_Mouse_Down'Access,
                                          mouse_up => Scroll_Mouse_Up'Access,
                                          mouse_move => Scroll_Mouse_Move'Access,
                                          draw => Scroll_Draw'Access
                                         );

   volume_backgrounds : constant Subbitmap_Array :=
     (new Subbitmap'(subbmp_volume_bg1),
      new Subbitmap'(subbmp_volume_bg2),
      new Subbitmap'(subbmp_volume_bg3),
      new Subbitmap'(subbmp_volume_bg4),
      new Subbitmap'(subbmp_volume_bg5),
      new Subbitmap'(subbmp_volume_bg6),
      new Subbitmap'(subbmp_volume_bg7),
      new Subbitmap'(subbmp_volume_bg8),
      new Subbitmap'(subbmp_volume_bg9),
      new Subbitmap'(subbmp_volume_bg10),
      new Subbitmap'(subbmp_volume_bg11),
      new Subbitmap'(subbmp_volume_bg12),
      new Subbitmap'(subbmp_volume_bg13),
      new Subbitmap'(subbmp_volume_bg14),
      new Subbitmap'(subbmp_volume_bg15),
      new Subbitmap'(subbmp_volume_bg16),
      new Subbitmap'(subbmp_volume_bg17),
      new Subbitmap'(subbmp_volume_bg18),
      new Subbitmap'(subbmp_volume_bg19),
      new Subbitmap'(subbmp_volume_bg20),
      new Subbitmap'(subbmp_volume_bg21),
      new Subbitmap'(subbmp_volume_bg22),
      new Subbitmap'(subbmp_volume_bg23),
      new Subbitmap'(subbmp_volume_bg24),
      new Subbitmap'(subbmp_volume_bg25),
      new Subbitmap'(subbmp_volume_bg26),
      new Subbitmap'(subbmp_volume_bg27),
      new Subbitmap'(subbmp_volume_bg28)
     );

   balance_backgrounds : constant Subbitmap_Array :=
     (new Subbitmap'(subbmp_balance_bg28),
      new Subbitmap'(subbmp_balance_bg26),
      new Subbitmap'(subbmp_balance_bg24),
      new Subbitmap'(subbmp_balance_bg22),
      new Subbitmap'(subbmp_balance_bg20),
      new Subbitmap'(subbmp_balance_bg18),
      new Subbitmap'(subbmp_balance_bg16),
      new Subbitmap'(subbmp_balance_bg14),
      new Subbitmap'(subbmp_balance_bg12),
      new Subbitmap'(subbmp_balance_bg10),
      new Subbitmap'(subbmp_balance_bg8),
      new Subbitmap'(subbmp_balance_bg6),
      new Subbitmap'(subbmp_balance_bg4),
      new Subbitmap'(subbmp_balance_bg1),
      new Subbitmap'(subbmp_balance_bg2),
      new Subbitmap'(subbmp_balance_bg4),
      new Subbitmap'(subbmp_balance_bg6),
      new Subbitmap'(subbmp_balance_bg8),
      new Subbitmap'(subbmp_balance_bg10),
      new Subbitmap'(subbmp_balance_bg12),
      new Subbitmap'(subbmp_balance_bg14),
      new Subbitmap'(subbmp_balance_bg16),
      new Subbitmap'(subbmp_balance_bg18),
      new Subbitmap'(subbmp_balance_bg20),
      new Subbitmap'(subbmp_balance_bg22),
      new Subbitmap'(subbmp_balance_bg24),
      new Subbitmap'(subbmp_balance_bg26),
      new Subbitmap'(subbmp_balance_bg28)
     );

   main_template : aliased Template :=
     ((Background_Widget,
      (0, 0, 275, 116),
      CURSOR_NORMAL,
      new Handler'(background_handlers),
      new Subbitmap'(subbmp_main),
      False),
      (Background_Widget,
       (0, 0, 275, 14),
       CURSOR_TITLEBAR,
       new Handler'(background_handlers),
       new Subbitmap'(subbmp_title_bar_off),
       False),
      (Background_Widget,
       (212, 41, 29, 12),
       CURSOR_NORMAL,
       new Handler'(background_handlers),
       new Subbitmap'(subbmp_mono_off),
       False),
      (Background_Widget,
       (939, 41, 29, 12),
       CURSOR_NORMAL,
       new Handler'(background_handlers),
       new Subbitmap'(subbmp_stereo_off),
       False),
      (Background_Widget,
       (26, 28, 9, 9),
       CURSOR_NORMAL,
       new Handler'(background_handlers),
       new Subbitmap'(subbmp_status_stop),
       False),
      (Button_Widget,
       (6, 3, 9, 9),
       CURSOR_NORMAL,
       new Handler'(button_handlers),
       new Subbitmap'(subbmp_options_up),
       new Subbitmap'(subbmp_options_down),
       Test_Button'Access),
      (Button_Widget,
       (244, 3, 9, 9),
       CURSOR_MIN,
       new Handler'(button_handlers),
       new Subbitmap'(subbmp_minimize_up),
       new Subbitmap'(subbmp_minimize_down),
       Cmd_Main_Minimize'Access),
      (Button_Widget,
       (254, 3, 9, 9),
       CURSOR_NORMAL,
       new Handler'(button_handlers),
       new Subbitmap'(subbmp_maximize_normal_up),
       new Subbitmap'(subbmp_maximize_normal_down),
       Cmd_Main_Maximize'Access),
      (Button_Widget,
       (264, 3, 9, 9),
       CURSOR_CLOSE,
       new Handler'(button_handlers),
       new Subbitmap'(subbmp_close_up),
       new Subbitmap'(subbmp_close_down),
       Cmd_Main_Close'Access),
      (Button_Widget,
       (16+23*0, 88, 23, 18),
       CURSOR_NORMAL,
       new Handler'(button_handlers),
       new Subbitmap'(subbmp_previous_up),
       new Subbitmap'(subbmp_previous_down),
       Cmd_Main_Previous'Access),
      (Button_Widget,
       (16+23*1-1, 88, 23, 18),
       CURSOR_NORMAL,
       new Handler'(button_handlers),
       new Subbitmap'(subbmp_play_up),
       new Subbitmap'(subbmp_play_down),
       Cmd_Main_Play'Access),
      (Button_Widget,
       (16+23*2-1, 88, 23, 18),
       CURSOR_NORMAL,
       new Handler'(button_handlers),
       new Subbitmap'(subbmp_pause_up),
       new Subbitmap'(subbmp_pause_down),
       Cmd_Main_Pause'Access),
      (Button_Widget,
       (16+23*3-1, 88, 23, 18),
       CURSOR_NORMAL,
       new Handler'(button_handlers),
       new Subbitmap'(subbmp_stop_up),
       new Subbitmap'(subbmp_stop_down),
       Cmd_Main_Stop'Access),
      (Button_Widget,
       (16+23*4-1, 88, 23, 18),
       CURSOR_NORMAL,
       new Handler'(button_handlers),
       new Subbitmap'(subbmp_next_up),
       new Subbitmap'(subbmp_next_down),
       Cmd_Main_Next'Access),
      (Button_Widget,
       (136, 89, 22, 16),
       CURSOR_NORMAL,
       new Handler'(button_handlers),
       new Subbitmap'(subbmp_eject_up),
       new Subbitmap'(subbmp_eject_down),
       Cmd_Main_Eject'Access),
      (Background_Widget,
       (26, 28, 26+9, 28+9),
       CURSOR_NORMAL,
       new Handler'(background_handlers),
       null,
       False),
      (T => Slider_Widget,
       r => (107, 57, 68, 14),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(volume_backgrounds),
       slider_up => new Subbitmap'(subbmp_volume_bar_up),
       slider_down => new Subbitmap'(subbmp_volume_bar_down),
       slider_horizontal => True,
       slider_min => 0,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (177, 57, 38, 14),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(balance_backgrounds),
       slider_up => new Subbitmap'(subbmp_balance_bar_up),
       slider_down => new Subbitmap'(subbmp_balance_bar_down),
       slider_horizontal => True,
       slider_min => 0,
       slider_max => 24,
       slider_value => 0,
       slider_action => null),
      (Checkbox_Widget,
       (219, 58, 23, 12),
       CURSOR_NORMAL,
       new Handler'(checkbox_handlers),
       new Subbitmap'(subbmp_eq_on_up),
       new Subbitmap'(subbmp_eq_on_down),
       new Subbitmap'(subbmp_eq_off_up),
       new Subbitmap'(subbmp_eq_off_down),
       False,
       Cmd_Main_Eq'Access),
      (Checkbox_Widget,
       (219+23, 58, 23, 12),
       CURSOR_NORMAL,
       new Handler'(checkbox_handlers),
       new Subbitmap'(subbmp_pl_on_up),
       new Subbitmap'(subbmp_pl_on_down),
       new Subbitmap'(subbmp_pl_off_up),
       new Subbitmap'(subbmp_pl_off_down),
       False,
       Cmd_Main_Pl'Access),
      (Checkbox_Widget,
       (165, 89, 46, 15),
       CURSOR_NORMAL,
       new Handler'(checkbox_handlers),
       new Subbitmap'(subbmp_shuffle_on_up),
       new Subbitmap'(subbmp_shuffle_on_down),
       new Subbitmap'(subbmp_shuffle_off_up),
       new Subbitmap'(subbmp_shuffle_off_down),
       False,
       Test_Checkbox'Access),
      (Checkbox_Widget,
       (210, 89, 29, 15),
       CURSOR_NORMAL,
       new Handler'(checkbox_handlers),
       new Subbitmap'(subbmp_repeat_on_up),
       new Subbitmap'(subbmp_repeat_on_down),
       new Subbitmap'(subbmp_repeat_off_up),
       new Subbitmap'(subbmp_repeat_off_down),
       False,
       Test_Checkbox'Access),
      (T => Clutterbar_Widget,
       r => (10, 22, 8, 43),
       c => CURSOR_NORMAL,
       control => new Handler'(clutterbar_handlers),
       clutterbar_a_value => False,
       clutterbar_d_value => False,
       clutterbar_mouse_down => 0,
       clutterbar_set_o => Clutterbar_Set_O'Access,
       clutterbar_set_a => Clutterbar_Set_A'Access,
       clutterbar_set_i => Clutterbar_Set_I'Access,
       clutterbar_set_d => Clutterbar_Set_D'Access,
       clutterbar_set_v => Clutterbar_Set_V'Access),
      (Song_Title_Widget,
       (112, 27, 152, 6),
       CURSOR_SONGNAME,
       new Handler'(song_title_handlers),
       new String'(" *** "),
       0),
      (Scroll_Widget,
       (16, 72, 248, 10),
       CURSOR_NORMAL,
       new Handler'(scroll_handlers),
       new Subbitmap'(subbmp_posbar_background),
       new Subbitmap'(subbmp_posbar_bar_up),
       new Subbitmap'(subbmp_posbar_bar_down),
       29,
       0)
     );

   procedure cmd_eq_close is
   begin
      null;
   end cmd_eq_close;

   procedure cmd_eq_maximize is
   begin
      null;
   end cmd_eq_maximize;

   procedure test_checkbox is
   begin
      null;
   end test_checkbox;

   procedure cmd_eq_presets is
   begin
      null;
   end cmd_eq_presets;

   eq_slider_backgrounds : Subbitmap_Array :=
     (new Subbitmap'(subbmp_eq_slider_bg1),
      new Subbitmap'(subbmp_eq_slider_bg2),
      new Subbitmap'(subbmp_eq_slider_bg3),
      new Subbitmap'(subbmp_eq_slider_bg4),
      new Subbitmap'(subbmp_eq_slider_bg5),
      new Subbitmap'(subbmp_eq_slider_bg6),
      new Subbitmap'(subbmp_eq_slider_bg7),
      new Subbitmap'(subbmp_eq_slider_bg8),
      new Subbitmap'(subbmp_eq_slider_bg9),
      new Subbitmap'(subbmp_eq_slider_bg10),
      new Subbitmap'(subbmp_eq_slider_bg11),
      new Subbitmap'(subbmp_eq_slider_bg12),
      new Subbitmap'(subbmp_eq_slider_bg13),
      new Subbitmap'(subbmp_eq_slider_bg14),
      new Subbitmap'(subbmp_eq_slider_bg15),
      new Subbitmap'(subbmp_eq_slider_bg16),
      new Subbitmap'(subbmp_eq_slider_bg17),
      new Subbitmap'(subbmp_eq_slider_bg18),
      new Subbitmap'(subbmp_eq_slider_bg19),
      new Subbitmap'(subbmp_eq_slider_bg20),
      new Subbitmap'(subbmp_eq_slider_bg21),
      new Subbitmap'(subbmp_eq_slider_bg22),
      new Subbitmap'(subbmp_eq_slider_bg23),
      new Subbitmap'(subbmp_eq_slider_bg24),
      new Subbitmap'(subbmp_eq_slider_bg25),
      new Subbitmap'(subbmp_eq_slider_bg26),
      new Subbitmap'(subbmp_eq_slider_bg27),
      new Subbitmap'(subbmp_eq_slider_bg28)
     );

   equalizer_template : aliased Template :=
     -- background
     ((T => Background_Widget,
       r => (0, 0, 275, 116),
       c => CURSOR_NORMAL,
       control => new Handler'(background_handlers),
       subbmp => new Subbitmap' (subbmp_eq_background),
       move_window => False),
      -- title bar
      (T => Background_Widget,
       r => (0, 0, 275, 14),
       c => CURSOR_NORMAL,
       control => new Handler'(background_handlers),
       subbmp => new Subbitmap'(subbmp_eq_title_bar_off),
       move_window => False),
      (T => Button_Widget,
       r => (254, 3, 9, 9),
       c => CURSOR_NORMAL,
       control => new Handler'(button_handlers),
       button_up => new Subbitmap'(subbmp_maximize_normal_up),
       button_down => new Subbitmap'(subbmp_maximize_normal_down),
       button_action => cmd_eq_maximize'Access),
      (T => Button_Widget,
       r => (264, 3, 9, 9),
       c => CURSOR_CLOSE,
       control => new Handler'(button_handlers),
       button_up => new Subbitmap'(subbmp_close_up),
       button_down => new Subbitmap'(subbmp_close_down),
       button_action => cmd_eq_close'Access),
      -- on auto buttons
      (T => Checkbox_Widget,
       r => (14, 18, 26, 12),
       c => CURSOR_NORMAL,
       control => new Handler'(checkbox_handlers),
       checkbox_on_up => new Subbitmap'(subbmp_eq_on_on_up),
       checkbox_on_down => new Subbitmap'(subbmp_eq_on_on_down),
       checkbox_off_up => new Subbitmap'(subbmp_eq_on_off_up),
       checkbox_off_down => new Subbitmap'(subbmp_eq_on_off_down),
       checkbox_action => test_checkbox'Access,
       checkbox_checked => False),
      (T => Checkbox_Widget,
       r => (40, 18, 32, 12),
       c => CURSOR_NORMAL,
       control => new Handler'(checkbox_handlers),
       checkbox_on_up => new Subbitmap'(subbmp_eq_auto_on_up),
       checkbox_on_down => new Subbitmap'(subbmp_eq_auto_on_down),
       checkbox_off_up => new Subbitmap'(subbmp_eq_auto_off_up),
       checkbox_off_down => new Subbitmap'(subbmp_eq_auto_off_down),
       checkbox_action => test_checkbox'Access,
       checkbox_checked => False),
      -- presets button
      (T => Button_Widget,
       r => (217, 18, 44, 12),
       c => CURSOR_NORMAL,
       control => new Handler'(button_handlers),
       button_up => new Subbitmap'(subbmp_eq_presets_up),
       button_down => new Subbitmap'(subbmp_eq_presets_down),
       button_action => cmd_eq_presets'Access),
      -- preamp
      (T => Slider_Widget,
       r => (21, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*0, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*1, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*2, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*3, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*4, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*5, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*6, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*7, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*8, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*9, 38, 14, 64),
       c => CURSOR_NORMAL,
       control => new Handler'(slider_handlers),
       slider_background => new Subbitmap_Array'(eq_slider_backgrounds),
       slider_up => new Subbitmap'(subbmp_eq_slider_up),
       slider_down => new Subbitmap'(subbmp_eq_slider_down),
       slider_horizontal => false,
       slider_min => 2,
       slider_max => 51,
       slider_value => 0,
       slider_action => null)
     );

   -- TODO: missing template functions.

   function Collision_Detection
     (X, Y : Integer;
      Temp : access Template;
      Num_Controls : Natural)
      return access Widget
   is
      R : Rect;
      wid : access Widget := null;
      I : Natural := Num_Controls;
   begin
      while I > 0 loop
         R := Temp(I).r;
         if X >= r(1) and Y >= r(2)
           and X <= (r(1) + r(3)) and Y < (r(2) + r(4)) then
            Put_Line ("Collision with widget " & I'Image);
            wid := Temp(I)'Access;
            exit;
         end if;
         I := I - 1;
      end loop;
      return wid;
   end Collision_Detection;

   procedure Template_Mouse_Down
     (win : Window_Type;
      Temp : access Template;
      X, Y : Integer)
   is
      wid : access Widget;
      Num_Controls : Natural := Temp'Length;
   begin
      wid := Collision_Detection(X, Y, Temp, Num_Controls);
      if wid /= null then
         wid.control.mouse_down (wid, win, X, Y);
      end if;
   end Template_Mouse_Down;

   procedure Template_Mouse_Up
     (win : Window_Type;
      Temp : access Template;
      X, Y : Integer)
   is
      wid : access Widget;
      Num_Controls : Natural := Temp'Length;
   begin
      if Capture /= null then
         wid := Capture;
      else
         wid := Collision_Detection(X, Y, Temp, Num_Controls);
      end if;

      if wid /= null then
         wid.control.mouse_up (wid, win, X, Y);
      end if;
   end Template_Mouse_Up;

   procedure Template_Mouse_Move
     (win : Window_Type;
      Temp : access Template;
      X, Y : Integer)
   is
      wid : access Widget;
      Num_Controls : Natural := Temp'Length;
   begin
      if capture /= null then
         wid := capture;
      else
         wid := Collision_Detection(X, Y, Temp, Num_Controls);
      end if;

      if wid /= null then
         gui.gui.set_cursor(cursors(Cursor'Pos(wid.c)));
         wid.control.mouse_move(wid, win, X, Y);
      end if;
   end Template_Mouse_Move;

   procedure Template_Draw(win : Window_Type; temp : in out Template) is
   begin
      gui.gui.begin_drawing(win);
      for I in temp'Range loop
         temp(I).control.draw(temp(I)'Access, win);
      end loop;
      gui.gui.end_drawing.all;
   end Template_Draw;

   -- TODO: missing skin callbacks.

   procedure Main_Mouse_Down(x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if double_size then
         X2 := X / 2;
         Y2 := Y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Down(w1, main_template'Access, X2, Y2);
   end Main_Mouse_Down;

   procedure Main_Mouse_Up(x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if double_size then
         X2 := X / 2;
         Y2 := Y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Up(w1, main_template'Access, X2, Y2);
   end Main_Mouse_Up;

   procedure Main_Mouse_Move(x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if double_size then
         X2 := x / 2;
         Y2 := y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Move(w1, main_template'Access, X2, Y2);
   end Main_Mouse_Move;

   procedure Main_Draw is
   begin
      Put_Line("main: draw");
      template_draw(w1, main_template);
   end Main_Draw;

   procedure Equalizer_Mouse_Down(x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if double_size then
         X2 := X / 2;
         Y2 := Y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Down(w2, equalizer_template'Access, X2, Y2);
   end Equalizer_Mouse_Down;

   procedure Equalizer_Mouse_Up(x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if double_size then
         X2 := X / 2;
         Y2 := Y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Up(w2, equalizer_template'Access, X2, Y2);
   end Equalizer_Mouse_Up;

   procedure Equalizer_Mouse_Move(x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if double_size then
         X2 := x / 2;
         Y2 := y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Move(w2, equalizer_template'Access, X2, Y2);
   end Equalizer_Mouse_Move;

   procedure Equalizer_Draw is
   begin
      Put_Line("equalizer: draw");
      template_draw(w2, equalizer_template);
   end Equalizer_Draw;

   main_callbacks : Skin_Callbacks :=
     (mouse_down => Main_Mouse_Down'Access,
      mouse_up => Main_Mouse_Up'Access,
      mouse_move => Main_Mouse_Move'Access,
      draw => Main_Draw'Access,
      focus => null,
      resize => null);

   equalizer_callbacks : aliased Skin_Callbacks :=
     (mouse_down => Equalizer_Mouse_Down'Access,
      mouse_up => Equalizer_Mouse_Up'Access,
      mouse_move => Equalizer_Mouse_Move'Access,
      draw => Equalizer_Draw'Access,
      focus => null,
      resize => null);

   procedure New_Classic
     (Skin : in out Classic_Skin_Type;
      GUI : Gui_Dispatch)
   is
      bmp : Overkill.Gui.Pixmap;
      cur : Overkill.Gui.Cursor;
   begin
      Put_Line ("Loading classic skin.");
      Skin.GUI := GUI;
      w1 := Skin.GUI.create_window (0, 0, 275, 116, "Main", new Skin_Callbacks'(main_callbacks));
      if w1 = null then
         raise Program_Error with "Failed to create Main Window.";
      end if;
      main_window := w1;
      w2 := Skin.GUI.create_window (0, 116, 275, 116, "Equalizer", equalizer_callbacks'Access);
      if w2 = null then
         raise Program_Error with "Failed to create Equalizer Window.";
      end if;
      w3 := Skin.GUI.create_window (0, 116*2, 275, 116, "Playlist", null);
      if w3 = null then
         raise Program_Error with "Failed to create Playlist Window.";
      end if;
      for I in bmp_files'Range loop
         bmp := Skin.GUI.load_image ("skins/classic/" & bmp_files(I).all);
         if bmp = null then
            Put_Line (bmp_files(I).all & ": could not load bmp file");
         end if;
         Put_Line ("I=" & I'Image);
         bmps(I) := bmp;
      end loop;
      for I in cursor_files'Range loop
         cur := Skin.GUI.load_cursor ("skins/classic/" & cursor_files(I).all);
         if cur = null then
            Put_Line (cursor_files(I).all & ": could not load cursor file");
         end if;
         cursors (I) := cur;
      end loop;
      -- ShowWindow might be ignored the first time it's called on win32
      Skin.GUI.show_window (w1);
      Skin.GUI.show_window (w1);
      Skin.GUI.show_window (w2);
      --Skin.GUI.show_window (w3);
   end New_Classic;

   procedure Finalize
     (Skin : in out Classic_Skin_Type)
   is
   begin
      Put_Line ("Finalizing classic skin.");
      for I in bmp_files'Range loop
         Skin.Gui.unload_image(bmps(I));
      end loop;
      for I in cursor_files'Range loop
         Skin.Gui.unload_cursor(cursors(I));
      end loop;
      Skin.Gui.destroy_window(w3);
      Skin.Gui.destroy_window(w2);
      Skin.Gui.destroy_window(w1);
   exception
      when Program_Error =>
         Put_Line ("Error finalizing classic skin.");
         return;
   end Finalize;

   procedure Run
     (Skin : in out Classic_Skin_Type)
   is
   begin
      Put_Line ("Entering event loop.");
      Skin.GUI.event_handler.all;
      Put_Line ("Leaving event loop.");
   end Run;
end Overkill.Classic;
