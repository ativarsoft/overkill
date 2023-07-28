with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Overkill.Debug;
use Overkill.Debug;
with Overkill.Playback;
with Overkill.Platform;
use Overkill.Platform;
with Overkill.Discovery;
use Overkill.Discovery;
with System;
use System;

package body Overkill.Classic with
SPARK_Mode => On
is
   type Mouse_Down_Handler_Func is access procedure (Skin : in out Skin_Type; ID : Window_ID; wid : Natural; X, Y : Integer);
   type Mouse_Up_Handler_Func is access procedure (Skin : in out Skin_Type; ID : Window_ID; wid : Natural; X, Y : Integer);
   type Mouse_Move_Handler_Func is access procedure (Skin : in out Skin_Type; ID : Window_ID; wid : Natural; X, Y : Integer);
   type Draw_Handler_Func is access procedure (Skin : in out Skin_Type; ID : Window_ID; wid : Natural);

   type Handler is record
      mouse_down : Mouse_Down_Handler_Func;
      mouse_up : Mouse_Up_Handler_Func;
      mouse_move : Mouse_Move_Handler_Func;
      draw : Draw_Handler_Func;
   end record;

   type Handler_Access is access constant Handler;

   type Resizeable_Background_Data is record
      top_left : Subbitmap_Access;
      title : Subbitmap_Access;
      top : Subbitmap_Access;
      top_right : Subbitmap_Access;
      left : Subbitmap_Access;
      right : Subbitmap_Access;
      bottom : Subbitmap_Access;
      bottom_left : Subbitmap_Access;
      bottom_right : Subbitmap_Access;
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
      buttons_up : Subbitmap_Access;
      buttons_down : Subbitmap_Access;
      bar : Subbitmap_Access;
   end record;

   type Grip_Data is record
      min_width : Natural;
      min_heigth : Natural;
   end record;

   type Scroll_Data is record
      background : Subbitmap_Access;
      bar_up : Subbitmap_Access;
      bar_down : Subbitmap_Access;
      bar_length : Natural;
      value : Integer;
   end record;

   BALANCE_BMP : aliased constant String := "BALANCE.BMP";
   CBUTTONS_BMP : aliased constant String := "CBUTTONS.BMP";
   EQ_EX_BMP : aliased constant String := "EQ_EX.BMP";
   EQMAIN_BMP : aliased constant String := "EQMAIN.BMP";
   EQMAIN_ISO_BMP : aliased constant String := "EQMAIN_ISO.BMP";
   GEN_BMP : aliased constant String := "GEN.BMP";
   GENEX_BMP : aliased constant String := "GENEX.BMP";
   MAIN_BMP : aliased constant String := "MAIN.BMP";
   MB_BMP : aliased constant String := "MB.BMP";
   MONOSTER_BMP : aliased constant String := "MONOSTER.BMP";
   NUMBERS_BMP : aliased constant String := "NUMBERS.BMP";
   NUMS_EX_BMP : aliased constant String := "NUMS_EX.BMP";
   PLAYPAUS_BMP : aliased constant String := "PLAYPAUS.BMP";
   PLEDIT_BMP : aliased constant String := "PLEDIT.BMP";
   POSBAR_BMP : aliased constant String := "POSBAR.BMP";
   SHUFREP_BMP : aliased constant String := "SHUFREP.BMP";
   TEXT_BMP : aliased constant String := "TEXT.BMP";
   TITLEBAR_BMP : aliased constant String := "TITLEBAR.BMP";
   VIDEO_BMP : aliased constant String := "VIDEO.BMP";
   VOLUME_BMP : aliased constant String := "VOLUME.BMP";

   CLOSE_CUR : aliased constant String := "CLOSE.CUR";
   EQCLOSE_CUR : aliased constant String := "EQCLOSE.CUR";
   EQNORMAL_CUR : aliased constant String := "EQNORMAL.CUR";
   EQSLID_CUR : aliased constant String := "EQSLID.CUR";
   EQTITLE_CUR : aliased constant String := "EQTITLE.CUR";
   MAINMENU_CUR : aliased constant String := "MAINMENU.CUR";
   MIN_CUR : aliased constant String := "MIN.CUR";
   NORMAL_CUR : aliased constant String := "NORMAL.CUR";
   PCLOSE_CUR : aliased constant String := "PCLOSE.CUR";
   PNORMAL_CUR : aliased constant String := "PNORMAL.CUR";
   POSBAR_CUR : aliased constant String := "POSBAR.CUR";
   PSIZE_CUR : aliased constant String := "PSIZE.CUR";
   PTBAR_CUR : aliased constant String := "PTBAR.CUR";
   PVSCROLL_CUR : aliased constant String := "PVSCROLL.CUR";
   PWINBUT_CUR : aliased constant String := "PWINBUT.CUR";
   PWSNORM_CUR : aliased constant String := "PWSNORM.CUR";
   PWSSIZE_CUR : aliased constant String := "PWSSIZE.CUR";
   SONGNAME_CUR : aliased constant String := "SONGNAME.CUR";
   TITLEBAR_CUR : aliased constant String := "TITLEBAR.CUR";
   VOLBAL_CUR : aliased constant String := "VOLBAL.CUR";
   VOLBAR_CUR : aliased constant String := "VOLBAR.CUR";
   WINBUT_CUR : aliased constant String := "WINBUT.CUR";
   WSCLOSE_CUR : aliased constant String := "WSCLOSE.CUR";
   WSMIN_CUR : aliased constant String := "WSMIN.CUR";
   WSNORMAL_CUR : aliased constant String := "WSNORMAL.CUR";
   WSPOSBAR_CUR : aliased constant String := "WSPOSBAR.CUR";
   WSWINBUT_CUR : aliased constant String := "WSWINBUT.CUR";

   subbmp_main : aliased constant Subbitmap := (BMP_MAIN, 0, 0);
   subbmp_title_bar_on : aliased constant Subbitmap := (BMP_TITLEBAR, 27, 0);
   subbmp_title_bar_off : aliased constant Subbitmap := (BMP_TITLEBAR, 27, 15);
   subbmp_title_bar_shade_on : aliased constant Subbitmap := (BMP_TITLEBAR, 27, 29);
   subbmp_title_bar_shade_off : aliased constant Subbitmap := (BMP_TITLEBAR, 27, 42);
   subbmp_title_bar_easter_on : aliased constant Subbitmap := (BMP_TITLEBAR, 27, 57);
   subbmp_title_bar_easter_off : aliased constant Subbitmap := (BMP_TITLEBAR, 27, 72);
   subbmp_a : aliased constant Subbitmap := (BMP_TITLEBAR, 304, 0);
   subbmp_mono_on : aliased constant Subbitmap := (BMP_MONOSTER, 29, 0);
   subbmp_mono_off : aliased constant Subbitmap := (BMP_MONOSTER, 29, 12);
   subbmp_stereo_on : aliased constant Subbitmap := (BMP_MONOSTER, 0, 0);
   subbmp_stereo_off : aliased constant Subbitmap := (BMP_MONOSTER, 0, 12);
   subbmp_previous_up : aliased constant Subbitmap := (BMP_CBUTTONS, 0, 0);
   subbmp_previous_down : aliased constant Subbitmap := (BMP_CBUTTONS, 0, 18);
   subbmp_play_up : aliased constant Subbitmap := (BMP_CBUTTONS, 22+23*0, 0);
   subbmp_play_down : aliased constant Subbitmap := (BMP_CBUTTONS, 22+23*0, 18);
   subbmp_pause_up : aliased constant Subbitmap := (BMP_CBUTTONS, 22+23*1, 0);
   subbmp_pause_down : aliased constant Subbitmap := (BMP_CBUTTONS, 22+23*1, 18);
   subbmp_stop_up : aliased constant Subbitmap := (BMP_CBUTTONS, 22+23*2, 0);
   subbmp_stop_down : aliased constant Subbitmap := (BMP_CBUTTONS, 22+23*2, 18);
   subbmp_next_up : aliased constant Subbitmap := (BMP_CBUTTONS, 22+23*3, 0);
   subbmp_next_down : aliased constant Subbitmap := (BMP_CBUTTONS, 22+23*3, 18);
   subbmp_eject_up : aliased constant Subbitmap := (BMP_CBUTTONS, 22+23*4, 0);
   subbmp_eject_down : aliased constant Subbitmap := (BMP_CBUTTONS, 22+23*4, 16);
   subbmp_status : aliased constant Subbitmap := (BMP_PLAYPAUS, 9*2, 0);
   subbmp_volume_bg1 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*0);
   subbmp_volume_bg2 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*1);
   subbmp_volume_bg3 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*2);
   subbmp_volume_bg4 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*3);
   subbmp_volume_bg5 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*4);
   subbmp_volume_bg6 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*5);
   subbmp_volume_bg7 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*6);
   subbmp_volume_bg8 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*7);
   subbmp_volume_bg9 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*8);
   subbmp_volume_bg10 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*9);
   subbmp_volume_bg11 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*10);
   subbmp_volume_bg12 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*11);
   subbmp_volume_bg13 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*12);
   subbmp_volume_bg14 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*13);
   subbmp_volume_bg15 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*14);
   subbmp_volume_bg16 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*15);
   subbmp_volume_bg17 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*16);
   subbmp_volume_bg18 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*17);
   subbmp_volume_bg19 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*18);
   subbmp_volume_bg20 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*19);
   subbmp_volume_bg21 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*20);
   subbmp_volume_bg22 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*21);
   subbmp_volume_bg23 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*22);
   subbmp_volume_bg24 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*23);
   subbmp_volume_bg25 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*24);
   subbmp_volume_bg26 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*25);
   subbmp_volume_bg27 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*26);
   subbmp_volume_bg28 : aliased constant Subbitmap := (BMP_VOLUME, 0, 15*27);
   subbmp_volume_bar_up : aliased constant Subbitmap := (BMP_VOLUME, 15, 422);
   subbmp_volume_bar_down : aliased constant Subbitmap := (BMP_VOLUME, 0, 422);
   subbmp_balance_bg1 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*0);
   subbmp_balance_bg2 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*1);
   subbmp_balance_bg3 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*2);
   subbmp_balance_bg4 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*3);
   subbmp_balance_bg5 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*4);
   subbmp_balance_bg6 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*5);
   subbmp_balance_bg7 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*6);
   subbmp_balance_bg8 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*7);
   subbmp_balance_bg9 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*8);
   subbmp_balance_bg10 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*9);
   subbmp_balance_bg11 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*10);
   subbmp_balance_bg12 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*11);
   subbmp_balance_bg13 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*12);
   subbmp_balance_bg14 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*13);
   subbmp_balance_bg15 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*14);
   subbmp_balance_bg16 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*15);
   subbmp_balance_bg17 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*16);
   subbmp_balance_bg18 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*17);
   subbmp_balance_bg19 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*18);
   subbmp_balance_bg20 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*19);
   subbmp_balance_bg21 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*20);
   subbmp_balance_bg22 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*21);
   subbmp_balance_bg23 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*22);
   subbmp_balance_bg24 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*23);
   subbmp_balance_bg25 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*24);
   subbmp_balance_bg26 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*25);
   subbmp_balance_bg27 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*26);
   subbmp_balance_bg28 : aliased constant Subbitmap := (BMP_BALANCE, 9, 15*27);
   subbmp_balance_bar_up : aliased constant Subbitmap := (BMP_BALANCE, 15, 422);
   subbmp_balance_bar_down : aliased constant Subbitmap := (BMP_BALANCE, 0, 422);
   subbmp_eq_off_up : aliased constant Subbitmap := (BMP_SHUFREP, 0, 61);
   subbmp_eq_off_down : aliased constant Subbitmap := (BMP_SHUFREP, 46, 61);
   subbmp_eq_on_up : aliased constant Subbitmap := (BMP_SHUFREP, 0, 73);
   subbmp_eq_on_down : aliased constant Subbitmap := (BMP_SHUFREP, 46, 73);
   subbmp_pl_off_up : aliased constant Subbitmap := (BMP_SHUFREP, 23, 61);
   subbmp_pl_off_down : aliased constant Subbitmap := (BMP_SHUFREP, 70, 61);
   subbmp_pl_on_up : aliased constant Subbitmap := (BMP_SHUFREP, 23, 73);
   subbmp_pl_on_down : aliased constant Subbitmap := (BMP_SHUFREP, 70, 73);
   subbmp_shuffle_off_up : aliased constant Subbitmap := (BMP_SHUFREP, 29, 0);
   subbmp_shuffle_off_down : aliased constant Subbitmap := (BMP_SHUFREP, 29, 15);
   subbmp_shuffle_on_up : aliased constant Subbitmap := (BMP_SHUFREP, 29, 15*2);
   subbmp_shuffle_on_down : aliased constant Subbitmap := (BMP_SHUFREP, 29, 15*3);
   subbmp_repeat_off_up : aliased constant Subbitmap := (BMP_SHUFREP, 0, 0);
   subbmp_repeat_off_down : aliased constant Subbitmap := (BMP_SHUFREP, 0, 15);
   subbmp_repeat_on_up : aliased constant Subbitmap := (BMP_SHUFREP, 0, 15*2);
   subbmp_repeat_on_down : aliased constant Subbitmap := (BMP_SHUFREP, 0, 15*3);
   subbmp_clutterbar_off : aliased constant Subbitmap := (BMP_TITLEBAR, 304, 0);
   subbmp_clutterbar_disabled : aliased constant Subbitmap := (BMP_TITLEBAR, 312, 0);
   subbmp_clutterbar_off_o : aliased constant Subbitmap := (BMP_TITLEBAR, 304, 44);
   subbmp_clutterbar_off_a : aliased constant Subbitmap := (BMP_TITLEBAR, 312, 44);
   subbmp_clutterbar_off_i : aliased constant Subbitmap := (BMP_TITLEBAR, 320, 44);
   subbmp_clutterbar_off_d : aliased constant Subbitmap := (BMP_TITLEBAR, 328, 44);
   subbmp_clutterbar_off_v : aliased constant Subbitmap := (BMP_TITLEBAR, 336, 44);
   subbmp_clutterbar_o : aliased constant Subbitmap := (BMP_TITLEBAR, 304, 45);
   subbmp_clutterbar_a : aliased constant Subbitmap := (BMP_TITLEBAR, 312, 53);
   subbmp_clutterbar_i : aliased constant Subbitmap := (BMP_TITLEBAR, 320, 61);
   subbmp_clutterbar_d : aliased constant Subbitmap := (BMP_TITLEBAR, 328, 69);
   subbmp_clutterbar_v : aliased constant Subbitmap := (BMP_TITLEBAR, 336, 77);
   subbmp_status_play : aliased constant Subbitmap := (BMP_PLAYPAUS, 0, 0);
   subbmp_status_pause : aliased constant Subbitmap := (BMP_PLAYPAUS, 9, 0);
   subbmp_status_stop : aliased constant Subbitmap := (BMP_PLAYPAUS, 18, 0);
   subbmp_status_red_on : aliased constant Subbitmap := (BMP_PLAYPAUS, 0, 0);
   subbmp_status_red_off : aliased constant Subbitmap := (BMP_PLAYPAUS, 0, 0);
   subbmp_status_green_on : aliased constant Subbitmap := (BMP_PLAYPAUS, 0, 0);
   subbmp_status_green_off : aliased constant Subbitmap := (BMP_PLAYPAUS, 0, 0);
   subbmp_options_up : aliased constant Subbitmap := (BMP_TITLEBAR, 0, 0);
   subbmp_options_down : aliased constant Subbitmap := (BMP_TITLEBAR, 0, 9);
   subbmp_minimize_up : aliased constant Subbitmap := (BMP_TITLEBAR, 9, 0);
   subbmp_minimize_down : aliased constant Subbitmap := (BMP_TITLEBAR, 9, 9);
   subbmp_close_up : aliased constant Subbitmap := (BMP_TITLEBAR, 18, 0);
   subbmp_close_down : aliased constant Subbitmap := (BMP_TITLEBAR, 18, 9);
   subbmp_maximize_normal_up : aliased constant Subbitmap := (BMP_TITLEBAR, 0, 18);
   subbmp_maximize_normal_down : aliased constant Subbitmap := (BMP_TITLEBAR, 9, 18);
   subbmp_maximize_ws_up : aliased constant Subbitmap := (BMP_TITLEBAR, 0, 27);
   subbmp_maximize_ws_down : aliased constant Subbitmap := (BMP_TITLEBAR, 9, 27);
   subbmp_posbar_background : aliased constant Subbitmap := (BMP_POSBAR, 0, 0);
   subbmp_posbar_bar_up : aliased constant Subbitmap := (BMP_POSBAR, 248, 0);
   subbmp_posbar_bar_down : aliased constant Subbitmap := (BMP_POSBAR, 277, 0);

   subbmp_eq_background : aliased constant Subbitmap := (BMP_EQMAIN, 0, 0);
   subbmp_eq_title_bar_on : aliased constant Subbitmap := (BMP_EQMAIN, 0, 134);
   subbmp_eq_title_bar_off : aliased constant Subbitmap := (BMP_EQMAIN, 0, 149);
   subbmp_eq_on_on_up : aliased constant Subbitmap := (BMP_EQMAIN, 69, 119);
   subbmp_eq_on_on_down : aliased constant Subbitmap := (BMP_EQMAIN, 187, 119);
   subbmp_eq_on_off_up : aliased constant Subbitmap := (BMP_EQMAIN, 10, 119);
   subbmp_eq_on_off_down : aliased constant Subbitmap := (BMP_EQMAIN, 128, 119);
   subbmp_eq_auto_on_up : aliased constant Subbitmap := (BMP_EQMAIN, 95, 119);
   subbmp_eq_auto_on_down : aliased constant Subbitmap := (BMP_EQMAIN, 213, 119);
   subbmp_eq_auto_off_up : aliased constant Subbitmap := (BMP_EQMAIN, 36, 119);
   subbmp_eq_auto_off_down : aliased constant Subbitmap := (BMP_EQMAIN, 154, 119);
   subbmp_eq_presets_up : aliased constant Subbitmap := (BMP_EQMAIN, 224, 164);
   subbmp_eq_presets_down : aliased constant Subbitmap := (BMP_EQMAIN, 224, 176);
   subbmp_eq_preamp : aliased constant Subbitmap := (BMP_EQMAIN, 13, 164);
   subbmp_eq_slider_bg1 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*0, 164);
   subbmp_eq_slider_bg2 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*1, 164);
   subbmp_eq_slider_bg3 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*2, 164);
   subbmp_eq_slider_bg4 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*3, 164);
   subbmp_eq_slider_bg5 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*4, 164);
   subbmp_eq_slider_bg6 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*5, 164);
   subbmp_eq_slider_bg7 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*6, 164);
   subbmp_eq_slider_bg8 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*7, 164);
   subbmp_eq_slider_bg9 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*8, 164);
   subbmp_eq_slider_bg10 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*9, 164);
   subbmp_eq_slider_bg11 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*10, 164);
   subbmp_eq_slider_bg12 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*11, 164);
   subbmp_eq_slider_bg13 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*12, 164);
   subbmp_eq_slider_bg14 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*13, 164);
   subbmp_eq_slider_bg15 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*0, 229);
   subbmp_eq_slider_bg16 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*1, 229);
   subbmp_eq_slider_bg17 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*2, 229);
   subbmp_eq_slider_bg18 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*3, 229);
   subbmp_eq_slider_bg19 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*4, 229);
   subbmp_eq_slider_bg20 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*5, 229);
   subbmp_eq_slider_bg21 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*6, 229);
   subbmp_eq_slider_bg22 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*7, 229);
   subbmp_eq_slider_bg23 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*8, 229);
   subbmp_eq_slider_bg24 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*9, 229);
   subbmp_eq_slider_bg25 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*10, 229);
   subbmp_eq_slider_bg26 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*11, 229);
   subbmp_eq_slider_bg27 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*12, 229);
   subbmp_eq_slider_bg28 : aliased constant Subbitmap := (BMP_EQMAIN, 13+15*13, 229);
   subbmp_eq_slider_up : aliased constant Subbitmap := (BMP_EQMAIN, 0, 164);
   subbmp_eq_slider_down : aliased constant Subbitmap := (BMP_EQMAIN, 0, 176);

   subbmp_pl_top_left_on : aliased constant Subbitmap := (BMP_PLEDIT, 0, 0);
   subbmp_pl_title_on : aliased constant Subbitmap := (BMP_PLEDIT, 26, 0);
   subbmp_pl_top_on : aliased constant Subbitmap := (BMP_PLEDIT, 127, 0);
   subbmp_pl_top_right_on : aliased constant Subbitmap := (BMP_PLEDIT, 153, 0);
   subbmp_pl_top_left_off : aliased constant Subbitmap := (BMP_PLEDIT, 0, 21);
   subbmp_pl_title_off : aliased constant Subbitmap := (BMP_PLEDIT, 26, 21);
   subbmp_pl_top_off : aliased constant Subbitmap := (BMP_PLEDIT, 127, 21);
   subbmp_pl_top_right_off : aliased constant Subbitmap := (BMP_PLEDIT, 153, 21);
   subbmp_pl_left : aliased constant Subbitmap := (BMP_PLEDIT, 0, 42);
   subbmp_pl_right : aliased constant Subbitmap := (BMP_PLEDIT, 26, 42);
   subbmp_pl_bottom : aliased constant Subbitmap := (BMP_PLEDIT, 179, 0);
   subbmp_pl_bottom_left : aliased constant Subbitmap := (BMP_PLEDIT, 0, 72);
   subbmp_pl_bottom_right : aliased constant Subbitmap := (BMP_PLEDIT, 126, 72);
   subbmp_pl_menu_add_url_up : aliased constant Subbitmap := (BMP_PLEDIT, 0, 111);
   subbmp_pl_menu_add_dir_up : aliased constant Subbitmap := (BMP_PLEDIT, 0, 130);
   subbmp_pl_menu_add_file_up : aliased constant Subbitmap := (BMP_PLEDIT, 0, 149);
   subbmp_pl_menu_add_url_down : aliased constant Subbitmap := (BMP_PLEDIT, 23, 111);
   subbmp_pl_menu_add_dir_down : aliased constant Subbitmap := (BMP_PLEDIT, 23, 130);
   subbmp_pl_menu_add_file_down : aliased constant Subbitmap := (BMP_PLEDIT, 23, 149);
   subbmp_pl_menu_add_bar : aliased constant Subbitmap := (BMP_PLEDIT, 48, 111);
   subbmp_pl_menu_rem_all_up : aliased constant Subbitmap := (BMP_PLEDIT, 54, 111);
   subbmp_pl_menu_rem_crop_up : aliased constant Subbitmap := (BMP_PLEDIT, 54, 130);
   subbmp_pl_menu_rem_sel_up : aliased constant Subbitmap := (BMP_PLEDIT, 54, 149);
   subbmp_pl_menu_rem_misc_up : aliased constant Subbitmap := (BMP_PLEDIT, 54, 168);
   subbmp_pl_menu_rem_all_down : aliased constant Subbitmap := (BMP_PLEDIT, 77, 111);
   subbmp_pl_menu_rem_crop_down : aliased constant Subbitmap := (BMP_PLEDIT, 77, 130);
   subbmp_pl_menu_rem_sel_down : aliased constant Subbitmap := (BMP_PLEDIT, 77, 149);
   subbmp_pl_menu_rem_misc_down : aliased constant Subbitmap := (BMP_PLEDIT, 77, 168);
   subbmp_pl_menu_rem_bar : aliased constant Subbitmap := (BMP_PLEDIT, 100, 111);
   subbmp_pl_menu_sel_inv_up : aliased constant Subbitmap := (BMP_PLEDIT, 104, 111);
   subbmp_pl_menu_sel_zero_up : aliased constant Subbitmap := (BMP_PLEDIT, 104, 130);
   subbmp_pl_menu_sel_all_up : aliased constant Subbitmap := (BMP_PLEDIT, 104, 149);
   subbmp_pl_menu_sel_inv_down : aliased constant Subbitmap := (BMP_PLEDIT, 127, 111);
   subbmp_pl_menu_sel_zero_down : aliased constant Subbitmap := (BMP_PLEDIT, 127, 130);
   subbmp_pl_menu_sel_all_down : aliased constant Subbitmap := (BMP_PLEDIT, 127, 149);
   subbmp_pl_menu_sel_bar : aliased constant Subbitmap := (BMP_PLEDIT, 150, 111);
   subbmp_pl_menu_misc_sort_up : aliased constant Subbitmap := (BMP_PLEDIT, 154, 111);
   subbmp_pl_menu_misc_inf_up : aliased constant Subbitmap := (BMP_PLEDIT, 154, 130);
   subbmp_pl_menu_misc_opts_up : aliased constant Subbitmap := (BMP_PLEDIT, 154, 149);
   subbmp_pl_menu_misc_sort_down : aliased constant Subbitmap := (BMP_PLEDIT, 177, 111);
   subbmp_pl_menu_misc_inf_down : aliased constant Subbitmap := (BMP_PLEDIT, 177, 130);
   subbmp_pl_menu_misc_opts_down : aliased constant Subbitmap := (BMP_PLEDIT, 177, 149);
   subbmp_pl_menu_misc_bar : aliased constant Subbitmap := (BMP_PLEDIT, 200, 111);
   subbmp_pl_menu_list_new_up : aliased constant Subbitmap := (BMP_PLEDIT, 204, 111);
   subbmp_pl_menu_list_save_up : aliased constant Subbitmap := (BMP_PLEDIT, 204, 130);
   subbmp_pl_menu_list_load_up : aliased constant Subbitmap := (BMP_PLEDIT, 204, 149);
   subbmp_pl_menu_list_new_down : aliased constant Subbitmap := (BMP_PLEDIT, 227, 111);
   subbmp_pl_menu_list_save_down : aliased constant Subbitmap := (BMP_PLEDIT, 227, 130);
   subbmp_pl_menu_list_load_down : aliased constant Subbitmap := (BMP_PLEDIT, 227, 149);
   subbmp_pl_menu_list_bar : aliased constant Subbitmap := (BMP_PLEDIT, 250, 111);

   -- TODO: bitmap font.

   --
   -- Widget Actions
   --

   procedure Test_Button (Skin : in out Skin_Type) is
   begin
      Put_Line("Test Button");
   end Test_Button;

   procedure Test_Checkbox(Skin : in out Skin_Type; state : Boolean) is
   begin
      Put_Line("Test Checkbox: " & state'Image);
   end Test_Checkbox;

   procedure Cmd_Main_Minimize (Skin : in out Skin_Type) is
   begin
      Skin.Gui.minimize_window(Skin.Windows (MAIN_WINDOW_ID));
      Skin.Gui.hide_window(Skin.Windows (EQUALIZER_WINDOW_ID));
      Skin.Gui.hide_window(Skin.Windows (PLAYLIST_WINDOW_ID));
   end Cmd_Main_Minimize;

   procedure Cmd_Main_Maximize (Skin : in out Skin_Type) is
   begin
      Skin.main_shade := not Skin.main_shade;
      if Skin.main_shade then
         Skin.Gui.resize_window (Skin.Windows (MAIN_WINDOW_ID), 275, 14);
      else
         Skin.Gui.resize_window (Skin.Windows (MAIN_WINDOW_ID), 275, 116);
      end if;
   end Cmd_Main_Maximize;

   procedure Cmd_Main_Close (Skin : in out Skin_Type) is
   begin
      Skin.Gui.destroy_window (Skin.Windows (MAIN_WINDOW_ID));
   end Cmd_Main_Close;

   procedure Cmd_Main_Previous (Skin : in out Skin_Type) is
   begin
      Playback.Previous;
   end Cmd_Main_Previous;

   procedure Cmd_Main_Play (Skin : in out Skin_Type) is
   begin
      Playback.Play;
   end Cmd_Main_Play;

   procedure Cmd_Main_Pause (Skin : in out Skin_Type) is
   begin
      Playback.Pause;
   end Cmd_Main_Pause;

   procedure Cmd_Main_Stop (Skin : in out Skin_Type) is
   begin
      Playback.Stop;
   end Cmd_Main_Stop;

   procedure Cmd_Main_Next (Skin : in out Skin_Type) is
   begin
      Playback.Next;
   end Cmd_Main_Next;

   procedure Cmd_Main_Eject (Skin : in out Skin_Type) is
   begin
      Skin.Gui.open_file_dialog.all;
   end Cmd_Main_Eject;

   procedure Cmd_Main_Eq(Skin : in out Skin_Type; checked : Boolean) is
   begin
      if checked then
         Skin.Gui.show_window(Skin.Windows (EQUALIZER_WINDOW_ID));
      else
         Skin.Gui.hide_window(Skin.Windows (EQUALIZER_WINDOW_ID));
      end if;
   end Cmd_Main_Eq;

   procedure Cmd_Main_Pl(Skin : in out Skin_Type; checked : Boolean) is
   begin
      if checked then
         Skin.Gui.show_window(Skin.Windows (PLAYLIST_WINDOW_ID));
      else
         Skin.Gui.hide_window(Skin.Windows (PLAYLIST_WINDOW_ID));
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

   --  procedure Draw_Time
   --    (minutes, seconds : Integer;
   --     remaining_time : Boolean)
   --  is
   --     bmp : Pixmap;
   --  begin
   --     bmp := bmps(Bitmap'Pos(BMP_NUMS_EX));
   --     Skin.Gui.draw_image(bmp, 48, 26, 9, 13, 9 * (minutes / 10), 0);
   --     Skin.Gui.draw_image(bmp, 60, 26, 9, 13, 9 * (minutes mod 10), 0);
   --     Skin.Gui.draw_image(bmp, 78, 26, 9, 13, 9 * (seconds / 10), 0);
   --     Skin.Gui.draw_image(bmp, 90, 26, 9, 13, 9 * (seconds mod 10), 0);
   --  end Draw_Time;

   procedure Capture_Mouse(Skin : in out Skin_Type; ID : Window_ID; wid : Natural) is
   begin
      Skin.Gui.capture_mouse(Skin.Windows (ID));
      Skin.capture := wid;
   end Capture_Mouse;

   procedure Redraw_Window (Skin : Skin_Type; ID : Window_ID)
   is
   begin
      Skin.Gui.Redraw_Window (Skin.Windows (ID));
   end Redraw_Window;

   procedure Release_Mouse (Skin : in out Skin_Type) is
   begin
      Skin.Gui.release_mouse.all;
      Skin.capture := 0;
   end Release_Mouse;

   procedure Draw_Pixmap
     (Skin : in out Skin_Type;
      subbmp : Subbitmap;
      dst_x, dst_y : Integer;
      w, h : Integer)
   is
      id : Bitmap_Type;
   begin
      id := subbmp.bmp;
      Skin.Gui.draw_image(Skin.bmps(id), dst_x, dst_y, w, h, subbmp.X, subbmp.Y);
   end Draw_Pixmap;

   procedure Draw_Pixmap_Double
     (Skin : in out Skin_Type;
      subbmp : Subbitmap;
      dst_x, dst_y : Integer;
      w, h : Integer)
   is
      id : Bitmap_Type;
   begin
      id := subbmp.bmp;
      if Skin.double_size = False then
         Skin.Gui.draw_image(Skin.bmps(id), dst_x, dst_y, w, h, subbmp.X, subbmp.Y);
      else
         Skin.Gui.draw_image_double(Skin.bmps(id), dst_x, dst_y, w, h, subbmp.X, subbmp.Y);
      end if;
   end Draw_Pixmap_Double;

   procedure Draw_Pixmap_Loop_Horizontal
     (Skin : in out Skin_Type;
      subbmp : Subbitmap;
      dst_x, dst_y : Integer;
      src_w, src_h : Integer;
      dst_w : Integer)
   is
      I : Integer := 0;
   begin
      while I < dst_w loop
         Draw_Pixmap(Skin, subbmp, dst_x + I, dst_y, src_w, src_h);
         I := I + src_w;
      end loop;
   end Draw_Pixmap_Loop_Horizontal;

   procedure Draw_Pixmap_Loop_Vertical
     (Skin : in out Skin_Type;
      subbmp : Subbitmap;
      dst_x, dst_y : Integer;
      src_w, src_h : Integer;
      dst_h : Integer)
   is
      I : Integer := 0;
   begin
      while I < dst_h loop
         Draw_Pixmap(Skin, subbmp, dst_x, dst_y + I, src_w, src_h);
         I := I + src_h;
      end loop;
   end Draw_Pixmap_Loop_Vertical;

   procedure Draw_Text
     (Skin : in out Skin_Type;
      x, y, w, h : Integer;
      Text : String)
   is
   begin
      Skin.Gui.Draw_Text (x, y, w, h, Text);
   end Draw_Text;

   procedure Background_Mouse_Down
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      if Y < 16 or Skin.easymove then
         Capture_Mouse(Skin, ID, wid);
         Skin.last_x := X;
         Skin.last_y := Y;
      end if;
   end Background_Mouse_Down;

   procedure Background_Mouse_Up
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      if Y < 16 or Skin.easymove then
         Release_Mouse (Skin);
      end if;
   end Background_Mouse_Up;

   procedure Background_Mouse_Move
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
      A, B : Integer;
      win : Window_Type;
   begin
      if Skin.capture = wid then
         Win := Skin.Windows (ID);

         if Skin.Gui.check_glue(win, Skin.Windows (MAIN_WINDOW_ID), X - Skin.last_x, Y - Skin.last_y) = False then
            Skin.Gui.move_window(win, x - Skin.last_x, y - Skin.last_y);
            A := X;
            B := Y;
         end if;

         if ID = MAIN_WINDOW_ID then
            Skin.Gui.move_window(Skin.Windows (EQUALIZER_WINDOW_ID), X - Skin.last_x, Y - Skin.last_y);
            Skin.Gui.move_window(Skin.Windows (PLAYLIST_WINDOW_ID), X - Skin.last_x, Y - Skin.last_y);
         end if;
      end if;
   end Background_Mouse_Move;

   procedure Background_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural)
   is
      r : constant Rect := Skin.Templates (ID) (wid).r;
      subbmp : Subbitmap;
   begin
      Put_Line ("Drawing bitmap " & wid'Image & " on window " & ID'Image);
      if Skin.Templates (ID) (wid).subbmp /= null then
         subbmp := Skin.Templates (ID) (wid).subbmp.all;
         Draw_Pixmap_Double(Skin, subbmp, r(1), r(2), r(3), r(4));
      end if;
   end Background_Draw;

   procedure Button_Mouse_Down
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      Capture_Mouse (Skin, ID, wid);
      Redraw_Window (Skin, ID);
   end Button_Mouse_Down;

   procedure Button_Mouse_Up
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
      R : Rect;
      Action : Button_Action_Func;
   begin
      if Skin.Capture = wid then
         Release_Mouse (Skin);
         Redraw_Window (Skin, ID);

         R := Skin.Templates (ID) (wid).r;
         if X > r (1) and X < r (1) + r (3) and
           Y > r (2) and Y < r (2) + r (4) then
            Action := Skin.Templates (ID) (wid).button_action;
            if Action /= null then
               Action.all (Skin);
            end if;
         end if;
      end if;
   end Button_Mouse_Up;

   procedure Button_Mouse_Move
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      null;
   end Button_Mouse_Move;

   procedure Button_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural)
   is
      r : Rect := Skin.Templates (ID) (wid).r;
      subbmp : Subbitmap_Access;
   begin
      if Skin.capture = wid then
         subbmp := Skin.Templates (ID) (wid).button_down;
      else
         subbmp := Skin.Templates (ID) (wid).button_up;
      end if;
      if subbmp /= null then
         Draw_Pixmap_Double(Skin, subbmp.all, r(1), r(2), r(3), r(4));
      end if;
   end Button_Draw;

   procedure Checkbox_Mouse_Down
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      Capture_Mouse (Skin, ID, wid);
      Redraw_Window (Skin, ID);
   end Checkbox_Mouse_Down;

   procedure Checkbox_Mouse_Up
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
      R : Rect;
      Action : Checkbox_Action_Func;
   begin
      if Skin.Capture = wid then
         Release_Mouse (Skin);
         Redraw_Window (Skin, ID);

         R := Skin.Templates (ID) (wid).r;
         if X > r (1) and X < r (1) + r (3) and
           Y > r (2) and Y < r (2) + r (4) then
            Action := Skin.Templates (ID) (wid).checkbox_action;
            if Action /= null then
               Action.all (Skin, Skin.Templates (ID) (wid).checkbox_checked);
            end if;
         end if;
      end if;
   end Checkbox_Mouse_Up;

   procedure Checkbox_Mouse_Move
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      null;
   end Checkbox_Mouse_Move;

   procedure Checkbox_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural)
   is
      r : Rect;
      subbmp : Subbitmap_Access;
   begin
      r := Skin.Templates (ID) (wid).r;
      if Skin.Templates (ID) (wid).checkbox_checked then
         if Skin.capture = wid then
            subbmp := Skin.Templates (ID) (wid).checkbox_on_down;
         else
            subbmp := Skin.Templates (ID) (wid).checkbox_on_up;
         end if;
      else
         if Skin.capture = wid then
            subbmp := Skin.Templates (ID) (wid).checkbox_off_down;
         else
            subbmp := Skin.Templates (ID) (wid).checkbox_off_up;
         end if;
      end if;

      Draw_Pixmap_Double(Skin, subbmp.all, r(1), r(2), r(3), r(4));
   end Checkbox_Draw;

   procedure Slider_Mouse_Down
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      Capture_Mouse (Skin, ID, wid);
      Skin.Last_X := X;
      Skin.Last_Y := Y;
      Redraw_Window (Skin, ID);
   end Slider_Mouse_Down;

   procedure Slider_Mouse_Up
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      Release_Mouse (Skin);
      Redraw_Window (Skin, ID);
   end Slider_Mouse_Up;

   procedure Slider_Mouse_Move
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
      Difference : Integer;
      Value : Integer;
   begin
      if Skin.Capture = wid then
         if Skin.Templates (ID) (wid).slider_horizontal = True then
            Difference := X - Skin.Last_X;
            Value := Skin.Templates (ID) (wid).slider_value + Difference;
         else
            Difference := Y - Skin.Last_Y;
            Value := Skin.Templates (ID) (wid).slider_value - Difference;
         end if;

         if Value < Skin.Templates (ID) (wid).slider_min then
            Skin.Templates (ID) (wid).slider_value := Skin.Templates (ID) (wid).slider_min;
         elsif Value > Skin.Templates (ID) (wid).slider_max then
            Skin.Templates (ID) (wid).slider_value := Skin.Templates (ID) (wid).slider_max;
         else
            Skin.Last_X := X;
            Skin.Last_Y := Y;
            Skin.Templates (ID) (wid).slider_value := Value;
         end if;

         Skin.Gui.redraw_window (Skin.Windows (ID));
      end if;
   end Slider_Mouse_Move;

   procedure Slider_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural) is
      r : Rect;
      bg, bar : Subbitmap_Access;
      n, slider_range : Natural;
   begin
      Put_Line ("Slider_Draw: wid.slider_value: " & Skin.Templates (ID) (wid).slider_value'Image);
      r := Skin.Templates (ID) (wid).r;

      -- select the background
      -- there are 28 possible bg images
      if Skin.Templates (ID) (wid).slider_horizontal then
         slider_range := Skin.Templates (ID) (wid).slider_max - Skin.Templates (ID) (wid).slider_min;
         n := Skin.Templates (ID) (wid).slider_value * 28 / slider_range;
         if n > 27 then
            n := 27;
         end if;
      else
         slider_range := Skin.Templates (ID) (wid).slider_max - Skin.Templates (ID) (wid).slider_min;
         n := Skin.Templates (ID) (wid).slider_value * 28 / slider_range;
         if n > 27 then
            n := 27;
         end if;
      end if;
      bg := Skin.Templates (ID) (wid).slider_background(n);

      if Skin.capture = wid then
         bar := Skin.Templates (ID) (wid).slider_down;
      else
         bar := Skin.Templates (ID) (wid).slider_up;
      end if;

      -- Draw the background
      Draw_Pixmap_Double(Skin, bg.all, r(1), r(2), r(3), r(4));
      -- Draw the bar
      if Skin.Templates (ID) (wid).slider_horizontal = True then
         Draw_Pixmap_Double(Skin, bar.all, r(1) + Skin.Templates (ID) (wid).slider_value + Skin.Templates (ID) (wid).slider_min, r(2) + 1, 14, 11);
         null;
      else
         Draw_Pixmap_Double(Skin, bar.all, r(1) + 1, r(2) + r(4) - 13 - Skin.Templates (ID) (wid).slider_value, 11, 11);
      end if;
   end Slider_Draw;

   procedure Clutterbar_Mouse_Down
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
   begin
      null;
   end Clutterbar_Mouse_Down;

   procedure Clutterbar_Mouse_Up
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
   begin
      null;
   end Clutterbar_Mouse_Up;

   procedure Clutterbar_Mouse_Move
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
   begin
      null;
   end Clutterbar_Mouse_Move;

   procedure Clutterbar_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural) is
   begin
      null;
   end Clutterbar_Draw;

   procedure Song_Title_Mouse_Down
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      null;
   end Song_Title_Mouse_Down;

   procedure Song_Title_Mouse_Up
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      null;
   end Song_Title_Mouse_Up;

   procedure Song_Title_Mouse_Move
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      null;
   end Song_Title_Mouse_Move;

   procedure Song_Title_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural)
   is
   begin
      null;
   end Song_Title_Draw;

   procedure Scroll_Mouse_Down
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      null;
   end Scroll_Mouse_Down;

   procedure Scroll_Mouse_Up
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      null;
   end Scroll_Mouse_Up;

   procedure Scroll_Mouse_Move
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer) is
   begin
      null;
   end Scroll_Mouse_Move;

   procedure Scroll_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural)
   is
   begin
      null;
   end Scroll_Draw;

   procedure Menu_Mouse_Down
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
   begin
      null; -- TODO:
   end Menu_Mouse_Down;

   procedure Menu_Mouse_Up
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
   begin
      null; -- TODO:
   end Menu_Mouse_Up;

   procedure Menu_Mouse_Move
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
   begin
      null; -- TODO:
   end Menu_Mouse_Move;

   procedure Menu_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural)
   is
   begin
      null;
   end Menu_Draw;

   procedure Resizeable_Background_Mouse_Down
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
   begin
      if Y < 16 or Skin.easymove then
         Capture_Mouse(Skin, ID, Wid);
         Skin.Last_X := X;
         Skin.Last_Y := Y;
      end if;
   end Resizeable_Background_Mouse_Down;

   procedure Resizeable_Background_Mouse_Up
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
   begin
      if Y < 16 or Skin.easymove then
         Release_Mouse (Skin);
      end if;
   end Resizeable_Background_Mouse_Up;

   procedure Resizeable_Background_Mouse_Move
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural;
      X, Y : Integer)
   is
   begin
      if Skin.Capture = Wid then
         Skin.Gui.move_window (Skin.Windows (ID), X - Skin.Last_X, Y - Skin.Last_Y);
      end if;
   end Resizeable_Background_Mouse_Move;

   procedure Resizeable_Background_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural)
   is
      RB_Top_Left : Subbitmap := Skin.Templates (ID) (Wid).RB_top_left.all;
      RB_Top_Right : Subbitmap := Skin.Templates (ID) (Wid).RB_top_right.all;
      RB_Top : Subbitmap := Skin.Templates (ID) (Wid).RB_top.all;
      RB_Bottom : Subbitmap := Skin.Templates (ID) (Wid).RB_bottom.all;
      RB_Left : Subbitmap := Skin.Templates (ID) (Wid).RB_left.all;
      RB_Right : Subbitmap := Skin.Templates (ID) (Wid).RB_right.all;
      RB_Bottom_Left : Subbitmap := Skin.Templates (ID) (Wid).RB_bottom_left.all;
      RB_Bottom_Right : Subbitmap := Skin.Templates (ID) (Wid).RB_bottom_right.all;
   begin
      Draw_Pixmap (Skin, RB_Top_Left, 0, 0, 25, 20);
      draw_pixmap(Skin, RB_Top_Right, Skin.Templates (ID) (Wid).r(3) - 25, 0, 25, 20);
      draw_pixmap_loop_horizontal(Skin, RB_Top, 25, 0, 25, 20, Skin.Templates (ID) (Wid).r(3)-50);
      draw_pixmap_loop_horizontal(Skin, RB_Bottom, 0, Skin.Templates (ID) (Wid).r(4) - Skin.Templates (ID) (Wid).RB_length_bottom, 25, Skin.Templates (ID) (Wid).RB_length_bottom, Skin.Templates (ID) (Wid).r(3));
      draw_pixmap_loop_vertical(Skin, RB_Left, 0, 20, 25, 29, Skin.Templates (ID) (Wid).r(4)-19*2);
      draw_pixmap_loop_vertical(Skin, RB_Right, Skin.Templates (ID) (Wid).r(3)-25, 20, 25, 29, Skin.Templates (ID) (Wid).r(4)-19*2);
      draw_pixmap(Skin, RB_Bottom_Left, 0, Skin.Templates (ID) (Wid).r(4) - Skin.Templates (ID) (Wid).RB_length_bottom, 125, Skin.Templates (ID) (Wid).RB_length_bottom);
      draw_pixmap(Skin, RB_Bottom_Right, Skin.Templates (ID) (Wid).r(3) - 150, Skin.Templates (ID) (Wid).r(4) - Skin.Templates (ID) (Wid).RB_length_bottom, 150, Skin.Templates (ID) (Wid).RB_length_bottom);
   end Resizeable_Background_Draw;

   procedure Pledit_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID;
      wid : Natural)
   is
      r : Rect := Skin.Templates (ID) (wid).r;
   begin
      Draw_Text (Skin, r (1), r (2), r (3), r (4), "Test");
   end Pledit_Draw;

   background_handlers : aliased constant Handler :=
     (mouse_down => Background_Mouse_Down'Access,
      mouse_up => Background_Mouse_Up'Access,
      mouse_move => Background_Mouse_Move'Access,
      draw => Background_Draw'Access);

   button_handlers : aliased constant Handler :=
     (mouse_down => Button_Mouse_Down'Access,
      mouse_up => Button_Mouse_Up'Access,
      mouse_move => Button_Mouse_Move'Access,
      draw => Button_Draw'Access);

   checkbox_handlers : aliased constant Handler :=
     (mouse_down => Checkbox_Mouse_Down'Access,
      mouse_up => Checkbox_Mouse_Up'Access,
      mouse_move => Checkbox_Mouse_Move'Access,
      draw => Checkbox_Draw'Access);

   slider_handlers : aliased constant Handler :=
     (mouse_down => Slider_Mouse_Down'Access,
      mouse_up => Slider_Mouse_Up'Access,
      mouse_move => Slider_Mouse_Move'Access,
      draw => Slider_Draw'Access);

   clutterbar_handlers : aliased constant Handler :=
     (mouse_down => Clutterbar_Mouse_Down'Access,
      mouse_up => Clutterbar_Mouse_Up'Access,
      mouse_move => Clutterbar_Mouse_Move'Access,
      draw => Clutterbar_Draw'Access);

   song_title_handlers : aliased constant Handler :=
     (mouse_down => Song_Title_Mouse_Down'Access,
      mouse_up => Song_Title_Mouse_Up'Access,
      mouse_move => Song_Title_Mouse_Move'Access,
      draw => Song_Title_Draw'Access);

   scroll_handlers : aliased constant Handler :=
     (mouse_down => Scroll_Mouse_Down'Access,
      mouse_up => Scroll_Mouse_Up'Access,
      mouse_move => Scroll_Mouse_Move'Access,
      draw => Scroll_Draw'Access);

   menu_handlers : aliased constant Handler :=
     (mouse_down => Menu_Mouse_Down'Access,
      mouse_up => Menu_Mouse_Up'Access,
      mouse_move => Menu_Mouse_Move'Access,
      draw => Menu_Draw'Access);

   Resizeable_Background_Handlers : aliased constant Handler :=
     (mouse_down => Resizeable_Background_Mouse_Down'Access,
      mouse_up => Resizeable_Background_Mouse_Up'Access,
      mouse_move => Resizeable_Background_Mouse_Move'Access,
      draw => Resizeable_Background_Draw'Access);

   Pledit_Handlers : aliased constant Handler :=
     (mouse_down => null,
      mouse_up => null,
      mouse_move => null,
      draw => Pledit_Draw'Access);

   type Control_Type is array (Widget_Type) of Handler_Access;

   Control : constant Control_Type :=
     (Background_Widget => background_handlers'Access,
      Button_Widget => button_handlers'Access,
      Checkbox_Widget => checkbox_handlers'Access,
      Slider_Widget => slider_handlers'Access,
      Clutterbar_Widget => clutterbar_handlers'Access,
      Song_Title_Widget => song_title_handlers'Access,
      Scroll_Widget => scroll_handlers'Access,
      Menu_Widget => menu_handlers'Access,
      Resizeable_Background_Widget => Resizeable_Background_Handlers'Access,
      Pledit_Widget => Pledit_Handlers'Access);

   volume_backgrounds : aliased constant Subbitmap_Array :=
     (subbmp_volume_bg1'Access,
      subbmp_volume_bg2'Access,
      subbmp_volume_bg3'Access,
      subbmp_volume_bg4'Access,
      subbmp_volume_bg5'Access,
      subbmp_volume_bg6'Access,
      subbmp_volume_bg7'Access,
      subbmp_volume_bg8'Access,
      subbmp_volume_bg9'Access,
      subbmp_volume_bg10'Access,
      subbmp_volume_bg11'Access,
      subbmp_volume_bg12'Access,
      subbmp_volume_bg13'Access,
      subbmp_volume_bg14'Access,
      subbmp_volume_bg15'Access,
      subbmp_volume_bg16'Access,
      subbmp_volume_bg17'Access,
      subbmp_volume_bg18'Access,
      subbmp_volume_bg19'Access,
      subbmp_volume_bg20'Access,
      subbmp_volume_bg21'Access,
      subbmp_volume_bg22'Access,
      subbmp_volume_bg23'Access,
      subbmp_volume_bg24'Access,
      subbmp_volume_bg25'Access,
      subbmp_volume_bg26'Access,
      subbmp_volume_bg27'Access,
      subbmp_volume_bg28'Access
     );

   balance_backgrounds : aliased constant Subbitmap_Array :=
     (subbmp_balance_bg28'Access,
      subbmp_balance_bg26'Access,
      subbmp_balance_bg24'Access,
      subbmp_balance_bg22'Access,
      subbmp_balance_bg20'Access,
      subbmp_balance_bg18'Access,
      subbmp_balance_bg16'Access,
      subbmp_balance_bg14'Access,
      subbmp_balance_bg12'Access,
      subbmp_balance_bg10'Access,
      subbmp_balance_bg8'Access,
      subbmp_balance_bg6'Access,
      subbmp_balance_bg4'Access,
      subbmp_balance_bg1'Access,
      subbmp_balance_bg2'Access,
      subbmp_balance_bg4'Access,
      subbmp_balance_bg6'Access,
      subbmp_balance_bg8'Access,
      subbmp_balance_bg10'Access,
      subbmp_balance_bg12'Access,
      subbmp_balance_bg14'Access,
      subbmp_balance_bg16'Access,
      subbmp_balance_bg18'Access,
      subbmp_balance_bg20'Access,
      subbmp_balance_bg22'Access,
      subbmp_balance_bg24'Access,
      subbmp_balance_bg26'Access,
      subbmp_balance_bg28'Access
     );

   menu_add_up_backgrounds : aliased constant Subbitmap_Array :=
     (subbmp_pl_menu_add_url_up'Access,
      subbmp_pl_menu_add_dir_up'Access,
      subbmp_pl_menu_add_file_up'Access);

   menu_add_down_backgrounds : aliased constant Subbitmap_Array :=
     (subbmp_pl_menu_add_url_down'Access,
      subbmp_pl_menu_add_dir_down'Access,
      subbmp_pl_menu_add_file_down'Access);

   procedure cmd_eq_close (Skin : in out Skin_Type) is
   begin
      null;
   end cmd_eq_close;

   procedure cmd_eq_maximize (Skin : in out Skin_Type) is
   begin
      null;
   end cmd_eq_maximize;

   procedure test_checkbox (Skin : in out Skin_Type) is
   begin
      null;
   end test_checkbox;

   procedure cmd_eq_presets (Skin : in out Skin_Type) is
   begin
      null;
   end cmd_eq_presets;

   eq_slider_backgrounds : aliased constant Subbitmap_Array :=
     (subbmp_eq_slider_bg1'Access,
      subbmp_eq_slider_bg2'Access,
      subbmp_eq_slider_bg3'Access,
      subbmp_eq_slider_bg4'Access,
      subbmp_eq_slider_bg5'Access,
      subbmp_eq_slider_bg6'Access,
      subbmp_eq_slider_bg7'Access,
      subbmp_eq_slider_bg8'Access,
      subbmp_eq_slider_bg9'Access,
      subbmp_eq_slider_bg10'Access,
      subbmp_eq_slider_bg11'Access,
      subbmp_eq_slider_bg12'Access,
      subbmp_eq_slider_bg13'Access,
      subbmp_eq_slider_bg14'Access,
      subbmp_eq_slider_bg15'Access,
      subbmp_eq_slider_bg16'Access,
      subbmp_eq_slider_bg17'Access,
      subbmp_eq_slider_bg18'Access,
      subbmp_eq_slider_bg19'Access,
      subbmp_eq_slider_bg20'Access,
      subbmp_eq_slider_bg21'Access,
      subbmp_eq_slider_bg22'Access,
      subbmp_eq_slider_bg23'Access,
      subbmp_eq_slider_bg24'Access,
      subbmp_eq_slider_bg25'Access,
      subbmp_eq_slider_bg26'Access,
      subbmp_eq_slider_bg27'Access,
      subbmp_eq_slider_bg28'Access
     );

   -- TODO: missing template functions.

   function Collision_Detection
     (Skin : in Skin_Type;
      X, Y : Integer;
      ID : Window_ID)
      return Natural
   is
      R : Rect;
      wid : Natural := 0;
      Num_Controls : Natural;
      I : Natural;
   begin
      Num_Controls := Skin.Templates (ID)'Length;
      I := Num_Controls;
      while I > 0 loop
         R := Skin.Templates (ID) (I).r;
         if X >= r(1) and Y >= r(2)
           and X <= (r(1) + r(3)) and Y < (r(2) + r(4)) then
            Put_Line ("Collision with widget " & I'Image);
            --wid := Temp(I)'Access;
            wid := I;
            exit;
         end if;
         I := I - 1;
      end loop;
      return I;
   end Collision_Detection;

   procedure Template_Mouse_Down
     (Skin : in out Skin_Type;
      ID : Window_ID;
      X, Y : Integer)
   is
      wid : Integer;
   begin
      wid := Collision_Detection(Skin, X, Y, ID);
      if wid /= 0 then
         control (Skin.Templates (ID) (wid).T).mouse_down (Skin, ID, wid, X, Y);
      end if;
   end Template_Mouse_Down;

   procedure Template_Mouse_Up
     (Skin : in out Skin_Type;
      ID : Window_ID;
      X, Y : Integer)
   is
      wid : Natural;
   begin
      if Skin.Capture /= 0 then
         wid := Skin.Capture;
      else
         wid := Collision_Detection(Skin, X, Y, ID);
      end if;

      if wid /= 0 then
         control (Skin.Templates (ID) (wid).T).mouse_up (Skin, ID, wid, X, Y);
      end if;
   end Template_Mouse_Up;

   procedure Template_Mouse_Move
     (Skin : in out Skin_Type;
      ID : Window_ID;
      X, Y : Integer)
   is
      wid : Natural;
   begin
      if Skin.capture /= 0 then
         wid := Skin.capture;
      else
         wid := Collision_Detection(Skin, X, Y, ID);
      end if;

      if wid /= 0 then
         Skin.Gui.set_cursor(Skin.cursors(Skin.Templates (ID) (wid).c));
         control (Skin.Templates (ID) (wid).T).mouse_move (Skin, ID, wid, X, Y);
      end if;
   end Template_Mouse_Move;

   procedure Template_Draw
     (Skin : in out Skin_Type;
      ID : Window_ID)
   is
   begin
      Skin.Gui.begin_drawing(Skin.Windows (ID));
      for I in Skin.Templates (ID)'Range loop
         control (Skin.Templates (ID) (I).T).draw(Skin, ID, I);
      end loop;
      Skin.Gui.end_drawing.all;
   end Template_Draw;

   -- TODO: missing skin callbacks.

   procedure Main_Mouse_Down(Skin : in out Skin_Type; x, y : Integer)
   is
      X2, Y2 : Integer;
      ID : constant Window_ID := MAIN_WINDOW_ID;
   begin
      if Skin.double_size then
         X2 := X / 2;
         Y2 := Y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Down(Skin, ID, X2, Y2);
   end Main_Mouse_Down;

   procedure Main_Mouse_Up(Classic_Skin : in out Skin_Type; x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if Classic_Skin.double_size then
         X2 := X / 2;
         Y2 := Y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Up(Classic_Skin, MAIN_WINDOW_ID, X2, Y2);
   end Main_Mouse_Up;

   procedure Main_Mouse_Move(Classic_Skin : in out Skin_Type; x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if Classic_Skin.double_size then
         X2 := x / 2;
         Y2 := y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Move(Classic_Skin, MAIN_WINDOW_ID, X2, Y2);
   end Main_Mouse_Move;

   procedure Main_Draw (Classic_Skin : in out Skin_Type)
   is
   begin
      Put_Line("main: draw");
      template_draw(Classic_Skin, MAIN_WINDOW_ID);
   end Main_Draw;

   procedure Equalizer_Mouse_Down(Classic_Skin : in out Skin_Type; x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if Classic_Skin.double_size then
         X2 := X / 2;
         Y2 := Y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Down(Classic_Skin, EQUALIZER_WINDOW_ID, X2, Y2);
   end Equalizer_Mouse_Down;

   procedure Equalizer_Mouse_Up(Classic_Skin : in out Skin_Type; x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if Classic_Skin.double_size then
         X2 := X / 2;
         Y2 := Y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Up(Classic_Skin, EQUALIZER_WINDOW_ID, X2, Y2);
   end Equalizer_Mouse_Up;

   procedure Equalizer_Mouse_Move(Classic_Skin : in out Skin_Type; x, y : Integer)
   is
      X2, Y2 : Integer;
   begin
      if Classic_Skin.double_size then
         X2 := x / 2;
         Y2 := y / 2;
      else
         X2 := X;
         Y2 := Y;
      end if;

      Template_Mouse_Move(Classic_Skin, EQUALIZER_WINDOW_ID, X2, Y2);
   end Equalizer_Mouse_Move;

   procedure Equalizer_Draw (Classic_Skin : in out Skin_Type)
   is
   begin
      Put_Line("equalizer: draw");
      template_draw(Classic_Skin, EQUALIZER_WINDOW_ID);
   end Equalizer_Draw;

   procedure Playlist_Draw (Classic_Skin : in out Skin_Type)
   is
   begin
      Template_Draw(Classic_Skin, PLAYLIST_WINDOW_ID);
   end Playlist_Draw;

   main_callbacks : aliased Skin_Callbacks :=
     (T => Classic_Skin,
      mouse_down => Main_Mouse_Down'Access,
      mouse_up => Main_Mouse_Up'Access,
      mouse_move => Main_Mouse_Move'Access,
      draw => Main_Draw'Access,
      focus => null,
      resize => null);

   equalizer_callbacks : aliased Skin_Callbacks :=
     (T => Classic_Skin,
      mouse_down => Equalizer_Mouse_Down'Access,
      mouse_up => Equalizer_Mouse_Up'Access,
      mouse_move => Equalizer_Mouse_Move'Access,
      draw => Equalizer_Draw'Access,
      focus => null,
      resize => null);

   Playlist_Callbacks : aliased Skin_Callbacks :=
     (T => Classic_Skin,
      mouse_down => null,
      mouse_up => null,
      mouse_move => null,
      draw => Playlist_Draw'Access,
      focus => null,
      resize => null);

   Main_Template : constant Template :=
     ((Background_Widget,
      (0, 0, 275, 116),
      CURSOR_NORMAL,
      subbmp_main'Access,
      False),
      (Background_Widget,
       (0, 0, 275, 14),
       CURSOR_TITLEBAR,
       subbmp_title_bar_off'Access,
       False),
      (Background_Widget,
       (212, 41, 29, 12),
       CURSOR_NORMAL,
       subbmp_mono_off'Access,
       False),
      (Background_Widget,
       (939, 41, 29, 12),
       CURSOR_NORMAL,
       subbmp_stereo_off'Access,
       False),
      (Background_Widget,
       (26, 28, 9, 9),
       CURSOR_NORMAL,
       subbmp_status_stop'Access,
       False),
      (Button_Widget,
       (6, 3, 9, 9),
       CURSOR_NORMAL,
       subbmp_options_up'Access,
       subbmp_options_down'Access,
       Test_Button'Access),
      (Button_Widget,
       (244, 3, 9, 9),
       CURSOR_MIN,
       subbmp_minimize_up'Access,
       subbmp_minimize_down'Access,
       Cmd_Main_Minimize'Access),
      (Button_Widget,
       (254, 3, 9, 9),
       CURSOR_NORMAL,
       subbmp_maximize_normal_up'Access,
       subbmp_maximize_normal_down'Access,
       Cmd_Main_Maximize'Access),
      (Button_Widget,
       (264, 3, 9, 9),
       CURSOR_CLOSE,
       subbmp_close_up'Access,
       subbmp_close_down'Access,
       Cmd_Main_Close'Access),
      (Button_Widget,
       (16+23*0, 88, 23, 18),
       CURSOR_NORMAL,
       subbmp_previous_up'Access,
       subbmp_previous_down'Access,
       Cmd_Main_Previous'Access),
      (Button_Widget,
       (16+23*1-1, 88, 23, 18),
       CURSOR_NORMAL,
       subbmp_play_up'Access,
       subbmp_play_down'Access,
       Cmd_Main_Play'Access),
      (Button_Widget,
       (16+23*2-1, 88, 23, 18),
       CURSOR_NORMAL,
       subbmp_pause_up'Access,
       subbmp_pause_down'Access,
       Cmd_Main_Pause'Access),
      (Button_Widget,
       (16+23*3-1, 88, 23, 18),
       CURSOR_NORMAL,
       subbmp_stop_up'Access,
       subbmp_stop_down'Access,
       Cmd_Main_Stop'Access),
      (Button_Widget,
       (16+23*4-1, 88, 23, 18),
       CURSOR_NORMAL,
       subbmp_next_up'Access,
       subbmp_next_down'Access,
       Cmd_Main_Next'Access),
      (Button_Widget,
       (136, 89, 22, 16),
       CURSOR_NORMAL,
       subbmp_eject_up'Access,
       subbmp_eject_down'Access,
       Cmd_Main_Eject'Access),
      (Background_Widget,
       (26, 28, 26+9, 28+9),
       CURSOR_NORMAL,
       null,
       False),
      (T => Slider_Widget,
       r => (107, 57, 68, 14),
       c => CURSOR_NORMAL,
       slider_background => volume_backgrounds'Access,
       slider_up => subbmp_volume_bar_up'Access,
       slider_down => subbmp_volume_bar_down'Access,
       slider_horizontal => True,
       slider_min => 0,
       slider_max => 51,
       slider_value => 40,
       slider_action => null),
      (T => Slider_Widget,
       r => (177, 57, 38, 14),
       c => CURSOR_NORMAL,
       slider_background => balance_backgrounds'Access,
       slider_up => subbmp_balance_bar_up'Access,
       slider_down => subbmp_balance_bar_down'Access,
       slider_horizontal => True,
       slider_min => 0,
       slider_max => 24,
       slider_value => 12,
       slider_action => null),
      (Checkbox_Widget,
       (219, 58, 23, 12),
       CURSOR_NORMAL,
       subbmp_eq_on_up'Access,
       subbmp_eq_on_down'Access,
       subbmp_eq_off_up'Access,
       subbmp_eq_off_down'Access,
       False,
       Cmd_Main_Eq'Access),
      (Checkbox_Widget,
       (219+23, 58, 23, 12),
       CURSOR_NORMAL,
       subbmp_pl_on_up'Access,
       subbmp_pl_on_down'Access,
       subbmp_pl_off_up'Access,
       subbmp_pl_off_down'Access,
       False,
       Cmd_Main_Pl'Access),
      (Checkbox_Widget,
       (165, 89, 46, 15),
       CURSOR_NORMAL,
       subbmp_shuffle_on_up'Access,
       subbmp_shuffle_on_down'Access,
       subbmp_shuffle_off_up'Access,
       subbmp_shuffle_off_down'Access,
       False,
       Test_Checkbox'Access),
      (Checkbox_Widget,
       (210, 89, 29, 15),
       CURSOR_NORMAL,
       subbmp_repeat_on_up'Access,
       subbmp_repeat_on_down'Access,
       subbmp_repeat_off_up'Access,
       subbmp_repeat_off_down'Access,
       False,
       Test_Checkbox'Access),
      (T => Clutterbar_Widget,
       r => (10, 22, 8, 43),
       c => CURSOR_NORMAL,
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
       null,
       0),
      (Scroll_Widget,
       (16, 72, 248, 10),
       CURSOR_NORMAL,
       subbmp_posbar_background'Access,
       subbmp_posbar_bar_up'Access,
       subbmp_posbar_bar_down'Access,
       29,
       0));

   Equalizer_Template : constant Template :=
     ((T => Background_Widget,
       r => (0, 0, 275, 116),
       c => CURSOR_NORMAL,
       subbmp => subbmp_eq_background'Access,
       move_window => False),
      -- title bar
      (T => Background_Widget,
       r => (0, 0, 275, 14),
       c => CURSOR_NORMAL,
       subbmp => subbmp_eq_title_bar_off'Access,
       move_window => False),
      (T => Button_Widget,
       r => (254, 3, 9, 9),
       c => CURSOR_NORMAL,
       button_up => subbmp_maximize_normal_up'Access,
       button_down => subbmp_maximize_normal_down'Access,
       button_action => cmd_eq_maximize'Access),
      (T => Button_Widget,
       r => (264, 3, 9, 9),
       c => CURSOR_CLOSE,
       button_up => subbmp_close_up'Access,
       button_down => subbmp_close_down'Access,
       button_action => cmd_eq_close'Access),
      -- on auto buttons
      (T => Checkbox_Widget,
       r => (14, 18, 26, 12),
       c => CURSOR_NORMAL,
       checkbox_on_up => subbmp_eq_on_on_up'Access,
       checkbox_on_down => subbmp_eq_on_on_down'Access,
       checkbox_off_up => subbmp_eq_on_off_up'Access,
       checkbox_off_down => subbmp_eq_on_off_down'Access,
       checkbox_action => test_checkbox'Access,
       checkbox_checked => False),
      (T => Checkbox_Widget,
       r => (40, 18, 32, 12),
       c => CURSOR_NORMAL,
       checkbox_on_up => subbmp_eq_auto_on_up'Access,
       checkbox_on_down => subbmp_eq_auto_on_down'Access,
       checkbox_off_up => subbmp_eq_auto_off_up'Access,
       checkbox_off_down => subbmp_eq_auto_off_down'Access,
       checkbox_action => test_checkbox'Access,
       checkbox_checked => False),
      -- presets button
      (T => Button_Widget,
       r => (217, 18, 44, 12),
       c => CURSOR_NORMAL,
       button_up => subbmp_eq_presets_up'Access,
       button_down => subbmp_eq_presets_down'Access,
       button_action => cmd_eq_presets'Access),
      -- preamp
      (T => Slider_Widget,
       r => (21, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*0, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*1, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*2, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*3, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*4, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*5, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*6, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*7, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*8, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null),
      (T => Slider_Widget,
       r => (78+18*9, 38, 14, 64),
       c => CURSOR_NORMAL,
       slider_background => eq_slider_backgrounds'Access,
       slider_up => subbmp_eq_slider_up'Access,
       slider_down => subbmp_eq_slider_down'Access,
       slider_horizontal => false,
       slider_min => 0,
       slider_max => 51,
       slider_value => 25,
       slider_action => null));

   Playlist_Template : constant Template :=
     ((T => Resizeable_Background_Widget,
       r => (0, 0, 275, 116),
       c => CURSOR_NORMAL,
       RB_top_left => subbmp_pl_top_left_off'Access,
       RB_title => subbmp_pl_title_off'Access,
       RB_top => subbmp_pl_top_off'Access,
       RB_top_right => subbmp_pl_top_right_off'Access,
       RB_left => subbmp_pl_left'Access,
       RB_right => subbmp_pl_right'Access,
       RB_bottom => subbmp_pl_bottom'Access,
       RB_bottom_left => subbmp_pl_bottom_left'Access,
       RB_bottom_right => subbmp_pl_bottom_right'Access,
       RB_length_top => 0,
       RB_length_left => 0,
       RB_length_right => 0,
       RB_length_bottom => 38),
      (T => Pledit_Widget,
       r => (12, 20, 243, 58),
       c => CURSOR_NORMAL),
      --   -- menus
      (T => Menu_Widget,
       r => (11, 86, 25, 18),
       c => CURSOR_NORMAL,
       Menu_Num_Buttons => 3,
       Menu_Buttons_Up => menu_add_up_backgrounds'Access,
       Menu_Buttons_Down => menu_add_down_backgrounds'Access,
       Menu_Bar => subbmp_pl_menu_add_bar'Access),
      --  (T => Menu_Widget,
      --   r => (40, 86, 25, 18),
      --   c => CURSOR_NORMAL,
      --   control => menu_handlers,
      --   &data_menu_rem),
      --  (T => Menu_Widget,
      --   r => (69, 86, 25, 18),
      --   c => CURSOR_NORMAL,
      --   control => menu_handlers,
      --   &data_menu_sel),
      --  (T => Menu_Widget,
      --   r => (98, 86, 25, 18),
      --   c => CURSOR_NORMAL,
      --   control => menu_handlers,
      --   &data_menu_misc),
      --  (T => Menu_Widget,
      --   r => (228, 86, 25, 18},
      --   c => CURSOR_NORMAL,
      --   control => menu_handlers,
      --   &data_menu_list),
      --   -- playback
      (T => Button_Widget,
       r => (0, 0, 0, 0),
       c => CURSOR_NORMAL,
       button_up => null,
       button_down => null,
       button_action => Test_Button'Access),
      (T => Button_Widget,
       r => (131, 101, 8, 8),
       c => CURSOR_NORMAL,
       button_up => null,
       button_down => null,
       button_action => Test_Button'Access),
      (T => Button_Widget,
       r => (0, 0, 0, 0),
       c => CURSOR_NORMAL,
       button_up => null,
       button_down => null,
       button_action => Test_Button'Access),
      (T => Button_Widget,
       r => (0, 0, 0, 0),
       c => CURSOR_NORMAL,
       button_up => null,
       button_down => null,
       button_action => Test_Button'Access),
      (T => Button_Widget,
       r => (0, 0, 0, 0),
       c => CURSOR_NORMAL,
       button_up => null,
       button_down => null,
       button_action => Test_Button'Access),
      (T => Button_Widget,
       r => (0, 0, 0, 0),
       c => CURSOR_NORMAL,
       button_up => null,
       button_down => null,
       button_action => Test_Button'Access)
      --
      --    ((256, 97, 19, 19),
      --     CURSOR_NORMAL,
      --     grip_handlers,
      --     &data_pl_grip),
      --    (T => Button_Widget,
      --     (254, 3, 9, 9),
      --     CURSOR_NORMAL,
      --     button_handlers,
      --     &data_pl_maximize),
      --    (T => Button_Widget,
      --     (264, 3, 9, 9),
      --     CURSOR_CLOSE,
      --     button_handlers,
      --     &data_pl_close)
     );

   procedure New_Classic
     (Skin : in out Skin_Type;
      GUI : Gui_Dispatch)
   is
      bmp : Overkill.Gui.Pixmap;
      cur : Overkill.Gui.Cursor;

      procedure Load_Bitmap (I : Bitmap_Type; Filename : String)
      is
      begin
         bmp := Gui.load_image ("skins/classic/" & Filename);
         if bmp = Pixmap (Null_Address) then
            Put_Line (Filename & ": could not load bmp file");
         end if;
         Skin.bmps(I) := bmp;
      end Load_Bitmap;

      procedure Load_Cursor (I : Cursor_Type; Filename : String)
      is
      begin
         cur := Gui.load_cursor ("skins/classic/" & Filename);
         if cur = Cursor (Null_Address) then
            Put_Line (Filename & ": could not load cursor file");
         end if;
         Skin.cursors (I) := cur;
      end Load_Cursor;
   begin
      Put_Line ("Loading classic skin.");
      Skin.Gui := GUI;

      Skin.Templates :=
        (MAIN_WINDOW_ID => new Template'(Main_Template),
         EQUALIZER_WINDOW_ID => new Template' (Equalizer_Template),
         PLAYLIST_WINDOW_ID => new Template'(Playlist_Template));

      Skin.Windows (MAIN_WINDOW_ID) := Gui.create_window (0, 0, 275, 116, "Main", main_callbacks);
      if Skin.Windows (MAIN_WINDOW_ID) = Window_Type(Null_Address) then
         raise Program_Error with "Failed to create Main Window.";
      end if;
      main_window := Skin.Windows (MAIN_WINDOW_ID);
      Skin.Windows (EQUALIZER_WINDOW_ID) := Gui.create_window (0, 116, 275, 116, "Equalizer", equalizer_callbacks);
      if Skin.Windows (EQUALIZER_WINDOW_ID) = Window_Type(Null_Address) then
         raise Program_Error with "Failed to create Equalizer Window.";
      end if;
      Skin.Windows (PLAYLIST_WINDOW_ID) := Gui.create_window (0, 116*2, 275, 116, "Playlist", Playlist_Callbacks);
      if Skin.Windows (PLAYLIST_WINDOW_ID) = Window_Type(Null_Address) then
         raise Program_Error with "Failed to create Playlist Window.";
      end if;

      Load_Bitmap (BMP_BALANCE, BALANCE_BMP);
      Load_Bitmap (BMP_CBUTTONS, CBUTTONS_BMP);
      Load_Bitmap (BMP_EQ_EX, EQ_EX_BMP);
      Load_Bitmap (BMP_EQMAIN, EQMAIN_BMP);
      Load_Bitmap (BMP_EQMAIN_ISO, EQMAIN_ISO_BMP);
      Load_Bitmap (BMP_GEN, GEN_BMP);
      Load_Bitmap (BMP_GENEX, GENEX_BMP);
      Load_Bitmap (BMP_MAIN, MAIN_BMP);
      Load_Bitmap (BMP_MB, MB_BMP);
      Load_Bitmap (BMP_MONOSTER, MONOSTER_BMP);
      Load_Bitmap (BMP_NUMBERS, NUMBERS_BMP);
      Load_Bitmap (BMP_NUMS_EX, NUMS_EX_BMP);
      Load_Bitmap (BMP_PLAYPAUS, PLAYPAUS_BMP);
      Load_Bitmap (BMP_PLEDIT, PLEDIT_BMP);
      Load_Bitmap (BMP_POSBAR, POSBAR_BMP);
      Load_Bitmap (BMP_SHUFREP, SHUFREP_BMP);
      Load_Bitmap (BMP_TEXT, TEXT_BMP);
      Load_Bitmap (BMP_TITLEBAR, TITLEBAR_BMP);
      Load_Bitmap (BMP_VIDEO, VIDEO_BMP);
      Load_Bitmap (BMP_VOLUME, VOLUME_BMP);

      Load_Cursor (CURSOR_CLOSE, CLOSE_CUR);
      Load_Cursor (CURSOR_EQCLOSE, EQCLOSE_CUR);
      Load_Cursor (CURSOR_EQNORMAL, EQNORMAL_CUR);
      Load_Cursor (CURSOR_EQSLID, EQSLID_CUR);
      Load_Cursor (CURSOR_EQTITLE, EQTITLE_CUR);
      Load_Cursor (CURSOR_MAINMENU, MAINMENU_CUR);
      Load_Cursor (CURSOR_MAINMENU, MIN_CUR);
      Load_Cursor (CURSOR_NORMAL, NORMAL_CUR);
      Load_Cursor (CURSOR_PCLOSE, PCLOSE_CUR);
      Load_Cursor (CURSOR_PNORMAL, PNORMAL_CUR);
      Load_Cursor (CURSOR_POSBAR, POSBAR_CUR);
      Load_Cursor (CURSOR_PSIZE, PSIZE_CUR);
      Load_Cursor (CURSOR_PTBAR, PTBAR_CUR);
      Load_Cursor (CURSOR_PVSCROLL, PVSCROLL_CUR);
      Load_Cursor (CURSOR_PWINBUT, PWINBUT_CUR);
      Load_Cursor (CURSOR_PWSNORM, PWSNORM_CUR);
      Load_Cursor (CURSOR_PWSSIZE, PWSSIZE_CUR);
      Load_Cursor (CURSOR_SONGNAME, SONGNAME_CUR);
      Load_Cursor (CURSOR_TITLEBAR, TITLEBAR_CUR);
      Load_Cursor (CURSOR_VOLBAL, VOLBAL_CUR);
      Load_Cursor (CURSOR_VOLBAR, VOLBAR_CUR);
      Load_Cursor (CURSOR_WINBUT, WINBUT_CUR);
      Load_Cursor (CURSOR_WSCLOSE, WSCLOSE_CUR);
      Load_Cursor (CURSOR_WSMIN, WSMIN_CUR);
      Load_Cursor (CURSOR_WSNORMAL, WSNORMAL_CUR);
      Load_Cursor (CURSOR_WSPOSBAR, WSPOSBAR_CUR);
      Load_Cursor (CURSOR_WSWINBUT, WSWINBUT_CUR);

      -- ShowWindow might be ignored the first time it's called on win32
      Gui.show_window (Skin.Windows (MAIN_WINDOW_ID));
      Gui.show_window (Skin.Windows (MAIN_WINDOW_ID));
      Gui.show_window (Skin.Windows (EQUALIZER_WINDOW_ID));
      Gui.show_window (Skin.Windows (PLAYLIST_WINDOW_ID));
   end New_Classic;

   procedure Finalize
     (Skin : in out Skin_Type)
   is
   begin
      Put_Line ("Finalizing classic skin.");
      for I in Skin.bmps'Range loop
         Skin.Gui.unload_image(Skin.bmps(I));
      end loop;
      for I in Skin.cursors'Range loop
         Skin.Gui.unload_cursor(Skin.cursors(I));
      end loop;
      Skin.Gui.destroy_window(Skin.Windows (PLAYLIST_WINDOW_ID));
      Skin.Gui.destroy_window(Skin.Windows (EQUALIZER_WINDOW_ID));
      Skin.Gui.destroy_window(Skin.Windows (MAIN_WINDOW_ID));
   end Finalize;

   procedure Run
     (Skin : in out Skin_Type;
      Discovery : Discovery_Access)
   is
   begin
      Put_Line ("Entering event loop.");
      Skin.Discovery := Discovery;
      Skin.Gui.event_handler.all;
      Put_Line ("Leaving event loop.");
   end Run;
end Overkill.Classic;
