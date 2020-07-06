with Gui;
use Gui;

package w32 is

   procedure W32_Init;
   procedure W32_Quit;

   function W32_Create_Window(x : Integer; y: Integer; w : Integer; h : Integer; title : String; callbacks : access Skin_Callbacks) return Window;
   procedure W32_Destroy_Window(w : Window);
   procedure W32_Event_Handler;
   procedure W32_Show_Window(w : Window);
   procedure W32_Hide_Window(w : Window);
   procedure W32_Move_Window(w : Window; x : Integer; y : Integer);
   procedure W32_Redraw_Window(w : Window);
   procedure W32_Set_Topmost(w : Window);
   procedure W32_Set_Not_Topmost(w : Window);
   procedure W32_Resize_Window(w : Window; width : Integer; height : Integer);
   procedure W32_Get_Window_Rect(rect : Color);
   procedure W32_Minimize_Window(w : Window);

   function W32_Load_Image(filename : String) return Pixmap;
   procedure W32_Unload_Image(image : Pixmap);
   procedure W32_Begin_Drawing(w : Window);
   procedure W32_Draw_Image(p : Pixmap; dst_x : Integer; dst_y : Integer; w : Integer; h : Integer; src_x : Integer; src_y : Integer);
   procedure W32_Draw_Image_Double(p : Pixmap; dst_x : Integer; dst_y : Integer; w : Integer; h : Integer; src_x : Integer; src_y : Integer);
   procedure W32_Draw_Filled_Rectangle(x : Integer; y : Integer; w : Integer; h : Integer; c : Color);
   procedure W32_End_Drawing;

   procedure W32_Capture_Mouse(w : Window);
   procedure W32_Release_Mouse;
   function W32_Load_Cursor(filename : String) return Cursor;
   procedure W32_Unload_Cursor(p : Cursor);
   procedure W32_Set_Cursor(p : Cursor);

   function W32_Check_Glue(a : Window; b : Window; x : Integer; y : Integer) return Boolean;

   procedure W32_Open_File_Dialog;
   procedure W32_Open_Dir_Dialog;

   w32_gui : gui.Gui_Dispatch := (
                                  init => W32_Init'Access,
                                  quit => W32_Quit'Access,

                                  create_window => W32_Create_Window'Access,
                                  destroy_window => W32_Destroy_Window'Access,
                                  event_handler => W32_Event_Handler'Access,
                                  show_window => W32_Show_Window'Access,
                                  hide_window => W32_Hide_Window'Access,
                                  move_window => W32_Move_Window'Access,
                                  redraw_window => W32_Redraw_Window'Access,
                                  set_topmost => W32_Set_Topmost'Access,
                                  set_not_topmost => W32_Set_Not_Topmost'Access,
                                  resize_window => W32_Resize_Window'Access,
                                  get_window_rect => W32_Get_Window_Rect'Access,
                                  minimize_window => W32_Minimize_Window'Access,

                                  load_image => W32_Load_Image'Access,
                                  unload_image => W32_Unload_Image'Access,
                                  begin_drawing => W32_Begin_Drawing'Access,
                                  draw_image => W32_Draw_Image'Access,
                                  draw_image_double => W32_Draw_Image_Double'Access,
                                  draw_filled_rectangle => W32_Draw_Filled_Rectangle'Access,
                                  end_drawing => W32_End_Drawing'Access,

                                  capture_mouse => W32_Capture_Mouse'Access,
                                  release_mouse => W32_Release_Mouse'Access,
                                  load_cursor => W32_Load_Cursor'Access,
                                  unload_cursor => W32_Unload_Cursor'Access,
                                  set_cursor => W32_Set_Cursor'Access,

                                  check_glue => W32_Check_Glue'Access,

                                  open_file_dialog => W32_Open_File_Dialog'Access,
                                  open_dir_dialog => W32_Open_Dir_Dialog'Access
                                 );

end w32;
