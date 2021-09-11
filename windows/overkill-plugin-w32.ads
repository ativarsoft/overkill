package Overkill.Plugin.W32 is

   type W32_Plugin_Manager_Type is new Plugin_Manager_Type with null record;
   
   procedure Load_Input_Plugin
     (Plugin_Manager : W32_Plugin_Manager_Type;
      Library : Library_Type);
   
   procedure Load_Output_Plugin
     (Plugin_Manager : W32_Plugin_Manager_Type;
      Library : Library_Type);
   
   procedure Load_General_Plugin
     (Plugin_Manager : W32_Plugin_Manager_Type;
      Library : Library_Type);
   
   procedure Load_DSP_Plugin
     (Plugin_Manager : W32_Plugin_Manager_Type;
      Library : Library_Type);
   
   procedure Load_Visualization_Plugin
     (Plugin_Manager : W32_Plugin_Manager_Type;
      Library : Library_Type);
   
   procedure Load_Encoder_Plugin
     (Plugin_Manager : W32_Plugin_Manager_Type;
      Library : Library_Type);

end Overkill.Plugin.W32;
