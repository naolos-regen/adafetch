package Linux.Info.Graphics_Card is
   Graphics_Card_Information    : constant String := "/sys/class/card/drm/";
   Alternative_Card_Information : constant String := "/sys/class/graphics/fb0/device/";

   type Graphics_Card_Information is record
      Device               : constant Integer;
      Vendor               : constant Integer;
      Name_Hardware_Data   : constant String;
      Virtual_Memory       : constant Integer;
   end record;

   type Graphics_Card_Information_Pointer is access all Graphics_Card_Information;

   function Get_Graphics_Card_Information return Graphics_Card_Information_Pointer;

end Linux.Info.Graphics_Card;
