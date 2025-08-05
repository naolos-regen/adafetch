package Linux.Info.Bios is
      Bios_Path : constant String := "/sys/devices/virtual/dmi/id/";

      type Bios is record
         Date    : constant String; -- bd
         Release : constant String; -- br
         Vendor  : constant String; -- bvn:
         Version : constant String; -- bvr:
      end record;

      type System is record
         Vendor  : constant String; -- svn
      end record;

      type Board is record
         Name      : constant String; -- rn
         Vendor    : constant String; -- rvn
         Version   : constant String; -- rvr
      end record;

      type Chassis is record
         Name      : constant String; -- ( Name <=> Type ) ct
         Vendor    : constant String; -- cvn:
         Version   : constant String; -- cvr:
      end record;

      type Product is record
         Name      : constant String; -- pn:
         SKU       : constant String; -- sku:
         Version   : constant String; -- pvr:
      end record;

      type Bios_Info is record
         Bios_Info    : Bios;
         Board_Info   : Board;
         Chassis_Info : Chassis;
         Product_Info : Product;
         System_Info  : System;
      end record;

      type Bios_Information_Pointer is access all Bios_Info;

      function Get_Bios_Information return Bios_Information_Pointer;

end Linux.Info.Bios;
