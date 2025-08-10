with Ada.Directories;        use Ada.Directories;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Containers;         use Ada.Containers;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;

package Linux.Info.Bios is
      Bios_Path : constant String := "/sys/devices/virtual/dmi/id/modalias";

      type Bios is record
         Date    : Unbounded_String; -- bd
         Release : Unbounded_String; -- br
         Vendor  : Unbounded_String; -- bvn:
         Version : Unbounded_String; -- bvr:
      end record;
      
      type System is record
         Vendor  : Unbounded_String; -- svn
      end record;

      type Board is record
         Name      : Unbounded_String; -- rn
         Vendor    : Unbounded_String; -- rvn
         Version   : Unbounded_String; -- rvr
      end record;

      type Chassis is record
         Name      : Unbounded_String; -- ct
         Vendor    : Unbounded_String; -- cvn:
         Version   : Unbounded_String; -- cvr:
      end record;

      type Product is record
         Name      : Unbounded_String; -- pn:
         SKU       : Unbounded_String; -- sku:
         Version   : Unbounded_String; -- pvr:
      end record;

      type Bios_Info is record
         Bios_Info    : Bios;
         Board_Info   : Board;
         Chassis_Info : Chassis;
         Product_Info : Product;
         System_Info  : System;
      end record;

      type Bios_Information_Pointer is access all Bios_Info;

      function Empty_Unbounded_String return Unbounded_String;

      Empty_Bios    : Bios    := (others => Empty_Unbounded_String);
      Empty_System  : System  := (others => Empty_Unbounded_String);
      Empty_Board   : Board   := (others => Empty_Unbounded_String);
      Empty_Chassis : Chassis := (others => Empty_Unbounded_String);
      Empty_Product : Product := (others => Empty_Unbounded_String);

      function Get_Bios_Information return Bios_Information_Pointer;

end Linux.Info.Bios;
