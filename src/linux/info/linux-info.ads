package Linux.Info is

   Battery_Path            : constant String := "/sys/class/power_supply/";
   Bios_Path               : constant String := "/sys/devices/virtual/dmi/id/";
   CPU_Path                : constant String := "/proc/cpuinfo";
   Graphics_Card_Path      : constant String := "/sys/class/card/drm/";
   Monitor_Brightness_Path : constant String := "/sys/class/backlight/";
   Operating_System_Path   : constant String := "/etc/os-release";
   Uptime_Info_Path        : constant String := "/proc/uptime";

   subtype Percentage          is Integer range 0 .. 100;
   subtype Detailed_Percentage is Float   range 0.0 .. 100.0;

   type Information is record
      Battery_Information            : Battery_Pointer                   := Get_Battery_Information;
      Bios_Information               : Bios_Information_Pointer          := Get_Bios_Information;
      System_Information             : System_Info_Pointer               := Get_System_Information;
      Disk_Information               : Statfs_Pointer                    := Get_Statfs_Information;
      Operating_System_Information   : Operating_System_Release_Pointer  := Get_Operating_System_Release_Information;
      Graphics_Card_Information      : Graphics_Card_Information_Pointer := Get_Graphics_Card_Information;
      CPU_Useage_Information         : constant Percentage               := Get_Cpu_Percentage;
   end record;

   type Information_Pointer is access all Information;

   package Linux.Info.Battery is

      type Capacity_Description is (Unknown, Critical, Low, Normal, High, Full);

      type Battery is record
         Dev_Type           : constant String;
         Name               : constant String;
         Status             : constant String;
         Model_Name         : constant String;
         Manufacturer       : constant String;
         Technology         : constant String;
         Present            : constant Boolean;  -- 1 -> True | 0 -> False
         Cycle_Count        : constant Integer;  -- How Aged is the Battery the higher the older
         Voltage_Min_Design : constant Integer;
         Voltage_Now        : constant Integer;
         Power_Now          : constant Integer;
         Energy_Full_Design : constant Integer;
         Energy_Full        : constant Integer;
         Energy_Now         : constant Integer;
         Capacity           : constant Percentage;
         Capacity_Level     : constant Capacity_Description;
      end record;

      type Battery_Pointer is access all Battery;

      function Find_Path               return String;
      function Get_Battery_Information return Battery_Pointer;

   end Linux.Info.Battery;

   package Linux.Info.System is
      type System_Info is record
            Uptime            : constant Interfaces.C.long;
            Loads             : constant array (0 .. 2) of Interfaces.C.unsigned_long; 
            Total_Ram,        : constant Interfaces.C.unsigned_long;
            Free_Ram,         : constant Interfaces.C.unsigned_long;
            Shared_Ram,       : constant Interfaces.C.unsigned_long;
            Buffer_Ram,       : constant Interfaces.C.unsigned_long;
            Total_Swap,       : constant Interfaces.C.unsigned_long;
            Free_Swap         : constant Interfaces.C.unsigned_long;
            Procs             : constant Interfaces.C.unsigned_short;
            Total_High        : constant Interfaces.C.unsigned_long;
            Free_High         : constant Interfaces.C.unsigned_long;
            Mem_Unit          : constant Interfaces.C.unsigned_long;
            Whatever          : constant array (0 .. 7) of Interfaces.c.char;
      end record;
      pragma Convention (C, Sysinfo);

      function sysinfo (info : access Sysinfo) return Interfaces.C.int;
      pragma Import (C, sysinfo, "sysinfo");

      type System_Info_Pointer is access all System_Info;
      
      function Get_System_Information return System_Info_Pointer;

   end Linux.Info.System;
   
   package Linux.Info.Bios is
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

   package Linux.Info.Cpu_Useage is
      type Cpu_Stats is record
         User        : constant Integer;
         Nice        : constant Integer;
         System      : constant Integer;
         Idle        : constant Integer;
         Io_Wait     : constant Integer;
         Irq         : constant Integer;
         Soft_Irq    : constant Integer;
         Steal       : constant Integer;
         Guest       : constant Integer;
         Guest_Nice  : constant Integer;
      end record;
   
      function Get_Cpu_Percentage return Percentage;

   end Linux.Info.Cpu_Useage;

   package Linux.Info.Disk is
      type Statfs is record 
         F_Type      : constant C.long;
         F_Bsize     : constant C.long;
         F_Blocks    : constant C.unsigned_long;
         F_Bfree     : constant C.unsigned_long;
         F_Bavail    : constant C.unsigned_long;
         F_Files     : constant C.unsigned_long;
         F_Ffree     : constant C.unsigned_long;
         F_Fsid      : constant C.long;
         F_NameLen   : constant C.long;
         F_Frsize    : constant C.long;
         F_Spare     : constant array (0 .. 5) of C.long;
      end record;
      pragma Convention (C, Statfs);

      function statfs (path : Interfaces.C.char_array; buf : access Statfs);
      pragma Import (C, statfs, "statfs");

      type Statfs_Pointer is access all Statfs;

      function Get_Statfs_Information return Statfs_Pointer;

   end Linux.Info.Disk;

   package Linux.Info.Graphics_Card is
      type Graphics_Card_Information is record
         Device               : constant Integer;
         Vendor               : constant Integer;
         Name_Hardware_Data   : constant String;
         Virtual_Memory       : constant Integer;
      end record;

      type Graphics_Card_Information_Pointer is access all Graphics_Card_Information;

      function Get_Graphics_Card_Information return Graphics_Card_Information_Pointer;

   end Linux.Info.Graphics_Card;

   package Linux.Info.Monitor_Brightness is
      -- this might exist and/or might not /sys/class/backlight/<ALL>/
   end Linux.Info.Monitor_Brightness;

   package Linux.Info.Operating_System is
      type Operating_System_Release is record
         Name                 : constant String;
         Pretty Name          : constant String;
         ID                   : constant String;
         Build_ID             : constant String;
         ANSI_Color           : constant String;
         Home_URL             : constant String; -- maybe create URL type (?)
         Documentation_URL    : constant String; -- maybe create URL type (?)
         Support_URL          : constant String; -- maybe create URL type (?)
         Bug_Report_URL       : constant String; -- maybe create URL type (?)
         Privacy_Policy_URL   : constant String; -- maybe create URL type (?)
         Logo                 : constant String;
      end record; 
                  -- other Idea is to interface with C fopen, 
                  -- but ada uses the same but more reliable and safe,
                  -- via the use of lightweight exceptions

      type Operating_System_Release_Pointer is access all Operating_System_Release;

      function Get_Operating_System_Release_Information return Operating_System_Release_Pointer;
      
   end Linux.Info.Operating_System;

end Linux.Info;
