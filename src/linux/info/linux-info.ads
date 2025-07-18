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
      Battery_Information   : Battery_Pointer     := Get_Battery_Information;;
      System_Information    : System_Info_Pointer := Get_System_Information;
      Disk_Information      : Statfs_Pointer      := Get_Statfs_Information;
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
         Uptime            : Interfaces.C.long;
         Loads             : array (0 .. 2) of Interfaces.C.unsigned_long; 
         Total_Ram,        : Interfaces.C.unsigned_long;
         Free_Ram,         : Interfaces.C.unsigned_long;
         Shared_Ram,       : Interfaces.C.unsigned_long;
         Buffer_Ram,       : Interfaces.C.unsigned_long;
         Total_Swap,       : Interfaces.C.unsigned_long;
         Free_Swap         : Interfaces.C.unsigned_long;
         Procs             : Interfaces.C.unsigned_short;
         Total_High        : Interfaces.C.unsigned_long;
         Free_High         : Interfaces.C.unsigned_long;
         Mem_Unit          : Interfaces.C.unsigned_long;
         Whatever          : array (0 .. 7) of Interfaces.c.char;
      end record;
      pragma Convention (C, Sysinfo);

      function sysinfo (info : access Sysinfo) return Interfaces.C.int;
      pragma Import (C, sysinfo, "sysinfo");

      type System_Info_Pointer is access all System_Info;
      
      function Get_System_Information return System_Info_Pointer;

   end Linux.Info.System;

   package Linux.Info.Bios is

   end Linux.Info.Bios;

   package Linux.Info.Cpu_Cache is

   end Linux.Info.Cpu_Cache;

   package Linux.Info.Cpu_Useage is
      -- getloadavg()
   end Linux.Info.Cpu_Useage;

   package Linux.Info.Disk is
      type Statfs is record 
         F_Type      : C.long;
         F_Bsize     : C.long;
         F_Blocks    : C.unsigned_long;
         F_Bfree     : C.unsigned_long;
         F_Bavail    : C.unsigned_long;
         F_Files     : C.unsigned_long;
         F_Ffree     : C.unsigned_long;
         F_Fsid      : C.long;
         F_NameLen   : C.long;
         F_Frsize    : C.long;
         F_Spare     : array (0 .. 5) of C.long;
      end record;
      pragma Convention (C, Statfs);

      function statfs (path : Interfaces.C.char_array; buf : access Statfs);
      pragma Import (C, statfs, "statfs");

      type Statfs_Pointer is access all Statfs;

      function Get_Statfs_Information return Statfs_Pointer;

   end Linux.Info.Disk;

   package Linux.Info.Disk_IO is

   end Linux.Info.Disk_IO;

   package Linux.Info.Graphics_Card is

   end Linux.Info.Graphics_Card;

   package Linux.Info.Monitor_Brightness is

   end Linux.Info.Monitor_Brightness;

   package Linux.Info.Operating_System is

   end Linux.Info.Operating_System;

end Linux.Info;
