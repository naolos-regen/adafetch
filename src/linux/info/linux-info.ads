with Linux.Info.Battery;          use Linux.Info.Battery;
with Linux.Info.Bios;             use Linux.Info.Bios;
with Linux.Info.System;           use Linux.Info.System;
with Linux.Info.Disk;             use Linux.Info.Disk;
with Linux.Info.Operating_System; use Linux.Info.Operating_System;
with Linux.Info.CPU_Useage;       use Linux.Info.CPU_Useage;
with Linux.Info.Graphics_Card;    use Linux.Info.Graphics_Card;
with Constants; use Constants;

package Linux.Info is

   Battery_Path            : constant String := "/sys/class/power_supply/";
   Bios_Path               : constant String := "/sys/devices/virtual/dmi/id/";
   CPU_Path                : constant String := "/proc/cpuinfo";
   CPU_Useage_Path         : constant String := "/proc/stat";
   Graphics_Card_Path      : constant String := "/sys/class/card/drm/";
   Monitor_Brightness_Path : constant String := "/sys/class/backlight/";
   Operating_System_Path   : constant String := "/etc/os-release";
   Uptime_Info_Path        : constant String := "/proc/uptime";

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

   function Get_Information return Information_Pointer;

end Linux.Info;
