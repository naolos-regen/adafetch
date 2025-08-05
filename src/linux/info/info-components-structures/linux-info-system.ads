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

