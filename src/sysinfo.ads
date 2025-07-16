with Interfaces.C;

package Sysinfo is
   package C renames Interfaces.C;

   procedure Get_Sys_Information;
private

   type System is (Sys_Linux, Sys_FreeBSD, Sys_OpenBSD,
                   Sys_MacOS, Sys_POSIX, Sys_Other);
   
   type Utsname is record
      sysname    : Interfaces.C.char_array(0 .. 64);
      nodename   : Interfaces.C.char_array(0 .. 64);
      release    : Interfaces.C.char_array(0 .. 64);
      version    : Interfaces.C.char_array(0 .. 64);
      machine    : Interfaces.C.char_array(0 .. 64);
      domainname : Interfaces.C.char_array(0 .. 64);
   end record;
   
   pragma Convention (C, Utsname);

   function uname (name : access Utsname) return C.int;
   pragma Import (C, uname, "uname");

   function Find_System return System;

end Sysinfo;
