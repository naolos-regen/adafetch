with Linux;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;

package body Sysinfo is

   function Find_System return Sys_Type is
      Info    : aliased  Utsname;
      Int_Ret : constant Interfaces.C.int := uname (Info'Access);
   begin
      Put_Line (Int_Ret'Image);
      if Int_Ret = 0 then
         declare
            System_Name : constant String := To_Ada (Info.sysname);
         begin
            if System_Name = "Linux" then
               return Sys_Linux;
            elsif System_Name = "FreeBSD" then
               return Sys_FreeBSD;
            elsif System_Name = "OpenBSD" then
               return Sys_OpenBSD;
            elsif System_Name = "Darwin" then
               return Sys_MacOS;
            else
               return Sys_POSIX;
            end if;
         end;
      end if;
      return Sys_Other;
   end Find_System;

   procedure Get_Sys_Information is
   begin
         if Find_System = Sys_Linux then
            Linux.Get_ALL;
         end if;
   end Get_Sys_Information;

end Sysinfo;
