with Linux;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;

package body Sysinfo is

   function Find_System return System is
      Info    : aliased  Utsname;
      Int_Ret : constant Interfaces.C.int := uname (Info'Access);
   begin
      Put_Line (Int_Ret'Image);
      if Int_Ret = 0 then
         Put_Line (" System  Name: " & To_Ada (Info.sysname));
         Put_Line (" Machine Name: " & To_Ada (Info.machine));
         Put_Line (" Domain  Name: " & To_Ada (Info.domainname));
         Put_Line (" Release Name: " & To_Ada (Info.release));
         Put_Line (" Version Name: " & To_Ada (Info.version));
      end if;
      return Sys_Linux;
   end Find_System;

   procedure Get_Sys_Information is
   begin
         if Find_System = Sys_Linux then
            Linux.Get_ALL;
         end if;
   end Get_Sys_Information;

end Sysinfo;
