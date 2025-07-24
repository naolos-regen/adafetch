with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Linux.Info.Disk is

   Statfs_Error : exception;  

   function Get_Statfs_Information return Statfs_Pointer is
      Statfs_Result : Statfs_Pointer;
   begin
      if statfs (Statfs_Result'Access) /= 0 then
         raise Statfs_Error with "statfs failed: " & Errno_Message (Errno);
      end if;
      return Statfs_Result;
   end Get_Statfs_Information;
      
end Linux.Info.Disk;
