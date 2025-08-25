package body Linux.Info.Disk is

   function Get_Statfs_Information return Statfs is
      Statfs_Result : aliased Statfs;
      Statfs_Error : exception;  
   begin
      if statfs ("/", Statfs_Result'Access) /= 0 then
         raise Statfs_Error with "statfs failed: " & Errno_Message;
      end if;
      return Statfs_Result;
   end Get_Statfs_Information;

end Linux.Info.Disk;
