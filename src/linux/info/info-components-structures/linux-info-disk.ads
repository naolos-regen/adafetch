with Interfaces.C; use Interfaces.C;
with Ada.Text_IO;  use Ada.Text_IO;
with GNAT.OS_Lib;  use Ada.OS_Lib;

package Linux.Info.Disk is
   type Statfs is record
      File_Block_Size              : unsigned_long;
      File_Fragment_Size           : unsigned_long;
      File_Blocks                  : unsigned_long_long;
      File_Blocks_Free             : unsigned_long_long;
      File_Blocks_Available        : unsigned_long_long;
      File_INodes                  : unsigned_long_long;
      File_Free_Inodes             : unsigned_long_long;
      File_Free_Inodes_Unpriv      : unsigned_long_long;
      File_System_ID               : unsigned_long;
      File_Flag                    : unsigned_long;
      File_Maximum_Filename_Length : unsigned_long;
   end record;
   pragma Convention (C, Statfs);
   
   function Get_Statfs (path : char_array; buf : access Statfs) return Integer;
   pragma Import (C, Get_Statfs, "statvfs");

   function Get_Statfs_Information return Statfs;

end Linux.Info.Disk;
