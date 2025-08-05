with Interfaces.C; use Interfaces.C;

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
