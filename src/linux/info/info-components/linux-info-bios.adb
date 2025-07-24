with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

package body Linux.Info.Bios is
   function Get_Bios_Information return Bios_Information_Pointer is 
      Default_Bios_Path : constant String := Bios_Path;
      File              : File_Type;
   begin
      if not Exists (Default_Bios_Path) then
         return null;
      end if;
      
      Open (File, In_File, Default_Bios_Path + "uevent");
      
      while not End_Of_File loop
         declare
            Line : constant String := Get_Line();
         begin
            -- TODO: Do schemantics here
         end;
      end loop;

      Close (File);

      return null;
   end Get_Bios_Information;
end Linux.Info.Bios;
