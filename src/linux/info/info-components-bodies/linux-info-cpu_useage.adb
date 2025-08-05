with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

package body Linux.Info.Cpu_Useage is 
 
   CPU_Useage_Path_Error : exception;

   function Get_Cpu_Percentage return Percentage is
      Default_Cpu_Useage_Path : constant String := CPU_Useage_Path;
      File : File_Type;
   begin
      if not Exists (Default_Cpu_Useage_Path) then
         raise CPU_Useage_Path_Error with "File Does Not Exist" & Default_CPU_Useage_Path;
      end if;
      
      Open (File, In_File, Default_Cpu_Useage_Path);
      
      while End_Of_File (File) loop
         declare
            Line : constant String := Get_Line(File);
         begin
            -- TODO: Schemantics here
         end;
      end loop;
      
   end Get_Cpu_Percentage;

end Linux.Info.Cpu_Useage;
