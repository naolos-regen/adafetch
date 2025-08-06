with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

package body Linux.Info.Battery is 
   
   Battery_Path_Error : exception;
   
   function Find_Path return String is
      Default_Battery_Path : constant String := Linux.Info.Battery_Path;
      Default_Result       : constant String := "not fonud";
      Battery_Entry        : Directory_Entry_Type;
      Battery_Search       : Search_Type;
      Battery_Filter       : Filter_Type := (Directory => True, others => False);
   begin
      if not Exists (Default_Battery_Path) then
         raise Battery_Path_Error with "Directory Does Not Exist" & Default_Battery_Path;
      end if;

      Start_Search (
                    Battery_Search, 
                    Directory => Default_Battery_Path,
                    Pattern => "BAT*",
                    Filter  => Battery_Filter
                    );

      while More_Entries (Battery_Search) loop
         Get_Next_Entry (Battery_Search, Battery_Entry);
         declare
            Name : constant String := Full_Name (Battery_Entry);
         begin
            End_Search (Battery_Search);
            return Name;
         end;
      end loop;
      return Default_Result;
   end Find_Path;

   function Get_Battery_Information return Battery_Pointer is
      Result_Battery_Path : constant String := Find_Path & "/uevent";
      File                : File_Type;
      Battery_Result      : Battery_Pointer;
   begin
      if Battery_Path = "not found" then
         raise Battery_Path_Error with "Not Found";
      end if;
      
      Open (File, In_File, Result_Battery_Path);

      while not End_Of_File loop
         declare
            Line : constant String := Get_Line;
         begin
            Put_Line(Line);
         end;
      end loop;
      Close (File);
   end Get_Battery_Information;


end Linux.Info.Battery;
