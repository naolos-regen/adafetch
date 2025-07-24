with Ada.Directories; use Ada.Directories;

package body Linux.Info.Battery is 
   function Find_Path return String is
      Default_Battery_Path : constant String := Linux.Info.Battery_Path;
      Battery_Entry        : Directory_Entry_Type;
      Battery_Search       : Search_Type;
      Battery_Filter       : Filter_Type := (Directory => True, others => False);
   begin
      if not Exists (Default_Battery_Path) then
         return "not found";
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
            Name : constant String := Simple_Name (Battery_Entry);
         begin
            End_Search (Battery_Search);
            return Name;
         end;
      end loop;
      
   end Find_Path;

   function Get_Battery_Information return Battery_Pointer is
      Result_Battery_Path : constant String := Find_Path;
   begin
      if Battery_Path = "not found" then
         return null;
      end if;
      

      
   end Get_Battery_Information;


end Linux.Info.Battery;
