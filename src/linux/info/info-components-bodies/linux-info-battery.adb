with Ada.Directories;                        use Ada.Directories;
with Ada.Text_IO;                            use Ada.Text_IO;
with Ada.Containers.Indefinite_Ordered_Maps; use Ada.Containers.Indefinite_Ordered_Maps;
package body Linux.Info.Battery is 
   -- TODO: EXAMPLE OF MY LENOVO LAPTOP, BECAUSE I DON'T HAVE IT ON PC :-)
   -- DEVTYPE=power_supply
   -- POWER_SUPPLY_NAME=BAT0
   -- POWER_SUPPLY_TYPE=Battery
   -- POWER_SUPPLY_STATUS=Discharging
   -- POWER_SUPPLY_PRESENT=1
   -- POWER_SUPPLY_TECHNOLOGY=Li-poly
   -- POWER_SUPPLY_CYCLE_COUNT=819
   -- POWER_SUPPLY_VOLTAGE_MIN_DESIGN=11550000
   -- POWER_SUPPLY_VOLTAGE_NOW=12263000
   -- POWER_SUPPLY_POWER_NOW=5996000
   -- POWER_SUPPLY_ENERGY_FULL_DESIGN=50500000
   -- POWER_SUPPLY_ENERGY_FULL=51480000
   -- POWER_SUPPLY_ENERGY_NOW=45600000
   -- POWER_SUPPLY_CAPACITY=88
   -- POWER_SUPPLY_CAPACITY_LEVEL=Normal
   -- POWER_SUPPLY_TYPE=Battery
   -- POWER_SUPPLY_MODEL_NAME=02DL007
   -- POWER_SUPPLY_MANUFACTURER=LGC
   -- POWER_SUPPLY_SERIAL_NUMBER= 1421

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
