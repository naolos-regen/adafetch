package body Linux.Info.Battery is 
   package Battery_Vec is new Ada.Containers.Vectors (
      Index_Type => Integer,
      Element_Type => String);
   
   use type Battery_Vec.Cursor;
   
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
   
   function Parse_Lines (Vector : in out Vector) return Battery_Pointer is
      Battery_Map  : Empty_Map;
      Battery_Data : Battery := (
         Dev_Type           => To_Unbounded_String(""),
         Name               => To_Unbounded_String(""),
         Status             => To_Unbounded_String(""),
         Model_Name         => To_Unbounded_String(""),
         Manufacturer       => To_Unbounded_String(""),
         Technology         => To_Unbounded_String(""),
         Present            => False,
         Cycle_Count        => 0,
         Voltage_Min_Design => 0,
         Voltage_Now        => 0,
         Power_Now          => 0,
         Energy_Full_Design => 0,
         Energy_Full        => 0,
         Energy_Now         => 0,
         Capacity           => 0,
         Capacity_Level     => To_Unbounded_String("") 
      );
   begin
      for Line of Vector loop
         declare
            Equal_Pos : constant Natural := Index (Line, "=");
         begin
            if Equal_Pos = 0 then
               continue;
            end if;

            declare
               Key   : constant String := Line (Line'First .. Equal_Pos - 1);
               Value : constant String := Line (Equal_Pos + 1 .. Line'Last);
            begin
               if Key = "POWER_SUPPLY_NAME" then
                  Battery_Data.Name := To_Unbounded_String(Value);
               elsif Key = "DEVTYPE" then
                  Battery_Data.Dev_Type := To_Unbounded_String(Value);
               elsif Key = "POWER_SUPPLY_STATUS" then
                  Battery_Data.Status := To_Unbounded_String(Value);
               elsif Key = "POWER_SUPPLY_RESENT" then
                  if Value "1" then
                     Battery_Data.Present := True;
                  end if;
               elsif Key = "POWER_SUPPLY_TECHNOLOGY" then
                  Battery_Data.Technology := To_Unbounded_String(Value);
               elsif Key = "POWER_SUPPLY_CYCLE_COUNT" then
                  Battery_Data.Cycle_Count := Integer'Value (Value);
               elsif Key = "POWER_SUPPLY_VOLTAGE_NOW" then
                  Battery_Data.Voltage_Now := Integer'Value (Value);
               elsif Key = "POWER_SUPPLY_VOLTAGE_MIN_DESIGN" then
                  Battery_Data.Voltage_Min_Design := Integer'Value (Value);
               elsif Key = "POWER_SUPPLY_POWER_NOW" then
                  Battery_Data.Power_Now := Integer'Value (Value);
               elsif Key = "POWER_SUPPLY_ENERGY_FULL_DESIGN" then
                  Battery_Data.Energy_Full_Design := Integer'Value (Value);
               elsif Key = "POWER_SUPPLY_ENERGY_FULL" then 
                  Battery_Data.Energy_Full = Integer'Value (Value);
               elsif Key = "POWER_SUPPLY_ENERGY_NOW" then
                  Battery_Data.Energy_Now = Integer'Value (Value);
               elsif Key = "POWER_SUPPLY_CAPACITY" then
                  Battery_Data.Capacity = Percentage'Value (Value);
               elsif Key = "POWER_SUPPLY_CAPACITY_LEVEL" then
                  Battery_Data.Capacity_Level := To_Unbounded_String(Value);
               elsif Key = "POWER_SUPPLY_MANUFACTUTER" then 
                  Battery_Data.Manufacturer := To_Unbounded_String(Value);
               end if;
            end;
         end;
      end loop;

      return new Battery'(Battery_Data);
   end Parse_Lines;

   function Get_Battery_Information return Battery_Pointer is
      Result_Battery_Path : constant String := Find_Path & "/uevent";
      File                : File_Type;
      Battery_Result      : Battery_Pointer;
      Battery_Vector      : Empty_Vector;
   begin
      if Battery_Path = "not found" then
         raise Battery_Path_Error with "Not Found";
      end if;
      
      Open (File, In_File, Result_Battery_Path);

      while not End_Of_File loop
         declare
            Line : constant String := Get_Line;
         begin
            Append (Battery_Vector, Line);
         end;
      end loop;
      Close (File);
      
      return Parse_Lines (Battery_Vector);
   end Get_Battery_Information;

end Linux.Info.Battery;
