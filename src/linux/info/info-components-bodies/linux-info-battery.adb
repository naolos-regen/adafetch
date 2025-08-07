package body Linux.Info.Battery is 

   package Vec is new Vectors (Natural, Unbounded_String);
   
   use Vec;

   Battery_Path : constant String := "/sys/class/power_supply/";

   type Battery is record
      Dev_Type           : Unbounded_String;
      Name               : Unbounded_String;
      Status             : Unbounded_String;
      Model_Name         : Unbounded_String;
      Manufacturer       : Unbounded_String;
      Technology         : Unbounded_String;
      Present            : Boolean;
      Cycle_Count        : Integer;
      Voltage_Min_Design : Integer;
      Voltage_Now        : Integer;
      Power_Now          : Integer;
      Energy_Full_Design : Integer;
      Energy_Full        : Integer;
      Energy_Now         : Integer;
      Capacity           : Integer;
      Capacity_Level     : Unbounded_String;
   end record;

   type Battery_Pointer is access all Battery;
   
   Battery_Path_Error : exception;

   function Find_Path return String is
      Default_Result : constant String := "not found";
      Battery_Entry  : Directory_Entry_Type;
      Battery_Search : Search_Type;
      Battery_Filter : constant Filter_Type 
                     := (Directory => True, others => False);
   begin
      if not Exists (Battery_Path) then
         raise Battery_Path_Error with 
         "Directory doesn't exist" & Battery_Path;
      end if;

      Start_Search (
                     Battery_Search,
                     Directory => Battery_Path,
                     Pattern => "BAT*",
                     Filter => Battery_Filter);

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
   
   function Parse_Lines (Vec: in out Vector) return Battery_Pointer is
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
      for Line of Vec loop
         declare
            Equal_Pos : constant Natural := Index (Line, "=");
         begin
            if Equal_Pos /= 0 then
               declare
                  Key   : String := Slice (Line, 1, Equal_Pos - 1);
                  Value : String := Slice (Line, Equal_Pos + 1, Length (Line));
               begin
                  Put_Line (Key & " is " & Value);
               end;
            end if;
         end;
      end loop;
      return new Battery'(Battery_Data);
   end Parse_Lines;

   function Get_Battery_Information return Battery_Pointer is
      Result   : constant String := Find_Path & "/uevent";
      File     : File_Type;
      B_Result : Battery_Pointer;
      B_Vector : Vector;
   begin
      if Result = "not found" then
         raise Battery_Path_Error with Result;
      end if;

      Open (File, In_File, Result);

      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
         begin
            Append (B_Vector, To_Unbounded_String(Line));
         end;
      end loop;

      Close (File);

      return Parse_Lines (B_Vector);
   end Get_Battery_Information;

end Linux.Info.Battery;
