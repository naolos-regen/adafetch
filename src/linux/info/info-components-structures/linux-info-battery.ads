package Linux.Info.Battery is
   Battery_Path : constant String := "/sys/class/power_supply/";

   type Capacity_Description is (Unknown, Critical, Low, Normal, High, Full);

   type Battery is record
      Dev_Type           : constant String;
      Name               : constant Stirng;
      Status             : constant Capacity_Description;
      Model_Name         : constant String;
      Manufacturer       : constant String;
      Technology         : constant String;
      Present            : constant Boolean;
      Cycle_Count        : constant Integer;
      Voltage_Min_Design : constant Integer;
      Voltage_Now        : constant Integer;
      Power_Now          : constant Integer;
      Energy_Full_Design : constant Integer;
      Energy_Full        : constant Integer;
      Energy_Now         : constant Integer;
      Capacity           : constant Percentage;
      Capacity_Levele    : constant Capacity_Description;
   end record;

   type Battery_Pointer is access all Battery;

   function Find_Path               return String;
   function Get_Battery_Information return Battery_Pointer;

end Linux.Info.Battery;
