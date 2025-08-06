with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Directories;              use Ada.Directories;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Containers.Ordered_Maps;  use Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;       use Ada.Containers.Vectors;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;

package Linux.Info.Battery is
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
      Capacity           : Percentage;
      Capacity_Level     : Capacity_Description;
   end record;

   type Battery_Pointer is access all Battery;

   function Find_Path               return String;
   function Get_Battery_Information return Battery_Pointer;

end Linux.Info.Battery;
