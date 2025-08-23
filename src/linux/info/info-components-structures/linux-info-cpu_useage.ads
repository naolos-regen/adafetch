with Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.String_Split; use GNAT.String_Split;
with Constants; use Constants;
 
package Linux.Info.Cpu_Useage is
   CPU_Path : constant String := "/proc/cpuinfo";

   type Cpu_Stats is record
         User        : Integer;
         Nice        : Integer;
         System      : Integer;
         Idle        : Integer;
         Io_Wait     : Integer;
         Irq         : Integer;
         Soft_Irq    : Integer;
   end record;
   
   function Get_Cpu_Percentage return Percentage;

end Linux.Info.Cpu_Useage;
