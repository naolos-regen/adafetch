with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Directories;        use Ada.Directories;
with Ada.Containers;         use Ada.Containers;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters;

package Linux.Info.Cpu_Useage is
   CPU_Path : constant String := "/proc/stat";

   package Vec is new Vector (Natural, Unbounded_String);
   use Vec;

   type Cpu_Stats is record
         User        : Integer;
         Nice        : Integer;
         System      : Integer;
         Idle        : Integer;
         Io_Wait     : Integer;
         Irq         : Integer;
         Soft_Irq    : Integer;
         Steal       : Integer;
         Guest       : Integer;
         Guest_Nice  : Integer;
   end record;
   
   function Get_Cpu_Percentage return Percentage;

end Linux.Info.Cpu_Useage;
