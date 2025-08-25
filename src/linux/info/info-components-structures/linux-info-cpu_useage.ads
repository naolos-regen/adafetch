<<<<<<< HEAD
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Directories;        use Ada.Directories;
with Ada.Containers;         use Ada.Containers;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters;

=======
with Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.String_Split; use GNAT.String_Split;
with Constants; use Constants;
 
>>>>>>> refs/remotes/origin/master
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
<<<<<<< HEAD
         Steal       : Integer;
         Guest       : Integer;
         Guest_Nice  : Integer;
=======
>>>>>>> refs/remotes/origin/master
   end record;
   
   function Get_Cpu_Percentage return Percentage;

end Linux.Info.Cpu_Useage;
