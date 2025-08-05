package Linux.Info.Cpu_Useage is
   CPU_Path : constant String := "/proc/cpuinfo";

   type Cpu_Stats is record
         User        : constant Integer;
         Nice        : constant Integer;
         System      : constant Integer;
         Idle        : constant Integer;
         Io_Wait     : constant Integer;
         Irq         : constant Integer;
         Soft_Irq    : constant Integer;
         Steal       : constant Integer;
         Guest       : constant Integer;
         Guest_Nice  : constant Integer;
   end record;
   
   function Get_Cpu_Percentage return Percentage;

end Linux.Info.Cpu_Useage;
