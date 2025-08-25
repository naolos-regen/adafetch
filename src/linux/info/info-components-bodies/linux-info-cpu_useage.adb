package body Linux.Info.Cpu_Useage is 
<<<<<<< HEAD
 
   CPU_Useage_Path_Error : exception;
   
   procedure Split_Vector (V : in out Vector) is 
      Subs  : Slice_Set;
      Seps  : constant String := " " & Latin_1.HT;
   begin
      for I of V loop
         Create (Subs, To_String (I), Seps, Multiple);
         declare
            V2 : Vector := Empty_Vector;
         begin
            for J in 2 .. Slice_Count (Subs) loop
               declare
                  Sub : constant String := Slice (Subs, J);
               begin
                  Append (V2, Sub);
               end;
            end loop;
         end;
      end loop;
   end Split_Vector;
=======
   type Stat is 
              (EMPTY,
              CPUNAME, USER, NICE, SYSTEM, 
              IDLE, IOWAIT, IRQ, SOFTIRQ);
   
   function Get_Stat return CPU_Stats is
      File : File_Type;
      Line : Unbounded_String;
      Seps : constant String := " " & Latin_1.HT;
      S : CPU_Stats;
   begin
      Open (File, In_File, CPU_Useage_Path);
      
      Line := To_Unbounded_String (Get_Line (File));
      declare
         Str : constant String := To_String (Line);
      begin
         if Str'Length >= 3 and then Str (1 .. 3) = "cpu" then
            declare
               Slices : Slice_Set;
            begin
               Create (Slices, Str, Seps, Multiple);
               S.User     := Integer'Value (Slice (Slices, Stat'Pos(USER)));
               S.Nice     := Integer'Value (Slice (Slices, Stat'Pos(NICE)));
               S.System   := Integer'Value (Slice (Slices, Stat'Pos(SYSTEM)));
               S.Idle     := Integer'Value (Slice (Slices, Stat'Pos(IDLE)));
               S.Io_Wait  := Integer'Value (Slice (Slices, Stat'Pos(IOWAIT)));
               S.Irq      := Integer'Value (Slice (Slices, Stat'Pos(IRQ)));
               S.Soft_Irq := Integer'Value (Slice (Slices, Stat'Pos(SOFTIRQ)));
            end;
         end if;
      end;
      Close (File);
      return S;
   end Get_Stat;
   
   function Sum_Total (S : CPU_Stats) return Integer is
   begin
      return S.User + S.Nice + S.System 
              + S.Idle + S.Io_Wait + S.Irq + S.Soft_Irq;
   end Sum_Total;
   
   function Percentage (F : Float) return Percentage is 
   begin
      if F < 0.0 then 
         return 0;
      elsif F > 100.0 then
         return 100;
      else
         return Percentage (F);
      end if;
   end Percentage;
>>>>>>> refs/remotes/origin/master

   function Calculate_CPU_Useage (I_N, I_L, T_N, T_L : Integer)
   return Percentage is
   begin 
      return Percentage (100.0 * (1.0 - Float (I_N - I_L) / Float (T_N - T_L)));
   end Calculate_CPU_Useage;
   
   function Get_Cpu_Percentage return Percentage is
      Default_Cpu_Useage_Path : constant String := CPU_Useage_Path;
<<<<<<< HEAD
      File                    : File_Type;
      C_Vec                   : Vector := Empty_Vector;
=======
      CPU_Useage_Path_Error   : exception;
      Stat1, Stat2            : CPU_Stats;
      Idle_Last, Idle_Next    : Integer;
      Total_Last, Total_Next  : Integer;
>>>>>>> refs/remotes/origin/master
   begin
      if not Exists (Default_Cpu_Useage_Path) then
         raise CPU_Useage_Path_Error with "File Does Not Exist" & Default_CPU_Useage_Path;
      end if;
      
<<<<<<< HEAD
      Open (File, In_File, Default_Cpu_Useage_Path);
      Set_Line_Length (File, 7);

      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line(File);
         begin
            Append (C_Vec, To_Unbounded_String (Line));
         end;
      end loop;
      
      Split_Vector (C_Vec);

=======
      Stat1      := Get_Stat;
      Idle_Last  := Stat1.Idle + Stat1.IO_Wait;
      Total_Last := Sum_Total (Stat1);
      
      delay 1;

      Stat2      := Get_Stat;
      Idle_Next  := Stat2.Idle + Stat2.IO_Wait;
      Total_Next := Sum_Total (Stat2);

      return Calculate_Cpu_Percentage (Idle_Next, Idle_Last, Total_Next, Total_Last);
>>>>>>> refs/remotes/origin/master
   end Get_Cpu_Percentage;

end Linux.Info.Cpu_Useage;
