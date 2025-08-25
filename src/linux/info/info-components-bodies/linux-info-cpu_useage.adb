package body Linux.Info.Cpu_Useage is 
 
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

   function Get_Cpu_Percentage return Percentage is
      Default_Cpu_Useage_Path : constant String := CPU_Useage_Path;
      File                    : File_Type;
      C_Vec                   : Vector := Empty_Vector;
   begin
      if not Exists (Default_Cpu_Useage_Path) then
         raise CPU_Useage_Path_Error with "File Does Not Exist" & Default_CPU_Useage_Path;
      end if;
      
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

   end Get_Cpu_Percentage;

end Linux.Info.Cpu_Useage;
