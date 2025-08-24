package body Linux.Info.Graphics_Card is

   function Find_Path return String is 
      GPU_Path_Error : exception;
      GPU_Search : Search_Type;
      GPU_Filter : constant Filter_Type := (Directory => True, others => False);
      GPU_Entry  : Directory_Entry_Type;
      GPU_Path   : constant String      := Alternative_Card_Information;
   begin
      if not Exists (GPU_Path) then
         raise GPU_Path_Error with "Directory not found : " & GPU_Path;
      end if;

      Start_Search (GPU_Search, GPU_Path, "fb*", GPU_Filter);

      while More_Entries (GPU_Search) loop
         Get_Next_Entry (GPU_Search, GPU_Entry);
         declare
            Name : constant String := Full_Name (GPU_Entry);
         begin
            Start_Search (GPU_Search, Name, "device", GPU_Filter);

            while More_Entries (GPU_Search) loop
               Get_Next_Entry (GPU_Search, GPU_Entry);
               declare
                  Path : constant String := Full_Name (GPU_Entry);
               begin
                  return Path;
               end;
            end loop;
         end;
      end loop;

      raise GPU_Path_Error with "Directory not found : " & GPU_Path;
   end Find_Path;

   function Get_Graphics_Card_Information return Graphics_Card_Information_Pointer is
   begin
      return null
   end Get_Graphics_Card_Information;
end Linux.Info.Graphics_Card;
