package Linux.Info.Operating_System is
   Operating_System_Path : constant String := "/etc/os-release";

   type Operating_System_Release is record
      Name                 : constant String;
      Pretty Name          : constant String;
      ID                   : constant String;
      Build_ID             : constant String;
      ANSI_Color           : constant String;
      Home_URL             : constant String; -- maybe create URL type (?)
      Documentation_URL    : constant String; -- maybe create URL type (?)
      Support_URL          : constant String; -- maybe create URL type (?)
      Bug_Report_URL       : constant String; -- maybe create URL type (?)
      Privacy_Policy_URL   : constant String; -- maybe create URL type (?)
      Logo                 : constant String;
   end record; 
      -- other Idea is to interface with C fopen, 
      -- but ada uses the same but more reliable and safe,
      -- via the use of lightweight exceptions

   type Operating_System_Release_Pointer is access all Operating_System_Release;

   function Get_Operating_System_Release_Information return Operating_System_Release_Pointer;
      
end Linux.Info.Operating_System;
