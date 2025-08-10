package body Linux.Info.Bios is
   package Vec is new Vectors (Natural, Unbounded_String);
   Bios_Path_Error : exception;

   use Vec;

   type Modalias is
   (
      DMI, BVN, BVR, BD,  BR,  SVN, PN, PVR, RVN
      RN,  RVR, CVN, CT3, CVR, SKU
   );

   Modalias_Strings : constant array (Modalias) of String :=
   (
      DMI => "dmi",
      BVN => "bvn",
      BVR => "bvr",
      BD  => "bd",
      BR  => "br",
      SVN => "svn",
      PN  => "PN",
      PVR => "pvr",
      RVN => "rvn",
      RN  => "rn",
      RVR => "rvr",
      CT3 => "ct3",
      CVR => "cvr",
      SKU => "sku"
   )
   
   function One_Liner_To_Vector (Line : Unbounded_String) return Vector is
      B_Vec       : Vector := Empty_Vector;
      String_Line : constant String := To_String (Line);

   begin
      -- TODO: Split Strings according to their Seperator ':' = New_Line
      --
      return B_Vec;
   end One_Liner_To_Vector;

   function Empty_Unbounded_String return Unbounded_String is
   begin
      return To_Unbounded_String ("");
   end Empty_Unbounded_String;

   function Append_From_Path return Vector is 
      Default_Bios_Path : constnat String  := Bios_Path;
      File              : File_Type;
      One_Liner_Modalias: Unbounded_String := "";
   begin
      if not Exists (Default_Bios_Path) then
         raise Bios_Path_Error with "Path Does Not Exist" & Default_Bios_Path;
      end if;

      Open (File, In_File, Default_Bios_Path);

      while not End_Of_File (File) loop
         One_Liner_Modalias := To_Unbounded_String (Get_Line (File));
      end loop;
      
      Close (File);

      return One_Liner_To_Vector (One_Liner_Modalias);
   end Append_From_Path;

   function Parse_Vector (Vec : in out Vector) return Bios_Information_Pointer is
      Bios_Structure    : Bios                     := Empty_Bios;
      System_Structure  : System                   := Empty_System;
      Board_Structure   : Board                    := Empty_Board;
      Chassis_Structure : Chassis                  := Empty_Chassis;
      Product_Structure : Product                  := Empty_Product;
      Result            : Bios_Information_Pointer := new Bios_Info;
   begin
      for Value of Vec loop
         -- TODO: Replace
         declare
            Line        : constant String  := To_String (Value);
            Colon_Pos   : constant Natural := Index (Line, ":");
            Tag         : String := (if Colon_Pos > 0 
                                       then Line (Line'First .. Colon_Pos - 1) 
                                       else Line);
            Value       : String := (if Colon_Pos > 0
                                       then Line (Colon_Pos + 1 .. Line'Last) 
                                       else "");
         begin
            -- TODO: Finish :-)
            for M in Modalias loop
               if Tag = Modalias_Strings (M) then
                  case M
                     when BD  =>
                     when BR  =>
                     when BVN =>
                     when BVR =>
                     when SVN =>
                     when RN  =>
                     when RVN =>
                     when RVR =>
                     when CT3 =>
                     when CVR =>
                     when CVR =>
                     when PN  =>
                     when SKU =>
                     when PVR =>
                     when others => null;
                  end case;
               end if;
            end loop;
         end;
      end loop;

      Result.all.Bios_Info    := Bios_Structure;
      Result.all.System_Info  := System_Structure;
      Result.all.Board_Info   := Board_Structure;
      Result.all.Chassis_Info := Chassis_Structure;
      Result.all.Product_Info := Product_Structure;

      return Result;
   end Parse_Vector;
   function Get_Bios_Information return Bios_Information_Pointer is 
      Default_Bios_Path : constant String := Bios_Path;
      File              : File_Type;
      Bios_Vector       : Vector          := Append_From_Path;
   begin 
      return Parse_Vector (Bios_Vector);
   end Get_Bios_Information;
end Linux.Info.Bios;
