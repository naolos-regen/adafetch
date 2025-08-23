package body Linux.Info.Bios is
   package Vec is new Vectors (Natural, Unbounded_String);
   Bios_Path_Error : exception;

   use Vec;

   function Get_One_Line return String is
      One_Liner : Unbounded_String := To_Unbounded_String ("");
      File      : File_Type;
   begin
      Open (File, In_File, Bios_Path);

      One_Liner := To_Unbounded_String (Get_Line (File));

      Close (File);

      return To_String (One_Liner);
   end Get_One_Line;
   
   function Parse_Vector (Vec : in out Vector) return Bios_Information_Pointer is 
      type Modalias is
      (
         DMI, BVN, BVR, BD,  BR,  EFR, SVN, PN,
         PVR, RVN, RN,  RVR, CVN, CT,  CVR, SKU
      );
      Modalias_Strings : constant array (Modalias) of Integer :=
      (
         DMI => 4,
         BVN => 4,
         BVR => 4,
         BD  => 3,
         BR  => 3,
         SVN => 4,
         EFR => 4,
         PN  => 3,
         PVR => 4,
         RVN => 4,
         RN  => 3,
         RVR => 4,
         CVN => 4,
         CT  => 3,
         CVR => 4,
         SKU => 4
      );
   begin
      for M in Modalias'First .. Modalias'Last loop
         declare
            Modalias_Index  : constant Natural
                            := Modalias'Pos(M);
            Length_Modalias : constant Positive
                            := Modalias_Strings (Modalias (M));
            Vector_String   : constant Unbounded_String
                            := Vec (Modalias_Index);
            Stripped        : constant String
                            := Slice (
                                      Vector_String, 
                                      Length_Modalias, 
                                      Length (Vector_String)
                                      );
         begin
            Put_Line (Stripped);
         end;
      end loop;
   end Parse_Vector;

   function Empty_Unbounded_String return Unbounded_String is
   begin
      return To_Unbounded_String ("");
   end Empty_Unbounded_String;
   
   function Split_One_Line (Line : constant String) return Vector is
      Bios_Vector : Vector := Empty_Vector;
      Subs        : Slice_Set;
      Seps        : constant String := ":" & Latin_1.HT;
   begin
      Create (Subs, Line, Seps, Multiple);

      for I in 1 .. Slice_Count (Subs) loop
         declare
            Sub : constant Unbounded_String := To_Unbounded_String (Slice (Subs, I));
         begin
            Append (Bios_Vector, Sub);
         end;
      end loop;

      return Bios_Vector;
   end Split_One_Line;

   function Get_Bios_Information return Bios_Information_Pointer is 
      Default_Bios_Path : constant String := Bios_Path;
      File              : File_Type;
      Bios_Vector       : Vector          := Append_From_Path;
   begin 
      return Parse_Vector (Split_One_Line (Get_One_Line));
   end Get_Bios_Information;
end Linux.Info.Bios;
