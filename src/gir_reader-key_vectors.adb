package body Gir_Reader.Key_Vectors is

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Key_Vector) is
   begin
      Output.Put ("[");

      for I in Item.First_Index .. Item.Last_Index loop
         Output.Put ("""");
         Gir_Reader.Key_Types.Image (Output, Item (I));
         Output.Put ("""");
         if I /= Item.Last_Index then
            Output.Put (", ");
         end if;
      end loop;

      Output.Put ("]");
   end Image;

end Gir_Reader.Key_Vectors;
