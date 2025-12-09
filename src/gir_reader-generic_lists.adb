package body Gir_Reader.Generic_Lists is

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Element_List) is
   begin
      Output.Put ("[");

      for I in Item.First_Index .. Item.Last_Index loop
         Output.Put ("""");
         Element_Image (Output, Item (I));
         Output.Put ("""");
         if I /= Item.Last_Index then
            Output.Put (", ");
         end if;
      end loop;

      Output.Put ("]");
   end Image;

end Gir_Reader.Generic_Lists;
