package body Gir_Reader.Generic_Lists is

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : T_List) is
   begin
      Output.Put ("[");

      for I in Item.First_Index .. Item.Last_Index loop
         Output.Put ("""");
         T_Image (Output, Item (I));
         Output.Put ("""");
         if I /= Item.Last_Index then
            Output.Put (", ");
         end if;
      end loop;

      Output.Put ("]");
   end Image;

   function To_String (Item : T_List) return Utf8 is
      (Item'Image);

   function Empty_List return T_List is
      List : T_List;
   begin
      return List;
   end Empty_List;

end Gir_Reader.Generic_Lists;
