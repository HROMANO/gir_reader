package body Gir_Reader.Element_Lists is

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : List) is
   begin
      if Item.Is_Empty then
         Output.Put ("[]");
         return;
      end if;

      Output.Put ("[");
      Output.New_Line;
      Output.Increase_Indent (Amount => 3);

      for I in Item.First_Index .. Item.Last_Index loop
         Gir_Reader.Elements.Image (Output, Item (I));
         if I /= Item.Last_Index then
            Output.Put (",");
         end if;
      end loop;

      Output.Decrease_Indent (Amount => 3);
      Output.Put ("]");
   end Image;

end Gir_Reader.Element_Lists;
