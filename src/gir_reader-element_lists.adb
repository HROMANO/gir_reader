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

   -----------------
   --  To_String  --
   -----------------

   function To_String (Item : List) return Utf8 is
     (Item'Image);

   ------------------
   --  Empty_List  --
   ------------------

   function Empty_List return List is
      Empty : List;
   begin
      return Empty;
   end Empty_List;

   function "/"
     (Left : List; Right : Positive)
      return Gir_Reader.Elements.Element is
      use type Ada.Containers.Count_Type;
   begin
      if Ada.Containers.Count_Type (Right) <= Left.Length then
         return Left (Right);
      else
         return Gir_Reader.Elements.Empty_Element;
      end if;
   end "/";

end Gir_Reader.Element_Lists;
