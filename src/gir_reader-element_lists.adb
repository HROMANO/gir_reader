with Ada.Containers.Vectors;

package body Gir_Reader.Element_Lists is

   use type Gir_Reader.Elements.Element;

   --  The 'List' type uses internally Ada.Containers.Vectors.
   package Element_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Gir_Reader.Elements.Element);

   --  The real list hidden by the holder.
   type Real_Element_List is new Element_Vectors.Vector and Holder_Content_Root
   with null record;

   ----------------
   -- Empty_List --
   ----------------

   function Empty_List return List is
      Empty : List;
   begin
      return Empty;
   end Empty_List;

   ----------------
   --  Is_Empty  --
   ----------------

   overriding
   function Is_Empty (Self : List) return Boolean is
   begin
      if Holders.Holder (Self).Is_Empty then
         return True;
      end if;

      return Real_Element_List (Self.Element).Is_Empty;
   end Is_Empty;

   --------------
   --  Length  --
   --------------

   function Length (Self : List) return Natural is
   begin
      if Self.Is_Empty then
         return 0;
      end if;

      return Natural (Real_Element_List (Self.Element).Length);
   end Length;

   -----------
   --  Get  --
   -----------

   function Get
     (Self : List; Index : Positive) return Gir_Reader.Elements.Element
   is (Real_Element_List (Self.Element) (Index));

   --------------------
   --  Last_Element  --
   --------------------

   function Last_Element (Self : List) return Gir_Reader.Elements.Element
   is (Self.Get (Self.Length));

   -------------------
   --  Delete_Last  --
   -------------------

   procedure Delete_Last (Self : in out List) is
   begin
      Real_Element_List (Self.Reference.Element.all).Delete_Last;
   end Delete_Last;

   -------------
   --  Clear  --
   -------------

   overriding
   procedure Clear (Self : in out List) is
   begin
      Holders.Holder (Self).Clear;
   end Clear;

   --------------
   --  Append  --
   --------------

   procedure Append (Self : in out List; Item : Gir_Reader.Elements.Element) is
   begin

      if Self.Is_Empty then

         declare
            R : Real_Element_List;
         begin
            R.Append (Item);
            Self.Replace_Element (R);
         end;

      else
         Real_Element_List (Self.Reference.Element.all).Append (Item);
      end if;

   end Append;

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

      for I in 1 .. Item.Length loop
         Gir_Reader.Elements.Image (Output, Item.Get (I));
         if I < Item.Length then
            Output.Put (",");
         end if;
      end loop;

      Output.Decrease_Indent (Amount => 3);
      Output.Put ("]");
   end Image;

end Gir_Reader.Element_Lists;
