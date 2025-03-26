with Ada.Containers.Vectors;

package body Gir_Reader.Element_Lists is

   use type Gir_Reader.Elements.Element;

   --  The 'List' type uses internally Ada.Containers.Vectors.
   package Element_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Gir_Reader.Elements.Element);

   subtype Vector is Element_Vectors.Vector;

   --  The real list hidden by the holder.
   type Real_Element_List is new Root with record
      Value : Vector;
   end record;

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

   function Is_Empty (Self : List) return Boolean is
   begin
      if Self.Internal.Is_Empty then
         return True;
      end if;

      return Real_Element_List (Self.Internal.Element).Value.Is_Empty;
   end Is_Empty;

   --------------
   --  Length  --
   --------------

   function Length (Self : List) return Natural is
   begin
      if Self.Internal.Is_Empty then
         return 0;
      end if;

      return Natural (Real_Element_List (Self.Internal.Element).Value.Length);
   end Length;

   -----------
   --  Get  --
   -----------

   function Get
     (Self : List; Index : Positive) return Gir_Reader.Elements.Element
   is (Real_Element_List (Self.Internal.Element).Value (Index));

   --------------------
   --  Last_Element  --
   --------------------

   function Last_Element (Self : List) return Gir_Reader.Elements.Element
   is (Self.Get (Self.Length));

   -------------------
   --  Delete_Last  --
   -------------------

   procedure Delete_Last (Self : in out List) is
      V : Vector := Real_Element_List (Self.Internal.Element).Value;
      R : Real_Element_List;
   begin
      V.Delete_Last;
      R.Value := V;
      Self.Internal.Replace_Element (R);
   end Delete_Last;

   -------------
   --  Clear  --
   -------------

   procedure Clear (Self : in out List) is
   begin
      if Self.Internal.Is_Empty then
         return;
      end if;

      Self.Internal.Clear;
   end Clear;

   --------------
   --  Append  --
   --------------

   procedure Append (Self : in out List; Item : Gir_Reader.Elements.Element) is
   begin

      if Self.Internal.Is_Empty then

         declare
            R : Real_Element_List;
         begin
            R.Value.Append (Item);
            Self.Internal.Replace_Element (R);
         end;

      else
         Real_Element_List (Self.Internal.Reference.Element.all).Value.Append
           (Item);
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
