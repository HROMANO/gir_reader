pragma Ada_2022;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with Gir_Reader.Attribute_Maps;
with Gir_Reader.Element_Lists;
with Gir_Reader.Images;

package body Gir_Reader.Elements is

   use type Gir_Reader.Key_Types.Key;

   --
   --  Vector_Data type
   --

   type Vector_Data is new Holder_Content_Root with record
      Value : Gir_Reader.Element_Lists.List;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Vector_Data) is
   begin
      Gir_Reader.Element_Lists.Image (Output, Item.Value);
   end Image;

   --
   --  Attribute type
   --

   type Attributes_Data is new Holder_Content_Root with record
      Value : Gir_Reader.Attribute_Maps.Attribute_Map;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Attributes_Data) is
   begin
      Gir_Reader.Attribute_Maps.Image (Output, Item.Value);
   end Image;

   --
   --  Real_Element type
   --

   package Real_Elements is

      function Less_Than
        (Left, Right : Gir_Reader.Key_Types.Element_Key'Class) return Boolean
      is (Gir_Reader.Key_Types.Less_Than
            (Gir_Reader.Key_Types.Key (Left),
             Gir_Reader.Key_Types.Key (Right)));

      package Element_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps
          (Key_Type     => Gir_Reader.Key_Types.Element_Key'Class,
           Element_Type => Holder_Content_Root'Class,
           "<"          => Less_Than);

      type Real_Element is new Holder_Content_Root with record
         Sub_Elements : Element_Maps.Map := Element_Maps.Empty_Map;
         Attributes   : Gir_Reader.Attribute_Maps.Attribute_Map :=
           Gir_Reader.Attribute_Maps.Empty_Attribute_Map;
         Content      : Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Null_Unbounded_String;
      end record;

      function Is_Empty (Item : Real_Element) return Boolean;

      procedure Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Item   : Real_Element);

   end Real_Elements;

   package body Real_Elements is

      use type Ada.Strings.Unbounded.Unbounded_String;

      function Is_Empty (Item : Real_Element) return Boolean is
      begin
         return
           Item.Sub_Elements.Is_Empty
           and then Item.Attributes.Is_Empty
           and then Item.Content = Ada.Strings.Unbounded.Null_Unbounded_String;
      end Is_Empty;

      procedure Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Item   : Real_Element)
      is
         use type Element_Maps.Cursor;
      begin
         if Item.Is_Empty then
            Output.Put ("()");
            return;
         end if;

         Output.Put ("(");
         Output.New_Line;
         Output.Increase_Indent (3);

         if not Item.Attributes.Is_Empty then
            Output.Put ("Attributes:");
            Output.New_Line;
            Output.Increase_Indent (3);
            Gir_Reader.Attribute_Maps.Image (Output, Item.Attributes);
            Output.Decrease_Indent (3);
            Output.New_Line;
         end if;

         if Item.Content /= Ada.Strings.Unbounded.Null_Unbounded_String then
            Output.Put ("Content:");
            Output.New_Line;
            Output.Increase_Indent (3);
            Gir_Reader.Images.Image
              (Output, Ada.Strings.Unbounded.To_String (Item.Content));
            Output.Decrease_Indent (3);
            Output.New_Line;
         end if;

         if Item.Sub_Elements.Is_Empty then
            Output.Decrease_Indent (3);
            return;
         end if;

         Output.Put ("Sub elements:");
         Output.New_Line;
         Output.Increase_Indent (3);

         for Index in Item.Sub_Elements.Iterate loop
            declare
               Key  : Gir_Reader.Key_Types.Element_Key'Class renames
                 Element_Maps.Key (Index);
               Data : Holder_Content_Root'Class renames
                 Item.Sub_Elements.Element (Key);
            begin
               Gir_Reader.Key_Types.Image (Output, Key);
               Output.Put (": ");

               Image (Output, Vector_Data (Data));

               if Element_Maps.Next (Index) /= Element_Maps.No_Element then
                  Output.Put (",");
                  Output.New_Line;
               end if;
            end;
         end loop;

         Output.Decrease_Indent (6);
         Output.New_Line;
         Output.Put (")");

      end Image;

   end Real_Elements;

   subtype Real_Element is Real_Elements.Real_Element;

   -------------------
   -- Empty_Element --
   -------------------

   function Empty_Element return Element is
      Empty : Element;
   begin
      return Empty;
   end Empty_Element;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (Self : in out Element) is
   begin
      Holders.Holder (Self).Clear;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   overriding
   function Is_Empty (Self : Element) return Boolean is
   begin
      if Holders.Holder (Self).Is_Empty then
         return True;
      end if;

      return Real_Element (Self.Element).Is_Empty;
   end Is_Empty;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Element; Item : Gir_Reader.Key_Types.Element_Key'Class)
      return Boolean is
   begin
      if Self.Is_Empty then
         return False;
      end if;

      return Real_Element (Self.Element).Sub_Elements.Contains (Item);
   end Contains;

   ------------------------------
   -- Get_Sub_Element_Key_List --
   ------------------------------

   function Get_Sub_Element_Key_List
     (Self : Element) return Gir_Reader.Key_Lists.Element_Key_List
   is
      Result : Gir_Reader.Key_Lists.Element_Key_List;
   begin
      if Self.Is_Empty then
         return Result;
      end if;

      declare
         Map : Real_Element renames Real_Element (Self.Element);
      begin
         for Iterator in Map.Sub_Elements.Iterate loop
            declare
               K : Gir_Reader.Key_Types.Element_Key'Class renames
                 Real_Elements.Element_Maps.Key (Iterator);
            begin
               Result.Append (Gir_Reader.Key_Types.Element_Key (K));
            end;
         end loop;
      end;

      return Result;
   end Get_Sub_Element_Key_List;

   ------------------
   -- Internal_Get --
   ------------------

   function Internal_Get
     (Self : Element; Item : Gir_Reader.Key_Types.Element_Key'Class)
      return Holder_Content_Root'Class
   is (Real_Element (Self.Element).Sub_Elements (Item))
   with Inline;

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Element_Key;
      Default : Gir_Reader.Element_Lists.List)
      return Gir_Reader.Element_Lists.List
   is (if Self.Contains (Item)
       then Vector_Data (Internal_Get (Self, Item)).Value
       else Default);

   ------------------
   -- Internal_Set --
   ------------------

   procedure Internal_Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Element_Key'Class;
      Value : Holder_Content_Root'Class) is
   begin
      if Self.Is_Empty then

         declare
            R : Real_Element;
         begin
            R.Sub_Elements.Insert (Item, Value);
            Self.Replace_Element (R);
         end;

      elsif Real_Element (Self.Element).Sub_Elements.Contains (Item) then
         Real_Element (Self.Reference.Element.all).Sub_Elements (Item) :=
           Value;
      else
         Real_Element (Self.Reference.Element.all).Sub_Elements.Insert
           (Item, Value);
      end if;

   end Internal_Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Element_Key;
      Value : Gir_Reader.Element_Lists.List)
   is
      Value_Record : Vector_Data;
   begin
      Value_Record.Value := Value;
      Internal_Set (Self, Item, Value_Record);
   end Set;

   procedure Set
     (Self : in out Element; Value : Gir_Reader.Attribute_Maps.Attribute_Map)
   is
   begin

      if Self.Is_Empty then

         declare
            R : Real_Element;
         begin
            R.Attributes := Value;
            Self.Replace_Element (R);
         end;

      else
         Real_Element (Self.Reference.Element.all).Attributes := Value;
      end if;

   end Set;

   procedure Set_Content
     (Self : in out Element; Value : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Self.Is_Empty then

         declare
            R : Real_Element;
         begin
            R.Content := Value;
            Self.Replace_Element (R);
         end;

      else
         Real_Element (Self.Reference.Element.all).Content := Value;
      end if;

   end Set_Content;
   ------------
   -- Append --
   ------------

   procedure Append
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Element_Key;
      Value : Element)
   is
      List : Gir_Reader.Element_Lists.List;
   begin
      if Self.Is_Empty
        or else
          not Real_Element (Self.Element).Sub_Elements.Contains
                (Gir_Reader.Key_Types.Element_Key (Item))
      then
         List.Append (Value);
         Self.Set (Item, List);
      else
         Gir_Reader.Element_Lists.List
           (Vector_Data
              (Real_Element (Self.Reference.Element.all).Sub_Elements.Reference
                 (Item)
                 .Element.all)
              .Value)
           .Append (Value);
      end if;
   end Append;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
     (Self : Element) return Gir_Reader.Attribute_Maps.Attribute_Map is
   begin
      if Self.Is_Empty then
         return Gir_Reader.Attribute_Maps.Empty_Attribute_Map;
      else
         return Real_Element (Self.Element).Attributes;
      end if;
   end Get_Attributes;

   function Get_Content (Self : Element) return Utf8 is
   begin
      if Self.Is_Empty then
         return "";
      else
         return
           Ada.Strings.Unbounded.To_String
             (Real_Element (Self.Element).Content);
      end if;
   end Get_Content;

   -----------
   -- Image --
   -----------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Element) is
   begin
      if Item.Is_Empty then
         Output.Put ("()");
      else
         Real_Elements.Image (Output, Real_Element (Item.Element));
      end if;
   end Image;

   function "/"
     (Left : Element; Right : Gir_Reader.Key_Types.Element_Key)
      return Gir_Reader.Element_Lists.List
   is (Left.Get_Or_Else (Right, Gir_Reader.Element_Lists.Empty_List));

   function "/"
     (Left : Element; Right : Gir_Reader.Key_Types.Element_Key) return Element
   is
      use type Gir_Reader.Element_Lists.List;
   begin
      return Left / Right / 1;
   end "/";

end Gir_Reader.Elements;
