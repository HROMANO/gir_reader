pragma Ada_2022;

with Ada.Containers.Indefinite_Ordered_Maps;

with Gir_Reader.Images;

package body Gir_Reader.Attribute_Maps is

   use type Gir_Reader.Key_Types.Key;
   use type Gir_Reader.Key_Types.Attribute_Key;

   --
   --  Boolean type
   --

   type Boolean_Data is new Holder_Content_Root with record
      Value : Boolean;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Boolean_Data) is
   begin
      Gir_Reader.Images.Image (Output, Item.Value);
   end Image;

   --
   --  Parameter_Direction type
   --

   type Parameter_Direction_Data is new Holder_Content_Root with record
      Value : Parameter_Direction;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Parameter_Direction_Data) is
   begin
      Image (Output, Item.Value);
   end Image;

   --
   --  Integer type
   --

   type Integer_Data is new Holder_Content_Root with record
      Value : Integer;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Integer_Data) is
   begin
      Gir_Reader.Images.Image (Output, Item.Value);
   end Image;

   --
   --  Lifetime_Scope type
   --

   type Lifetime_Scope_Data is new Holder_Content_Root with record
      Value : Lifetime_Scope;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Lifetime_Scope_Data) is
   begin
      Image (Output, Item.Value);
   end Image;

   --
   --  Ownership type
   --

   type Ownership_Data is new Holder_Content_Root with record
      Value : Ownership;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Ownership_Data) is
   begin
      Image (Output, Item.Value);
   end Image;

   --
   --  Signal_Emission type
   --

   type Signal_Emission_Data is new Holder_Content_Root with record
      Value : Signal_Emission;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Signal_Emission_Data) is
   begin
      Image (Output, Item.Value);
   end Image;

   --
   --  Text type
   --

   type Text_Data (Length : Natural) is new Holder_Content_Root with record
      Value : Utf8 (1 .. Length);
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Text_Data) is
   begin
      Gir_Reader.Images.Image (Output, Item.Value);
   end Image;

   --
   --  Internal_Attribute_Maps package
   --

   function Less_Than
     (Left, Right : Gir_Reader.Key_Types.Attribute_Key) return Boolean
   is (Gir_Reader.Key_Types.Less_Than (Left, Right));

   package Internal_Attribute_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Gir_Reader.Key_Types.Attribute_Key'Class,
        Element_Type => Holder_Content_Root'Class,
        "<"          => Less_Than);

   --
   --  Real_Attribute_Map type
   --

   type Real_Attribute_Map is
     new Internal_Attribute_Maps.Map
     and Holder_Content_Root
   with null record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Real_Attribute_Map)
   is
      use type Internal_Attribute_Maps.Cursor;
   begin
      if Item.Is_Empty then
         Output.Put ("()");
         return;
      end if;

      for Index in Item.Iterate loop
         declare
            Key  : Gir_Reader.Key_Types.Attribute_Key'Class renames
              Internal_Attribute_Maps.Key (Index);
            Data : Holder_Content_Root'Class renames Item.Element (Key);
         begin
            Gir_Reader.Key_Types.Image (Output, Key);
            Output.Put (": ");

            --  TODO: could this be avoided?
            if Key in Gir_Reader.Key_Types.Boolean_Key'Class then
               Image (Output, Boolean_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Parameter_Direction_Key'Class
            then
               Image (Output, Parameter_Direction_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Integer_Key'Class then
               Image (Output, Integer_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Lifetime_Scope_Key'Class then
               Image (Output, Lifetime_Scope_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Ownership_Key'Class then
               Image (Output, Ownership_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Signal_Emission_Key'Class then
               Image (Output, Signal_Emission_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Text_Key'Class then
               Image (Output, Text_Data (Data));

            end if;

            if Internal_Attribute_Maps.Next (Index)
              /= Internal_Attribute_Maps.No_Element
            then
               Output.Put (",");
               Output.New_Line;
            end if;
         end;
      end loop;

   end Image;

   -------------------------
   -- Empty_Attribute_Map --
   -------------------------

   function Empty_Attribute_Map return Attribute_Map is
      Empty : Attribute_Map;
   begin
      return Empty;
   end Empty_Attribute_Map;

   -----------
   -- Clear --
   -----------

   overriding
   procedure Clear (Self : in out Attribute_Map) is
   begin
      Holders.Holder (Self).Clear;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   overriding
   function Is_Empty (Self : Attribute_Map) return Boolean is
   begin
      if Holders.Holder (Self).Is_Empty then
         return True;
      end if;

      return Real_Attribute_Map (Self.Element).Is_Empty;
   end Is_Empty;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Attribute_Map; Item : Gir_Reader.Key_Types.Attribute_Key'Class)
      return Boolean is
   begin
      if Self.Is_Empty then
         return False;
      end if;

      return Real_Attribute_Map (Self.Element).Contains (Item);
   end Contains;

   ------------------------
   -- Get_Attribute_Keys --
   ------------------------

   function Get_Attribute_Keys
     (Self : Attribute_Map) return Gir_Reader.Key_Lists.Attribute_Key_List is
   begin
      return List : Gir_Reader.Key_Lists.Attribute_Key_List do
         for C in
           Real_Attribute_Map (Self.Constant_Reference.Element.all).Iterate
         loop
            List.Append
              (Gir_Reader.Key_Types.Attribute_Key
                 (Internal_Attribute_Maps.Key (C)));
         end loop;
      end return;
   end Get_Attribute_Keys;

   ------------------
   -- Internal_Get --
   ------------------

   function Internal_Get
     (Self : Attribute_Map; Item : Gir_Reader.Key_Types.Attribute_Key'Class)
      return Holder_Content_Root'Class
   is (Real_Attribute_Map (Self.Element) (Item))
   with Inline;

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Boolean_Key;
      Default : Boolean) return Boolean
   is (if Self.Contains (Item)
       then Boolean_Data (Internal_Get (Self, Item)).Value
       else Default);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Parameter_Direction_Key;
      Default : Parameter_Direction) return Parameter_Direction
   is (if Self.Contains (Item)
       then Parameter_Direction_Data (Internal_Get (Self, Item)).Value
       else Default);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Integer_Key;
      Default : Integer) return Integer
   is (if Self.Contains (Item)
       then Integer_Data (Internal_Get (Self, Item)).Value
       else Default);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Lifetime_Scope_Key;
      Default : Lifetime_Scope) return Lifetime_Scope
   is (if Self.Contains (Item)
       then Lifetime_Scope_Data (Internal_Get (Self, Item)).Value
       else Default);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Ownership_Key;
      Default : Ownership) return Ownership
   is (if Self.Contains (Item)
       then Ownership_Data (Internal_Get (Self, Item)).Value
       else Default);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Signal_Emission_Key;
      Default : Signal_Emission) return Signal_Emission
   is (if Self.Contains (Item)
       then Signal_Emission_Data (Internal_Get (Self, Item)).Value
       else Default);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Text_Key;
      Default : Utf8) return Utf8
   is (if Self.Contains (Item)
       then Text_Data (Internal_Get (Self, Item)).Value
       else Default);

   ------------------
   -- Internal_Set --
   ------------------

   procedure Internal_Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Attribute_Key'Class;
      Value : Holder_Content_Root'Class) is
   begin
      if Self.Is_Empty then

         declare
            R : Real_Attribute_Map;
         begin
            R.Insert (Item, Value);
            Self.Replace_Element (R);
         end;

      elsif Real_Attribute_Map (Self.Element).Contains (Item) then
         Real_Attribute_Map (Self.Reference.Element.all) (Item) := Value;
      else
         Real_Attribute_Map (Self.Reference.Element.all).Insert (Item, Value);
      end if;

   end Internal_Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Boolean_Key;
      Value : Boolean)
   is
      Value_Record : Boolean_Data := (Value => Value);
   begin
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Parameter_Direction_Key;
      Value : Parameter_Direction)
   is
      Value_Record : Parameter_Direction_Data := (Value => Value);
   begin
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Integer_Key;
      Value : Integer)
   is
      Value_Record : Integer_Data := (Value => Value);
   begin
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Lifetime_Scope_Key;
      Value : Lifetime_Scope)
   is
      Value_Record : Lifetime_Scope_Data := (Value => Value);
   begin
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Ownership_Key;
      Value : Ownership)
   is
      Value_Record : Ownership_Data := (Value => Value);
   begin
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Signal_Emission_Key;
      Value : Signal_Emission)
   is
      Value_Record : Signal_Emission_Data := (Value => Value);
   begin
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Text_Key;
      Value : Utf8)
   is
      Value_Record : Text_Data := (Length => Value'Length, Value => Value);
   begin
      Internal_Set (Self, Item, Value_Record);
   end Set;

   procedure Unset
     (Self : in out Attribute_Map;
      Item : Gir_Reader.Key_Types.Attribute_Key'Class) is
   begin
      Real_Attribute_Map (Self.Reference.Element.all).Delete (Item);
   end Unset;

   -----------
   -- Image --
   -----------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Attribute_Map) is
   begin
      if Item.Is_Empty then
         Output.Put ("()");
      else
         Image (Output, Real_Attribute_Map (Item.Element));
      end if;
   end Image;

end Gir_Reader.Attribute_Maps;
