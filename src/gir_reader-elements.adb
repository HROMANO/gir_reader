pragma Ada_2022;

with Ada.Containers.Indefinite_Ordered_Maps;

with Gir_Reader.Element_Lists;
with Gir_Reader.Images;

package body Gir_Reader.Elements is

   use type Gir_Reader.Keys.Key;

   --
   --  Element_Map type
   --

   package Element_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Gir_Reader.Keys.Key'Class,
        Element_Type => Root'Class,
        "<"          => Gir_Reader.Keys.Less_Than);

   subtype Element_Map is Element_Maps.Map;

   --
   --  Real_Element type
   --

   type Real_Element is new Root with record
      Value : Element_Map;
   end record;

   --
   --  Vector_Datas type
   --

   type Vector_Data is new Root with record
      Value : Gir_Reader.Element_Lists.List;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Vector_Data) is
   begin
      Gir_Reader.Element_Lists.Image (Output, Item.Value);
   end Image;

   --
   --  Boolean type
   --

   type Boolean_Data is new Root with record
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

   type Parameter_Direction_Data is new Root with record
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

   type Integer_Data is new Root with record
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

   type Lifetime_Scope_Data is new Root with record
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

   type Ownership_Data is new Root with record
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

   type Signal_Emission_Data is new Root with record
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

   type Text_Data is new Root with record
      Value : Text;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Text_Data) is
   begin
      Gir_Reader.Images.Image (Output, Item.Value);
   end Image;

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

   procedure Clear (Self : in out Element) is
   begin
      Self.Internal.Clear;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Element) return Boolean is
   begin
      if Self.Internal.Is_Empty then
         return True;
      end if;

      return Real_Element (Self.Internal.Element).Value.Is_Empty;
   end Is_Empty;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Element; Item : Gir_Reader.Keys.Key'Class) return Boolean is
   begin
      if Self.Is_Empty then
         return False;
      end if;

      declare
         Map : constant Element_Map :=
           Real_Element (Self.Internal.Element).Value;
      begin
         return Map.Contains (Item);
      end;
   end Contains;

   ------------------------------
   -- Get_Sub_Element_Key_List --
   ------------------------------

   function Get_Sub_Element_Key_List
     (Self : Element) return Gir_Reader.Keys.Vectors.Key_Vector
   is
      Result : Gir_Reader.Keys.Vectors.Key_Vector;
   begin
      if Self.Internal.Is_Empty then
         return Result;
      end if;

      declare
         M : constant Element_Map :=
           Real_Element (Self.Internal.Element).Value;
      begin
         for Iterator in M.Iterate loop
            declare
               K : constant Gir_Reader.Keys.Key'Class :=
                 Element_Maps.Key (Iterator);
            begin
               if K in Gir_Reader.Keys.Element_Key then
                  Result.Append (Gir_Reader.Keys.Element_Key (K));
               end if;
            end;
         end loop;
         return Result;
      end;
   end Get_Sub_Element_Key_List;

   ------------------
   -- Internal_Get --
   ------------------

   function Internal_Get
     (Self : Element; Item : Gir_Reader.Keys.Key'Class) return Root'Class
   is (Real_Element (Self.Internal.Element).Value.Element (Item));

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Keys.Boolean_Key) return Boolean
   is (Boolean_Data (Internal_Get (Self, Item)).Value);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Keys.Parameter_Direction_Key)
      return Parameter_Direction
   is (Parameter_Direction_Data (Internal_Get (Self, Item)).Value);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Keys.Integer_Key) return Integer
   is (Integer_Data (Internal_Get (Self, Item)).Value);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Keys.Lifetime_Scope_Key)
      return Lifetime_Scope
   is (Lifetime_Scope_Data (Internal_Get (Self, Item)).Value);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Keys.Ownership_Key) return Ownership
   is (Ownership_Data (Internal_Get (Self, Item)).Value);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Keys.Signal_Emission_Key)
      return Signal_Emission
   is (Signal_Emission_Data (Internal_Get (Self, Item)).Value);

   ---------
   -- Get --
   ---------

   function Get (Self : Element; Item : Gir_Reader.Keys.Text_Key) return Text
   is (Text_Data (Internal_Get (Self, Item)).Value);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Keys.Element_Key)
      return Gir_Reader.Element_Lists.List
   is (Vector_Data (Internal_Get (Self, Item)).Value);

   ------------------
   -- Internal_Set --
   ------------------

   procedure Internal_Set
     (Self  : in out Element;
      Item  : Gir_Reader.Keys.Key'Class;
      Value : Root'Class) is
   begin
      if Self.Internal.Is_Empty then

         declare
            R : Real_Element;
         begin
            R.Value.Insert (Item, Value);
            Self.Internal.Replace_Element (R);
         end;

      elsif Real_Element (Self.Internal.Element).Value.Contains (Item) then
         Real_Element (Self.Internal.Reference.Element.all).Value (Item) :=
           Value;
      else
         Real_Element (Self.Internal.Reference.Element.all).Value.Insert
           (Item, Value);
      end if;

   end Internal_Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Keys.Boolean_Key;
      Value : Boolean)
   is
      Value_Record : Boolean_Data;
   begin
      Value_Record.Value := Value;
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Keys.Parameter_Direction_Key;
      Value : Parameter_Direction)
   is
      Value_Record : Parameter_Direction_Data;
   begin
      Value_Record.Value := Value;
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Keys.Integer_Key;
      Value : Integer)
   is
      Value_Record : Integer_Data;
   begin
      Value_Record.Value := Value;
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Keys.Lifetime_Scope_Key;
      Value : Lifetime_Scope)
   is
      Value_Record : Lifetime_Scope_Data;
   begin
      Value_Record.Value := Value;
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Keys.Ownership_Key;
      Value : Ownership)
   is
      Value_Record : Ownership_Data;
   begin
      Value_Record.Value := Value;
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Keys.Signal_Emission_Key;
      Value : Signal_Emission)
   is
      Value_Record : Signal_Emission_Data;
   begin
      Value_Record.Value := Value;
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self : in out Element; Item : Gir_Reader.Keys.Text_Key; Value : Text)
   is
      Value_Record : Text_Data;
   begin
      Value_Record.Value := Value;
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Keys.Element_Key;
      Value : Gir_Reader.Element_Lists.List)
   is
      Value_Record : Vector_Data;
   begin
      Value_Record.Value := Value;
      Internal_Set (Self, Item, Value_Record);
   end Set;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self  : in out Element;
      Item  : Gir_Reader.Keys.Element_Key;
      Value : Element)
   is
      V : Gir_Reader.Element_Lists.List;
   begin
      if Self.Internal.Is_Empty
        or else not Real_Element (Self.Internal.Element).Value.Contains
                      (Gir_Reader.Keys.Key (Item))
      then
         V.Append (Value);
         Self.Set (Item, V);
      else
         --  TODO: avoid copying (doesn't work below). Element_Lists needs
         --  some reference access?
         --  Vector_Data (Real_Element (Self.Internal.Reference.Element.all)
         --  .Value (Item)).Value.Append (Value);
         V := Self.Get (Item);
         V.Append (Value);
         Self.Set (Item, V);
      end if;
   end Append;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes (Self : Element) return Element is
   begin
      return Result : Element do
         if not Self.Internal.Is_Empty then

            declare
               Map : constant Element_Map :=
                 Real_Element (Self.Internal.Element).Value;
            begin
               for Iterator in Map.Iterate loop

                  declare
                     K : Gir_Reader.Keys.Key'Class :=
                       Element_Maps.Key (Iterator);
                  begin
                     if K not in Gir_Reader.Keys.Element_Key then
                        Internal_Set (Result, K, Internal_Get (Self, K));
                     end if;
                  end;

               end loop;
            end;

         end if;
      end return;
   end Get_Attributes;

   -----------
   -- Image --
   -----------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Element_Map)
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

      for Index in Item.Iterate loop
         declare
            Key  : constant Gir_Reader.Keys.Key'Class :=
              Element_Maps.Key (Index);
            Data : constant Root'Class := Item.Element (Key);
         begin
            Gir_Reader.Keys.Image (Output, Key);
            Output.Put (": ");

            --  TODO: could this be avoided?
            if Key in Gir_Reader.Keys.Boolean_Key then
               Image (Output, Boolean_Data (Data));

            elsif Key in Gir_Reader.Keys.Parameter_Direction_Key then
               Image (Output, Parameter_Direction_Data (Data));

            elsif Key in Gir_Reader.Keys.Integer_Key then
               Image (Output, Integer_Data (Data));

            elsif Key in Gir_Reader.Keys.Lifetime_Scope_Key then
               Image (Output, Lifetime_Scope_Data (Data));

            elsif Key in Gir_Reader.Keys.Ownership_Key then
               Image (Output, Ownership_Data (Data));

            elsif Key in Gir_Reader.Keys.Signal_Emission_Key then
               Image (Output, Signal_Emission_Data (Data));

            elsif Key in Gir_Reader.Keys.Text_Key then
               Image (Output, Text_Data (Data));

            elsif Key in Gir_Reader.Keys.Element_Key then
               Image (Output, Vector_Data (Data));

            end if;

            if Index /= Item.Iterate.Last then
               Output.Put (",");
               Output.New_Line;
            end if;
         end;
      end loop;

      Output.Decrease_Indent (3);
      Output.New_Line;
      Output.Put (")");

   end Image;

   -----------
   -- Image --
   -----------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Element) is
   begin
      if Item.Internal.Is_Empty then
         Output.Put ("()");
      else
         Image (Output, Real_Element (Item.Internal.Element).Value);
      end if;
   end Image;

end Gir_Reader.Elements;
