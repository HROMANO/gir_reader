pragma Ada_2022;

with Ada.Containers.Indefinite_Ordered_Maps;

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

   type Text_Data is new Holder_Content_Root with record
      Value : Text;
   end record;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Text_Data) is
   begin
      Gir_Reader.Images.Image (Output, Item.Value);
   end Image;

   --
   --  Element_Map type
   --

   package Element_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Gir_Reader.Key_Types.Key'Class,
        Element_Type => Holder_Content_Root'Class,
        "<"          => Gir_Reader.Key_Types.Less_Than);

   --
   --  Real_Element type
   --

   type Real_Element is new Element_Maps.Map and Holder_Content_Root
   with null record;

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

      for Index in Item.Iterate loop
         declare
            Key  : constant Gir_Reader.Key_Types.Key'Class :=
              Element_Maps.Key (Index);
            Data : constant Holder_Content_Root'Class := Item.Element (Key);
         begin
            Gir_Reader.Key_Types.Image (Output, Key);
            Output.Put (": ");

            --  TODO: could this be avoided?
            if Key in Gir_Reader.Key_Types.Boolean_Key then
               Image (Output, Boolean_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Parameter_Direction_Key then
               Image (Output, Parameter_Direction_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Integer_Key then
               Image (Output, Integer_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Lifetime_Scope_Key then
               Image (Output, Lifetime_Scope_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Ownership_Key then
               Image (Output, Ownership_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Signal_Emission_Key then
               Image (Output, Signal_Emission_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Text_Key then
               Image (Output, Text_Data (Data));

            elsif Key in Gir_Reader.Key_Types.Element_Key then
               Image (Output, Vector_Data (Data));

            end if;

            if Element_Maps.Next (Index) /= Element_Maps.No_Element then
               Output.Put (",");
               Output.New_Line;
            end if;
         end;
      end loop;

      Output.Decrease_Indent (3);
      Output.New_Line;
      Output.Put (")");

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
     (Self : Element; Item : Gir_Reader.Key_Types.Key'Class) return Boolean is
   begin
      if Self.Is_Empty then
         return False;
      end if;

      return Real_Element (Self.Element).Contains (Item);
   end Contains;

   ------------------------------
   -- Get_Sub_Element_Key_List --
   ------------------------------

   function Get_Sub_Element_Key_List
     (Self : Element) return Gir_Reader.Key_Vectors.Key_Vector
   is
      Result : Gir_Reader.Key_Vectors.Key_Vector;
   begin
      if Self.Is_Empty then
         return Result;
      end if;

      declare
         Map : constant Real_Element := Real_Element (Self.Element);
      begin
         for Iterator in Map.Iterate loop
            declare
               K : constant Gir_Reader.Key_Types.Key'Class :=
                 Element_Maps.Key (Iterator);
            begin
               if K in Gir_Reader.Key_Types.Element_Key then
                  Result.Append (Gir_Reader.Key_Types.Element_Key (K));
               end if;
            end;
         end loop;
      end;

      return Result;
   end Get_Sub_Element_Key_List;

   ------------------
   -- Internal_Get --
   ------------------

   function Internal_Get
     (Self : Element; Item : Gir_Reader.Key_Types.Key'Class)
      return Holder_Content_Root'Class
   is (Real_Element (Self.Element) (Item))
   with Inline;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Boolean_Key) return Boolean
   is (Boolean_Data (Internal_Get (Self, Item)).Value);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Boolean_Key;
      Default : Boolean) return Boolean
   is (if Self.Contains (Item) then Self.Get (Item) else Default);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Parameter_Direction_Key)
      return Parameter_Direction
   is (Parameter_Direction_Data (Internal_Get (Self, Item)).Value);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Parameter_Direction_Key;
      Default : Parameter_Direction) return Parameter_Direction
   is (if Self.Contains (Item) then Self.Get (Item) else Default);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Integer_Key) return Integer
   is (Integer_Data (Internal_Get (Self, Item)).Value);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Integer_Key;
      Default : Integer) return Integer
   is (if Self.Contains (Item) then Self.Get (Item) else Default);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Lifetime_Scope_Key)
      return Lifetime_Scope
   is (Lifetime_Scope_Data (Internal_Get (Self, Item)).Value);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Lifetime_Scope_Key;
      Default : Lifetime_Scope) return Lifetime_Scope
   is (if Self.Contains (Item) then Self.Get (Item) else Default);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Ownership_Key)
      return Ownership
   is (Ownership_Data (Internal_Get (Self, Item)).Value);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Ownership_Key;
      Default : Ownership) return Ownership
   is (if Self.Contains (Item) then Self.Get (Item) else Default);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Signal_Emission_Key)
      return Signal_Emission
   is (Signal_Emission_Data (Internal_Get (Self, Item)).Value);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Signal_Emission_Key;
      Default : Signal_Emission) return Signal_Emission
   is (if Self.Contains (Item) then Self.Get (Item) else Default);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Text_Key) return Text
   is (Text_Data (Internal_Get (Self, Item)).Value);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self : Element; Item : Gir_Reader.Key_Types.Text_Key; Default : Text)
      return Text
   is (if Self.Contains (Item) then Self.Get (Item) else Default);

   ---------
   -- Get --
   ---------

   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Element_Key)
      return Gir_Reader.Element_Lists.List
   is (Vector_Data (Internal_Get (Self, Item)).Value);

   -----------------
   -- Get_Or_Else --
   -----------------

   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Element_Key;
      Default : Gir_Reader.Element_Lists.List)
      return Gir_Reader.Element_Lists.List
   is (if Self.Contains (Item) then Self.Get (Item) else Default);

   ------------------
   -- Internal_Set --
   ------------------

   procedure Internal_Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Key'Class;
      Value : Holder_Content_Root'Class) is
   begin
      if Self.Is_Empty then

         declare
            R : Real_Element;
         begin
            R.Insert (Item, Value);
            Self.Replace_Element (R);
         end;

      elsif Real_Element (Self.Element).Contains (Item) then
         Real_Element (Self.Reference.Element.all) (Item) := Value;
      else
         Real_Element (Self.Reference.Element.all).Insert (Item, Value);
      end if;

   end Internal_Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Boolean_Key;
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
      Item  : Gir_Reader.Key_Types.Parameter_Direction_Key;
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
      Item  : Gir_Reader.Key_Types.Integer_Key;
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
      Item  : Gir_Reader.Key_Types.Lifetime_Scope_Key;
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
      Item  : Gir_Reader.Key_Types.Ownership_Key;
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
      Item  : Gir_Reader.Key_Types.Signal_Emission_Key;
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
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Text_Key;
      Value : Text)
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
      Item  : Gir_Reader.Key_Types.Element_Key;
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
      Item  : Gir_Reader.Key_Types.Element_Key;
      Value : Element)
   is
      List : Gir_Reader.Element_Lists.List;
   begin
      if Self.Is_Empty
        or else not Real_Element (Self.Element).Contains
                      (Gir_Reader.Key_Types.Key (Item))
      then
         List.Append (Value);
         Self.Set (Item, List);
      else
         --  TODO: avoid copying (doesn't work below). Element_Lists needs
         --  some reference access?
         --  Vector_Data (Real_Element (Self.Reference.Element.all) (Item))
         --  .Value.Append (Value);
         List := Self.Get (Item);
         List.Append (Value);
         Self.Set (Item, List);
      end if;
   end Append;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes (Self : Element) return Element is
   begin
      return Result : Element do
         if not Self.Is_Empty then

            declare
               Map : constant Real_Element := Real_Element (Self.Element);
            begin
               for Iterator in Map.Iterate loop

                  declare
                     K : Gir_Reader.Key_Types.Key'Class :=
                       Element_Maps.Key (Iterator);
                  begin
                     if K not in Gir_Reader.Key_Types.Element_Key then
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
      Item   : Element) is
   begin
      if Item.Is_Empty then
         Output.Put ("()");
      else
         Image (Output, Real_Element (Item.Element));
      end if;
   end Image;

end Gir_Reader.Elements;
