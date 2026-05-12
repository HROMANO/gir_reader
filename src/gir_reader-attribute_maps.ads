pragma Ada_2022;

with Ada.Strings.Text_Buffers;

with Gir_Reader.Key_Lists;
with Gir_Reader.Key_Types;

package Gir_Reader.Attribute_Maps is

   type Attribute_Map is tagged private
   with Default_Initial_Condition => Is_Empty (Attribute_Map);

   --  Returns an empty attribute map.
   --  @return An empty attribute map.
   function Empty_Attribute_Map return Attribute_Map;

   --  Clears all content of an attribute map.
   --  @param Self The attribute map.
   procedure Clear (Self : in out Attribute_Map)
   with Post'Class => Self.Is_Empty;

   --  Checks if the attribute map is empty.
   --  @param Self The attribute map.
   --  @return True if the attribute map is empty.
   function Is_Empty (Self : Attribute_Map) return Boolean;

   --  Checks if the attribute map contains the key.
   --  @param Self The attribute map.
   --  @param Item The requested key.
   --  @return True if the key exists in the attribute map.
   function Contains
     (Self : Attribute_Map; Item : Gir_Reader.Key_Types.Attribute_Key'Class) return Boolean;

   --  Get attribute keys of the attribute map.
   --  @param Self The attribute map.
   --  @return The .
   function Get_Attribute_Keys (Self : Attribute_Map) return Gir_Reader.Key_Lists.Attribute_Key_List;

   ------------------
   --  Get methods --
   ------------------

   --  Get the requested boolean key of the attribute map or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self the attribute map.
   --  @param Item A boolean key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Boolean_Key;
      Default : Boolean) return Boolean;

   --  Get the requested in out key of the attribute map or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self the attribute map.
   --  @param Item An in out key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Parameter_Direction_Key;
      Default : Parameter_Direction) return Parameter_Direction;

   --  Get the requested integer key of the attribute map or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self the attribute map.
   --  @param Item An integer key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Integer_Key;
      Default : Integer) return Integer;

   --  Get the requested lifetime scope key of the attribute map or the provided
   --  default value if Self doesn't contain the key.
   --  @param Self the attribute map.
   --  @param Item A lifetime scope key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Lifetime_Scope_Key;
      Default : Lifetime_Scope) return Lifetime_Scope;

   --  Get the requested ownership key of the attribute map or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self the attribute map.
   --  @param Item An ownership key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Ownership_Key;
      Default : Ownership) return Ownership;

   --  Get the requested signal emission key of the attribute map or the provided
   --  default value if Self doesn't contain the key.
   --  @param Self the attribute map.
   --  @param Item A signal emission key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Attribute_Map;
      Item    : Gir_Reader.Key_Types.Signal_Emission_Key;
      Default : Signal_Emission) return Signal_Emission;

   --  Get the requested text key of the attribute map or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self the attribute map.
   --  @param Item A text key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self : Attribute_Map; Item : Gir_Reader.Key_Types.Text_Key; Default : Text)
      return Text;

   ------------------
   --  Set methods --
   ------------------

   --  Set the value of the requested boolean key for the attribute map.
   --  @param Self the attribute map.
   --  @param Item A boolean key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Boolean_Key;
      Value : Boolean)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested in out key for the attribute map.
   --  @param Self the attribute map.
   --  @param Item An in out key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Parameter_Direction_Key;
      Value : Parameter_Direction)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested integer key for the attribute map.
   --  @param Self the attribute map.
   --  @param Item An integer key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Integer_Key;
      Value : Integer)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested lifeteim scope key for the attribute map.
   --  @param Self the attribute map.
   --  @param Item A lifetime scope key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Lifetime_Scope_Key;
      Value : Lifetime_Scope)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested ownership key for the attribute map.
   --  @param Self the attribute map.
   --  @param Item An ownership key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Ownership_Key;
      Value : Ownership)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested signal emission key for the attribute map.
   --  @param Self the attribute map.
   --  @param Item A signal emission key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Signal_Emission_Key;
      Value : Signal_Emission)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested text key for the attribute map.
   --  @param Self the attribute map.
   --  @param Item A text key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Text_Key;
      Value : Text)
   with Post'Class => Self.Contains (Item);

   --------------
   --  Others  --
   --------------

   --  Unset the value of the requested key.
   --  @param Self the attribute map.
   --  @param Item An attribute key.
   procedure Unset
     (Self  : in out Attribute_Map;
      Item  : Gir_Reader.Key_Types.Attribute_Key'Class)
   with Post'Class => not Self.Contains (Item);

   --  Custom procedure to display an attribute map.
   --  @param Output The buffer used for output.
   --  @param Item The attribute map to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Attribute_Map);

private

   --  The Attribute_Map type is a Holder to hide the real type used in the body
   --  of the package.
   --  A 'Put_Image' aspect is used to have a readable and translatable display
   --  output.
   type Attribute_Map is new Holders.Holder with null record with Put_Image => Image;

end Gir_Reader.Attribute_Maps;
