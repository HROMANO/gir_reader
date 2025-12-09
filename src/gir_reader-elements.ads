--  This package provides the Element type.

pragma Ada_2022;

with Ada.Strings.Text_Buffers;

with Gir_Reader.Key_Types;
with Gir_Reader.Key_Lists;

limited with Gir_Reader.Element_Lists;

package Gir_Reader.Elements is

   --  This is the main type of the Ada structure translated from the gir file.
   type Element is tagged private
   with Default_Initial_Condition => Is_Empty (Element);

   --  Returns an empty element.
   --  @return An empty element.
   function Empty_Element return Element;

   --  Clears all content of an element.
   --  @param Self The element.
   procedure Clear (Self : in out Element)
   with Post'Class => Self.Is_Empty;

   --  Checks if the element is empty.
   --  @param Self The element.
   --  @return True if the element is empty.
   function Is_Empty (Self : Element) return Boolean;

   --  Checks if the element contains the key.
   --  @param Self The element.
   --  @param Item The requested key.
   --  @return True if the key exists in the element.
   function Contains
     (Self : Element; Item : Gir_Reader.Key_Types.Key'Class) return Boolean;

   --  Get all keys of sub elements.
   --  @param Self The element.
   --  @return The list of keys.
   function Get_Sub_Element_Key_List
     (Self : Element) return Gir_Reader.Key_Lists.Element_Key_List;

   --  Get attributes of the element excluding sub-elements.
   --  @param Self The element.
   --  @return The filtered element.
   function Get_Attributes (Self : Element) return Element;

   ------------------
   --  Get methods --
   ------------------

   --  Get the requested boolean key of the element.
   --  To avoid exception, check the existence of the key before the call.
   --  @param Self The element.
   --  @param Item A boolean key.
   --  @return The value of the key.
   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Boolean_Key) return Boolean
   with Pre'Class => Self.Contains (Item) or else raise Key_Error;

   --  Get the requested boolean key of the element or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self The element.
   --  @param Item A boolean key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Boolean_Key;
      Default : Boolean) return Boolean;

   --  Get the requested in out key of the element.
   --  To avoid exception, check the existence of the key before the call.
   --  @param Self The element.
   --  @param Item An in out key.
   --  @return The value of the key.
   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Parameter_Direction_Key)
      return Parameter_Direction
   with Pre'Class => Self.Contains (Item) or else raise Key_Error;

   --  Get the requested in out key of the element or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self The element.
   --  @param Item An in out key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Parameter_Direction_Key;
      Default : Parameter_Direction) return Parameter_Direction;

   --  Get the requested integer key of the element.
   --  To avoid exception, check the existence of the key before the call.
   --  @param Self The element.
   --  @param Item An integer key.
   --  @return The value of the key.
   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Integer_Key) return Integer
   with Pre'Class => Self.Contains (Item) or else raise Key_Error;

   --  Get the requested integer key of the element or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self The element.
   --  @param Item An integer key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Integer_Key;
      Default : Integer) return Integer;

   --  Get the requested lifetime scope key of the element.
   --  To avoid exception, check the existence of the key before the call.
   --  @param Self The element.
   --  @param Item A lifetime scope key.
   --  @return The value of the key.
   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Lifetime_Scope_Key)
      return Lifetime_Scope
   with Pre'Class => Self.Contains (Item) or else raise Key_Error;

   --  Get the requested lifetime scope key of the element or the provided
   --  default value if Self doesn't contain the key.
   --  @param Self The element.
   --  @param Item A lifetime scope key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Lifetime_Scope_Key;
      Default : Lifetime_Scope) return Lifetime_Scope;

   --  Get the requested ownership key of the element.
   --  To avoid exception, check the existence of the key before the call.
   --  @param Self The element.
   --  @param Item An ownership key.
   --  @return The value of the key.
   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Ownership_Key)
      return Ownership
   with Pre'Class => Self.Contains (Item) or else raise Key_Error;

   --  Get the requested ownership key of the element or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self The element.
   --  @param Item An ownership key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Ownership_Key;
      Default : Ownership) return Ownership;

   --  Get the requested signal emission key of the element.
   --  To avoid exception, check the existence of the key before the call.
   --  @param Self The element.
   --  @param Item A signal emission key.
   --  @return The value of the key.
   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Signal_Emission_Key)
      return Signal_Emission
   with Pre'Class => Self.Contains (Item) or else raise Key_Error;

   --  Get the requested signal emission key of the element or the provided
   --  default value if Self doesn't contain the key.
   --  @param Self The element.
   --  @param Item A signal emission key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Signal_Emission_Key;
      Default : Signal_Emission) return Signal_Emission;

   --  Get the requested text key of the element.
   --  To avoid exception, check the existence of the key before the call.
   --  @param Self The element.
   --  @param Item A text key.
   --  @return The value of the key.
   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Text_Key) return Text
   with Pre'Class => Self.Contains (Item) or else raise Key_Error;

   --  Get the requested text key of the element or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self The element.
   --  @param Item A text key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self : Element; Item : Gir_Reader.Key_Types.Text_Key; Default : Text)
      return Text;

   --  Get the requested sub-element key of the element.
   --  To avoid exception, check the existence of the key before the call.
   --  Note: sub-elements are always of type 'List'.
   --  @param Self The element.
   --  @param Item An element key.
   --  @return The value of the key.
   function Get
     (Self : Element; Item : Gir_Reader.Key_Types.Element_Key)
      return Gir_Reader.Element_Lists.List
   with Pre => Self.Contains (Item) or else raise Key_Error;

   --  Get the requested sub-element key of the element or the provided default
   --  value if Self doesn't contain the key.
   --  @param Self The element.
   --  @param Item A text key.
   --  @param Default Default return value.
   --  @return The value of the key or the default value.
   function Get_Or_Else
     (Self    : Element;
      Item    : Gir_Reader.Key_Types.Element_Key;
      Default : Gir_Reader.Element_Lists.List)
      return Gir_Reader.Element_Lists.List;

   ------------------
   --  Set methods --
   ------------------

   --  Set the value of the requested boolean key for the element.
   --  @param Self The element.
   --  @param Item A boolean key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Boolean_Key;
      Value : Boolean)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested in out key for the element.
   --  @param Self The element.
   --  @param Item An in out key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Parameter_Direction_Key;
      Value : Parameter_Direction)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested integer key for the element.
   --  @param Self The element.
   --  @param Item An integer key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Integer_Key;
      Value : Integer)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested lifeteim scope key for the element.
   --  @param Self The element.
   --  @param Item A lifetime scope key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Lifetime_Scope_Key;
      Value : Lifetime_Scope)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested ownership key for the element.
   --  @param Self The element.
   --  @param Item An ownership key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Ownership_Key;
      Value : Ownership)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested signal emission key for the element.
   --  @param Self The element.
   --  @param Item A signal emission key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Signal_Emission_Key;
      Value : Signal_Emission)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested text key for the element.
   --  @param Self The element.
   --  @param Item A text key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Text_Key;
      Value : Text)
   with Post'Class => Self.Contains (Item);

   --  Set the value of the requested sub-element key for the element.
   --  Note: sub-elements are always of type 'List'.
   --  @param Self The element.
   --  @param Item An element key.
   --  @param Value The value for the key.
   procedure Set
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Element_Key;
      Value : Gir_Reader.Element_Lists.List)
   with Post'Class => Self.Contains (Item);

   --------------
   --  Others  --
   --------------

   --  Appends an Element to the requested sub-element key for the element.
   --  Note: sub-elements are always of type 'List'.
   --  @param Self The element.
   --  @param Item An element key.
   --  @param Value The value for the key.
   procedure Append
     (Self  : in out Element;
      Item  : Gir_Reader.Key_Types.Element_Key;
      Value : Element)
   with Post'Class => Self.Contains (Item);

   --  Custom procedure to display an element.
   --  @param Output The buffer used for output.
   --  @param Item The 'Element' to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Element);

private

   --  The 'Element' type is a Holder to hide the real type used in the body
   --  of the package.
   --  A 'Put_Image' aspect is used to have a readable and translatable display
   --  output.
   type Element is new Holders.Holder with null record with Put_Image => Image;

end Gir_Reader.Elements;
