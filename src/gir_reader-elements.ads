--  This package provides the Element type.

with Ada.Strings.Unbounded;
with Ada.Strings.Text_Buffers;

with Gir_Reader.Attribute_Maps;
with Gir_Reader.Key_Types;
with Gir_Reader.Key_Lists;

limited with Gir_Reader.Element_Lists;

package Gir_Reader.Elements is

   use type Gir_Reader.Attribute_Maps.Attribute_Map;
   use type Ada.Strings.Unbounded.Unbounded_String;

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

   --  Checks if the element contains some sub-elements.
   --  @param Self The element.
   --  @param Item The requested key of sub-elements.
   --  @return True if there are sub-elements corresponding to that key.
   function Contains
     (Self : Element; Item : Gir_Reader.Key_Types.Element_Key) return Boolean;

   ------------------
   --  Get methods --
   ------------------

   --  Get all sub-elements' keys.
   --  @param Self The element.
   --  @return The list of keys.
   function Get_Sub_Elements_Key_List
     (Self : Element) return Gir_Reader.Key_Lists.Element_Key_List;

   --  Get attributes of the element.
   --  @param Self The element.
   --  @return The attributes of the element.
   function Get_Attributes
     (Self : Element) return Gir_Reader.Attribute_Maps.Attribute_Map;

   --  Get the content of the element.
   --  @param Self The element.
   --  @return The requested content.
   function Get_Content (Self : Element) return Utf8;

   --  Get the requested sub-element key of the element or the provided default
   --  value if Self doesn't contain that key.
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

   --  Set the value of the attributes.
   --  @param Self The Element.
   --  @param Value A map of attributes.
   procedure Set
     (Self : in out Element; Value : Gir_Reader.Attribute_Maps.Attribute_Map)
   with Post'Class => Self.Get_Attributes = Value;

   --  Set the content of the element.
   --  @param Self The Element.
   --  @param Value A string.
   procedure Set_Content
     (Self : in out Element; Value : Ada.Strings.Unbounded.Unbounded_String)
   with Post'Class => Self.Get_Content = Value;

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

   --  Syntactic sugar to get a sub-element list.
   --  @param Left An element.
   --  @param Right The key of the requested sub-element.
   --  @return The list of the requested sub-element or an empty list.
   function "/"
     (Left : Element; Right : Gir_Reader.Key_Types.Element_Key)
      return Gir_Reader.Element_Lists.List;

   --  Syntactic sugar to get the first element of a sub-element list.
   --  @param Left An element.
   --  @param Right The key of the requested sub-element.
   --  @return The first element of the requested sub-element or an empty
   --  element.
   function "/"
     (Left : Element; Right : Gir_Reader.Key_Types.Element_Key) return Element;

private

   --  The 'Element' type is a Holder to hide the real type used in the body
   --  of the package.
   --  A 'Put_Image' aspect is used to have a readable and translatable display
   --  output.
   type Element is new Holders.Holder with null record with Put_Image => Image;

end Gir_Reader.Elements;
