--  This package provides the Gir_Reader.Elements_Lists.List type.
--  Not all functions useful for lists are implemented. Only those
--  used elsewhere in the library.

pragma Ada_2022;

private with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Text_Buffers;

with Gir_Reader.Elements;

package Gir_Reader.Element_Lists is

   --  This type is used to hold a list of Gir_Reader.Elements.Element.
   type List is tagged private
   with Default_Initial_Condition => Is_Empty (List);

   --  Returns an empty list.
   --  @return An empty list.
   function Empty_List return List;

   --  Check if the list is empty.
   --  @param Self The list.
   --  @return True if the list is empty.
   function Is_Empty (Self : List) return Boolean;

   --  Length of the list.
   --  @param Self The list.
   --  @return A natural giving the length of the list.
   function Length (Self : List) return Natural;

   --  Get an element of the list. The index of the element must be at most
   --  equal to the length of the list (this covers the empty list case).
   --  @param Self The list.
   --  @param Index The positive index of the requested element in the list.
   --  @return The element at the requested index.
   function Get
     (Self : List; Index : Positive) return Gir_Reader.Elements.Element
   with Pre => Self.Length >= Index;

   --  Get the last element of the list. The list must be non-empty.
   --  @param Self The list.
   --  @return The last element.
   function Last_Element (Self : List) return Gir_Reader.Elements.Element
   with Pre => not Self.Is_Empty;

   --  Deletes the last element of the list. The list must be non-empty.
   --  @param Self The list.
   procedure Delete_Last (Self : in out List)
   with Pre => not Self.Is_Empty, Post => Self.Length = Self'Old.Length - 1;

   --  Deletes all elements of the list.
   --  @param Self The list.
   procedure Clear (Self : in out List)
   with Post => Self.Is_Empty;

   --  Appends an element to the list.
   --  @param Self The list.
   --  @param Item The element to append.
   procedure Append (Self : in out List; Item : Gir_Reader.Elements.Element)
   with Post => Self.Length = Self'Old.Length + 1;

   --  Custom procedure to display a list.
   --  @param Output The buffer used for output.
   --  @param Item The list to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : List);

private

   --  Indefinite_Holders are used here to avoid a cross dependency with
   --  Gir_Reader.Elements specification. This specification is only used
   --  in the body as allowed in Ada.

   --  @private The 'Root' type is used to instanciate the 'Holders' package.
   type Root is abstract tagged null record;

   --  @private Internal use only.
   package Holders is new
     Ada.Containers.Indefinite_Holders (Element_Type => Root'Class);

   --  The 'List' type uses a Holder to allow:
   --  - lists to contain elements,
   --  - and elements to point to lists.
   type List is new Root with record
      Internal : Holders.Holder;
   end record
   with Put_Image => Image;

end Gir_Reader.Element_Lists;
