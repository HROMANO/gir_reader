--  This package provides the Gir_Reader.Elements_Lists.List type.
--  Not all functions useful for lists are implemented. Only those
--  used elsewhere in the library.

pragma Ada_2022;

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Text_Buffers;

with Gir_Reader.Elements;

package Gir_Reader.Element_Lists is

   use type Gir_Reader.Elements.Element;

   --  @private Internal use only.
   package Element_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Gir_Reader.Elements.Element);

   -- This type is used to hold a list of Gir_Reader.Elements.Element.
   --  A 'Put_Image' aspect is used to control the display behavior.
   type List is new Element_Vectors.Vector with null record
   with Put_Image => Image;

   --  Custom procedure to display a list.
   --  @param Output The buffer used for output.
   --  @param Item The list to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : List);

end Gir_Reader.Element_Lists;
