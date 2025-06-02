pragma Ada_2022;

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Text_Buffers;

generic
   type Element (<>) is tagged private;
   with procedure Element_Image (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
                                 Item   : Element) is <>;
package Gir_Reader.Generic_Lists is

   use type Element;

   package Element_Lists is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Element);

   type Element_List is new Element_Lists.Vector with null record
     with Put_Image => Image;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Element_List);

end Gir_Reader.Generic_Lists;
