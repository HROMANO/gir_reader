pragma Ada_2022;

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Text_Buffers;

generic
   type T (<>) is tagged private;
   with
     procedure T_Image
       (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
        Item   : T) is <>;
package Gir_Reader.Generic_Lists is

   package T_Lists is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => T);

   type T_List is new T_Lists.Vector with null record
   with Put_Image => Image;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : T_List);

   function To_String (Item : T_List) return Utf8;

   function Empty_List return T_List;

end Gir_Reader.Generic_Lists;
