--  This package provides the 'Key_Vector' type.

with Ada.Containers.Indefinite_Vectors;

with Gir_Reader.Key_Types;

package Gir_Reader.Key_Vectors is

   use type Gir_Reader.Key_Types.Element_Key;

   --  @private Internal use only.
   package Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Gir_Reader.Key_Types.Element_Key);
   --  (Index_Type => Positive, Element_Type => Key'Class);

   --  The Key_Vector is used to hold a list of 'Key' type keys.
   --  A 'Put_Image' aspect is used to control the display behavior.
   type Key_Vector is new Vectors.Vector with null record
   with Put_Image => Image;

   --  Procedure to display a 'Key_Vector' as '[key name1, ...]'.
   --  @param Output The buffer used for output.
   --  @param Item The 'Key_Vector' to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Key_Vector);

end Gir_Reader.Key_Vectors;
