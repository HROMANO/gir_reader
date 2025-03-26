--  This package provides custom Image procedures for Boolean, Integer and
--  Text types.

pragma Ada_2022;

with Ada.Strings.Text_Buffers;

package Gir_Reader.Images is

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Boolean);

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Integer);

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Text);

end Gir_Reader.Images;
