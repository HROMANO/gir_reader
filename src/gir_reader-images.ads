--  This package provides custom To_String and Image procedures for Boolean,
--  Integer and Text types.

pragma Ada_2022;

with Ada.Strings.Text_Buffers;

package Gir_Reader.Images is

   Default_Indent_Size : constant Natural := 3;

   function To_String (Item : Boolean) return Utf8;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Boolean);

   function To_String (Item : Integer) return Utf8;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Integer);

   function To_String
     (Item : Text; Indent : Utf8 := ""; Indent_First_Line : Boolean := False)
      return Utf8;

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Text);

end Gir_Reader.Images;
