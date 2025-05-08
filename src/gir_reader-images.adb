with Ada.Strings.Fixed;

package body Gir_Reader.Images is

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Boolean) is
   begin
      if Item then
         Output.Put (-"true");
      else
         Output.Put (-"false");
      end if;
   end Image;

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Integer) is
   begin
      Output.Put (Ada.Strings.Fixed.Trim (Item'Image, Ada.Strings.Both));
   end Image;

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Text) is
   begin
      Output.Put ("""" & (+Item) & """");
   end Image;

end Gir_Reader.Images;
