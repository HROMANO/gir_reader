with Ada.Strings.Fixed;

package body Gir_Reader.Images is

   ---------------
   --  To_String  --
   ---------------

   function To_String (Item : Boolean) return Utf8
   is (if Item then -"true" else -"false");

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Boolean) is
   begin
      Output.Put (To_String (Item));
   end Image;

   ---------------
   --  To_String  --
   ---------------

   function To_String (Item : Integer) return Utf8
   is (Ada.Strings.Fixed.Trim (Item'Image, Ada.Strings.Both));

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Integer) is
   begin
      Output.Put (To_String (Item));
   end Image;

   ---------------
   --  To_String  --
   ---------------

   function To_String
     (Item : Text; Indent : Utf8 := ""; Indent_First_Line : Boolean := False)
      return Utf8
   is
      Result  : Text := Item;
      Start   : Positive := 1;
      Index   : Natural := 0;
      Pattern : String := "" & ASCII.LF;
   begin

      if Ada.Strings.Unbounded.Length (Item) = 0 then
         return "";
      end if;

      if Indent_First_Line then
         Ada.Strings.Unbounded.Insert (Result, 1, Indent);
      end if;

      while True loop

         Index := Ada.Strings.Unbounded.Index (Result, Pattern, Start);

         exit when Index = 0;

         Start := Index + 1;
         Ada.Strings.Unbounded.Insert (Result, Start, Indent);

      end loop;

      return """" & (+Result) & """";

   end To_String;

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Text)
   is

      use Ada.Strings.Fixed;

      Indent : constant Utf8 :=
        (Natural (Output.Current_Indent)
         + Gir_Reader.Images.Default_Indent_Size)
        * " ";
   begin
      Output.Put (To_String (Item, Indent));
   end Image;

end Gir_Reader.Images;
