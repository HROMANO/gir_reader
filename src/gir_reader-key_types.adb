with Ada.Strings.Hash;

package body Gir_Reader.Key_Types is

   type Real_Key (Length : Positive) is new Holder_Content_Root with record
      Name : String (1 .. Length);
   end record;

   -----------
   -- Equal --
   -----------

   function "=" (Left : Key; Right : Key) return Boolean
   is (Left.To_String = Right.To_String);

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : Key'Class) return Boolean
   is (Left.To_String < Right.To_String);

   ----------
   -- Hash --
   ----------

   function Hash (Item : Key'Class) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (Item.To_String));

   ---------------
   -- To_String --
   ---------------

   function To_String (Item : Key) return Utf8 is
   begin
      if Item.Is_Empty then
         return "";
      else
         return (Real_Key (Item.Element).Name);
      end if;
   end To_String;

   -----------
   -- Image --
   -----------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Key) is
   begin
      Output.Put (Item.To_String);
   end Image;

   -------------
   --  Create --
   -------------

   function Create (Text : Utf8) return Key is
      The_Key    : constant Real_Key := (Length => Text'Length, Name => Text);
      The_Holder : Key;
   begin
      The_Holder.Replace_Element (The_Key);
      return The_Holder;
   end Create;

end Gir_Reader.Key_Types;
