with Ada.Strings.Hash;

package body Gir_Reader.Key_Types is

   type Real_Key (Length : Positive) is new Holder_Content_Root with record
      Name : String (1 .. Length);
   end record;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : Key'Class) return Boolean
   is (Real_Key (Left.Element).Name < Real_Key (Right.Element).Name);

   ----------
   -- Hash --
   ----------

   function Hash (Item : Key'Class) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (Real_Key (Item.Element).Name));

   -----------
   -- Image --
   -----------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Key) is
   begin
      if Item.Is_Empty then
         Log_Error (-"No key. This is a bug in the library.");
      end if;

      Output.Put (Real_Key (Item.Element).Name);
   end Image;

   -------------
   --  Create --
   -------------

   function Create (Text : String) return Key is
      The_Key    : constant Real_Key := (Length => Text'Length, Name => Text);
      The_Holder : Key;
   begin
      The_Holder.Replace_Element (The_Key);
      return The_Holder;
   end Create;

end Gir_Reader.Key_Types;
