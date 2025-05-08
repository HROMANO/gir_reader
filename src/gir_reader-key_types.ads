--  This package provides:
--  - The 'Key' type as root of the other key types,
--  - Specialised key types corresponding to the type of the data they
--    are associated with (this allows dispatching and compile time checks),
--  - All keys used in GIR files.
--
--  Source of information used:
--  https://gitlab.gnome.org/GNOME/gobject-introspection/-/blob/main/docs/gir-1.2.rnc
--  Completed with real content of gir files for few missing keys.

pragma Ada_2022;

with Ada.Containers;
with Ada.Strings.Text_Buffers;

package Gir_Reader.Key_Types is

   --  'Key' is the root type of all key types.
   type Key (<>) is tagged private;

   function Create (Text : String) return Key;

   --  'Less_Than' is needed to use keys in ordered maps.
   --  @param Left The left key to compare.
   --  @param Right The right key to compare.
   --  @return True is Left < Right.
   function Less_Than (Left, Right : Key'Class) return Boolean;

   --  'Hash' is needed to use keys in hashed maps.
   --  @param Item The key to hash.
   --  @return Value of the hash.
   function Hash (Item : Key'Class) return Ada.Containers.Hash_Type;

   --  Procedure to display a key.
   --  @param Output The buffer used for output.
   --  @param Item The 'Key' to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Key);

   --  Root type for attribute keys.
   type Attribute_Key is new Key with private;

   --  This key type is to be used in association with 'Boolean' data.
   type Boolean_Key is new Attribute_Key with private;

   --  This key type is to be used in association with 'Integer' data.
   type Integer_Key is new Attribute_Key with private;

   --  This key type is to be used in association with 'Parameter_Direction'
   --  data (see gir_reader.ads).
   type Parameter_Direction_Key is new Attribute_Key with private;

   --  This key type is to be used in association with 'Lifetime_Scope' data
   --  (see gir_reader.ads).
   type Lifetime_Scope_Key is new Attribute_Key with private;

   --  This key type is to be used in association with 'Ownership' data
   --  (see gir_reader.ads).
   type Ownership_Key is new Attribute_Key with private;

   --  This key type is to be used in association with 'Signal_Emission' data
   --  (see gir_reader.ads).
   type Signal_Emission_Key is new Attribute_Key with private;

   --  This key type is to be used in association with 'Text' data
   --  (see gir_reader.ads).
   type Text_Key is new Attribute_Key with private;

   --  This key type is to be used in association with 'Element' data
   --  (see gir_reader-elements.ads).
   type Element_Key is new Key with private;

private

   --  The 'Key' type is a Holder to hide the real type used in the body
   --  of the package.
   --  A 'Put_Image' aspect is used to have a readable and translatable display
   --  output.
   type Key is new Holders.Holder with null record with Put_Image => Image;

   type Attribute_Key is new Key with null record;

   type Boolean_Key is new Attribute_Key with null record;
   type Integer_Key is new Attribute_Key with null record;
   type Parameter_Direction_Key is new Attribute_Key with null record;
   type Lifetime_Scope_Key is new Attribute_Key with null record;
   type Ownership_Key is new Attribute_Key with null record;
   type Signal_Emission_Key is new Attribute_Key with null record;
   type Text_Key is new Attribute_Key with null record;

   type Element_Key is new Key with null record;

end Gir_Reader.Key_Types;
