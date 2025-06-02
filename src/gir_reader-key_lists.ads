pragma Ada_2022;

--  This package provides the 'Element_Key_List' type.

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Text_Buffers;

with Gir_Reader.Generic_Lists;
with Gir_Reader.Key_Types;

package Gir_Reader.Key_Lists is

   package Key_Lists is new Gir_Reader.Generic_Lists
     (Element       => Gir_Reader.Key_Types.Key,
      Element_Image => Gir_Reader.Key_Types.Image);

   subtype Key_List is Key_Lists.Element_List;

   package Attribute_Key_Lists is new Gir_Reader.Generic_Lists
     (Element       => Gir_Reader.Key_Types.Attribute_Key,
      Element_Image => Gir_Reader.Key_Types.Image);

   subtype Attribute_Key_List is Attribute_Key_Lists.Element_List;

   package Boolean_Key_Lists is new Gir_Reader.Generic_Lists
     (Element       => Gir_Reader.Key_Types.Boolean_Key,
      Element_Image => Gir_Reader.Key_Types.Image);

   subtype Boolean_Key_List is Boolean_Key_Lists.Element_List;

   package Integer_Key_Lists is new Gir_Reader.Generic_Lists
     (Element       => Gir_Reader.Key_Types.Integer_Key,
      Element_Image => Gir_Reader.Key_Types.Image);

   subtype Integer_Key_List is Integer_Key_Lists.Element_List;

   package Parameter_Direction_Key_Lists is new Gir_Reader.Generic_Lists
     (Element       => Gir_Reader.Key_Types.Parameter_Direction_Key,
      Element_Image => Gir_Reader.Key_Types.Image);

   subtype Parameter_Direction_Key_List is Parameter_Direction_Key_Lists.Element_List;

   package Lifetime_Scope_Key_Lists is new Gir_Reader.Generic_Lists
     (Element       => Gir_Reader.Key_Types.Lifetime_Scope_Key,
      Element_Image => Gir_Reader.Key_Types.Image);

   subtype Lifetime_Scope_Key_List is Lifetime_Scope_Key_Lists.Element_List;

   package Ownership_Key_Lists is new Gir_Reader.Generic_Lists
     (Element       => Gir_Reader.Key_Types.Ownership_Key,
      Element_Image => Gir_Reader.Key_Types.Image);

   subtype Ownership_Key_List is Ownership_Key_Lists.Element_List;

   package Signal_Emission_Key_Lists is new Gir_Reader.Generic_Lists
     (Element       => Gir_Reader.Key_Types.Signal_Emission_Key,
      Element_Image => Gir_Reader.Key_Types.Image);

   subtype Signal_Emission_Key_List is Signal_Emission_Key_Lists.Element_List;

   package Text_Key_Lists is new Gir_Reader.Generic_Lists
     (Element       => Gir_Reader.Key_Types.Text_Key,
      Element_Image => Gir_Reader.Key_Types.Image);

   subtype Text_Key_List is Text_Key_Lists.Element_List;

   package Element_Key_Lists is new Gir_Reader.Generic_Lists
     (Element       => Gir_Reader.Key_Types.Element_Key,
      Element_Image => Gir_Reader.Key_Types.Image);

   subtype Element_Key_List is Element_Key_Lists.Element_List;

end Gir_Reader.Key_Lists;
