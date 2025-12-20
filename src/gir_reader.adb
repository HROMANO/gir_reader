with Gettexts;

package body Gir_Reader is

   -----------
   --  "-"  --
   -----------

   function "-" (Message : String) return Utf8
   is (Gettexts.Get_Text_With_Domain
       (Domain_Name => "gir_reader", Message_Id => Message));

   -----------------
   --  To_String  --
   -----------------

   function To_String (Item : Parameter_Direction) return Utf8 is
   begin
      case Item is
         when Gir_Reader.Is_In =>
            return -"in";

         when Gir_Reader.Is_Out =>
            return -"out";

         when Gir_Reader.Is_In_Out =>
            return -"in out";
      end case;
   end To_String;

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Parameter_Direction) is
   begin
      Output.Put (To_String (Item));
   end Image;

   -----------------
   --  To_String  --
   -----------------

   function To_String (Item : Lifetime_Scope) return Utf8 is
   begin
      case Item is
         when Gir_Reader.Notified =>
            return -"notified";

         when Gir_Reader.Async =>
            return -"async";

         when Gir_Reader.Call =>
            return -"call";

         when Gir_Reader.Forever =>
            return -"forever";
      end case;
   end To_String;

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Lifetime_Scope) is
   begin
      Output.Put (To_String (Item));
   end Image;

   -----------------
   --  To_String  --
   -----------------

   function To_String (Item : Ownership) return Utf8 is
   begin
      case Item is
         when Gir_Reader.None =>
            return -"none";

         when Gir_Reader.Container =>
            return -"container";

         when Gir_Reader.Full =>
            return -"full";
      end case;
   end To_String;

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Ownership) is
   begin
      Output.Put (To_String (Item));
   end Image;

   -----------------
   --  To_String  --
   -----------------

   function To_String (Item : Signal_Emission) return Utf8 is
   begin
      case Item is
         when Gir_Reader.First =>
            return -"first";

         when Gir_Reader.Last =>
            return -"last";

         when Gir_Reader.Cleanup =>
            return -"cleanup";
      end case;
   end To_String;

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Signal_Emission) is
   begin
      Output.Put (To_String (Item));
   end Image;

end Gir_Reader;
