package body Gir_Reader is

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Parameter_Direction) is
   begin
      case Item is
         when Gir_Reader.Is_In =>
            Output.Put (-"in");

         when Gir_Reader.Is_Out =>
            Output.Put (-"out");

         when Gir_Reader.Is_In_Out =>
            Output.Put (-"in out");
      end case;
   end Image;

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Lifetime_Scope) is
   begin
      case Item is
         when Gir_Reader.Notified =>
            Output.Put (-"notified");

         when Gir_Reader.Async =>
            Output.Put (-"async");

         when Gir_Reader.Call =>
            Output.Put (-"call");

         when Gir_Reader.Forever =>
            Output.Put (-"forever");
      end case;
   end Image;

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Ownership) is
   begin
      case Item is
         when Gir_Reader.None =>
            Output.Put (-"none");

         when Gir_Reader.Container =>
            Output.Put (-"container");

         when Gir_Reader.Full =>
            Output.Put (-"full");
      end case;
   end Image;

   -------------
   --  Image  --
   -------------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Signal_Emission) is
   begin
      case Item is
         when Gir_Reader.First =>
            Output.Put (-"first");

         when Gir_Reader.Last =>
            Output.Put (-"last");

         when Gir_Reader.Cleanup =>
            Output.Put (-"cleanup");
      end case;
   end Image;

end Gir_Reader;
