--  This library was developed to translate a gobject introspection file
--  (gir file) to an Ada structure with type guarantees at compile time for
--  users.
--
--  The main type is Gir_Reader.Elements.Element and the main function
--  is Gir_Reader.Readers.Read.
--
--  To explore the content read, different key types are used depending on
--  the return type. The names of the keys are closely related to the
--  element names in gir files (see Gir_Reader.Keys).

pragma Ada_2022;

with Ada.Strings.Text_Buffers;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;

with Gettexts;

private with Ada.Containers.Indefinite_Holders;

package Gir_Reader is

   subtype Utf8 is Ada.Strings.UTF_Encoding.UTF_8_String;

   -----------------------------------------
   --  Internal string type               --
   --  In case of implementation changes  --
   -----------------------------------------

   --  The Text type is used for all strings in the library.
   subtype Text is Ada.Strings.Unbounded.Unbounded_String;
   Empty_Text renames Ada.Strings.Unbounded.Null_Unbounded_String;

   --  The usual function to convert from Ada String to Text.
   --  @param Source An Ada String.
   --  @return The String converted to Text type.
   function "+" (Source : String) return Text
   renames Ada.Strings.Unbounded.To_Unbounded_String;

   --  The usual function to convert from Text to Ada String.
   --  @param Source A Text type string.
   --  @return Source converted to an Ada String type.
   function "+" (Source : Text) return String
   renames Ada.Strings.Unbounded.To_String;

   -----------------------------
   --  Translation functions  --
   -----------------------------

   --  Mark strings for translation.
   --  @param Message An Ada string to translate.
   --  @return The translated string.
   --  function "-" (Message : String) return Utf8 renames Gettexts.Get_Text;
   function "-" (Message : String) return Utf8
   is (Gettexts.Get_Text_With_Domain
         (Domain_Name => "gir_reader", Message_Id => Message));

   -------------------------
   --  Logging functions  --
   -------------------------

   --  Used in code to log runtime informations.
   --  @param Message String information to output.
   procedure Log_Info (Message : String) renames Ada.Text_IO.Put_Line;

   --  Used in code to log runtime warnings.
   --  @param Message String warning to output.
   procedure Log_Warning (Message : String) renames Ada.Text_IO.Put_Line;

   --  Used in code to log runtime errors.
   --  @param Message String error to output.
   procedure Log_Error (Message : String) renames Ada.Text_IO.Put_Line;

   --------------------------------------------------------------------------
   --  Specific enumerations types:                                        --
   --  Parameter_Direction, Lifetime_Scope, Ownership and Signal_Emission  --
   --------------------------------------------------------------------------

   --  Enumeration type used for the direction annotation of parameters in
   --  gir files:
   --  @enum Is_In goes into the callable,
   --  @enum Is_Out for output parameters from the callable (reference in C++,
   --    var in Pascal, etc),
   --  @enum Is_In_Out for both (like a pre-allocated structure which will
   --    be filled-in by the callable).
   type Parameter_Direction is (Is_In, Is_Out, Is_In_Out)
   with Put_Image => Image;

   --  Converts Item to an UTF-8 encoded string.
   --  @param Item A Parameter_Direction.
   --  @return Item converted to an UTF-8 string.
   function To_String (Item : Parameter_Direction) return Utf8;

   --  Custom output of Parameter_Direction type enumeration.
   --  @param Output The buffer used for output.
   --  @param Item The enumeration key to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Parameter_Direction);

   --  Enumeration type used for the lifetime scope of callback parameters.
   --  @enum Notified Valid until a GDestroyNotify argument is called,
   --  @enum Async Only valid for the duration of the first callback invocation
   --    (can only be called once),
   --  @enum Call Only valid for the duration of the call, can be called
   --    multiple times during the call,
   --  @enum Forever Valid until the process terminates.
   type Lifetime_Scope is (Notified, Async, Call, Forever)
   with Put_Image => Image;

   --  Converts Item to an UTF-8 encoded string.
   --  @param Item A Lifetime_Scope.
   --  @return Item converted to an UTF-8 string.
   function To_String (Item : Lifetime_Scope) return Utf8;

   --  Custom output of Lifetime_Scope type enumeration.
   --  @param Output The buffer used for output.
   --  @param Item The enumeration key to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Lifetime_Scope);

   --  Enumeration type used for the transfer of ownership, with for
   --  example, a returned value.
   --  @enum None If the recipient does not own the value,
   --  @enum Container If the recipient owns the container but not the value
   --    (for arrays or lists for example),
   --  @enum Full The recipient owns the entire value.
   --  For details, see
   --  https://gi.readthedocs.io/en/latest/annotations/giannotations.html#memo
   --  ry-and-lifecycle-management
   type Ownership is (None, Container, Full) with Put_Image => Image;

   --  Converts Item to an UTF-8 encoded string.
   --  @param Item An Ownership.
   --  @return Item converted to an UTF-8 string.
   function To_String (Item : Ownership) return Utf8;

   --  Custom output of Ownership type enumeration.
   --  @param Output The buffer used for output.
   --  @param Item The enumeration key to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Ownership);

   --  Enumeration type for glib signal attributes.
   --  When to run the signal during the 5 steps of signal emission
   --  (https://docs.gtk.org/gobject/concepts.html#signal-emission and
   --  https://docs.gtk.org/gobject/flags.SignalFlags.html).
   --  @enum First Invoke the object method handler in the first emission
   --    stage.
   --  @enum Last Invoke the object method handler in the third emission stage.
   --  @enum Cleanup Invoke the object method handler in the last emission
   --    stage.
   type Signal_Emission is (First, Last, Cleanup) with Put_Image => Image;

   --  Converts Item to an UTF-8 encoded string.
   --  @param Item A Signal_Emission.
   --  @return Item converted to an UTF-8 string.
   function To_String (Item : Signal_Emission) return Utf8;

   --  Custom output of Signal_Emission type enumeration.
   --  @param Output The buffer used for output.
   --  @param Item The enumeration key to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Signal_Emission);

   ------------------------
   --  Exceptions types  --
   ------------------------

   --  Exception used in contracts if the key used in getter functions
   --  is missing. Users shall use Gir_Reader.Elements.Contains or
   --  Gir_Reader.Elements.Get_Keys to avoid this exception.
   Key_Error : exception;

private

   --  Indefinite_Holders are used to hide implementation details to
   --  child packages as we don't want to guarantee the stability of those.

   --  @private The 'Holder_Content_Root' type is used to instanciate the
   --  'Holders' package.
   type Holder_Content_Root is interface;

   --  @private Internal use only.
   package Holders is new
     Ada.Containers.Indefinite_Holders
       (Element_Type => Holder_Content_Root'Class);

end Gir_Reader;
