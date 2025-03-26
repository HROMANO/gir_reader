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

private with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Text_Buffers;

package Gir_Reader.Keys is

   --  'Key' is the root type of all key types.
   type Key (<>) is abstract tagged private;

   --  'Less_Than' is needed to use keys for ordered maps.
   --  @param Left The left key to compare.
   --  @param Right The right key to compare.
   function Less_Than (Left, Right : Key'Class) return Boolean;

   --  Procedure to display a key.
   --  @param Output The buffer used for output.
   --  @param Item The 'Key' to display.
   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Key);

   --  Root type for attribute keys.
   type Attribute_Key is abstract new Key with private;

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

   -----------------------------------------------
   --  List of Boolean keys found in GIR files. --
   -----------------------------------------------

   --  For class elements:
   --  True if a class is abstract.
   function Is_Abstract return Boolean_Key;

   --  For glib signal elements:
   --  True if a signal can be freely emitted on alive objects from user code
   --  (https://docs.gtk.org/gobject/flags.SignalFlags.html).
   function Is_Action return Boolean_Key;

   --  For parameter, instance parameter and return value elements:
   --  Deprecated. Replaced by nullable and optional.
   function Is_Allow_None return Boolean_Key;

   --  For parameter, instance parameter and return value elements:
   --  True if the caller should allocate the parameter before calling
   --  the callable.
   function Is_Caller_Allocates return Boolean_Key;

   --  For property elements:
   --  True if the property will be set upon construction.
   function Is_Construct return Boolean_Key;

   --  For property elements:
   --  True if the property can only be set upon construction.
   function Is_Construct_Only return Boolean_Key;

   --  For most elements:
   --  True if the element has been deprecated.
   function Is_Deprecated return Boolean_Key;

   --  For glib signal elements:
   --  True if the signal has a detailed parameter
   --  (https://docs.gtk.org/gobject/concepts.html#the-detail-argument and
   --  https://docs.gtk.org/gobject/flags.SignalFlags.html).
   function Is_Detailed return Boolean_Key;

   --  For record elements:
   --  Deprecated. Binary attribute to tell if the record is disguised,
   --  i.e. whether the c:type is a typedef that doesn't look like a pointer,
   --  but is one internally. Its second meaning is "private" and is set when
   --  any typedef struct is parsed which doesn't also include a full struct
   --  with fields
   --  (https://gitlab.gnome.org/GNOME/gobject-introspection/issues/101).
   --  Replaced by "opaque" and "pointer".
   function Is_Disguised return Boolean_Key;

   --  For class elements:
   --  Attribute to declare the class final or not (non-derivable class
   --  in a derivable hierarchy).
   function Is_Final return Boolean_Key;

   --  For record elements:
   --  Attribute to tell if the record is foreign, that is it is not available
   --  in a g-i supported library.
   function Is_Foreign return Boolean_Key;

   --  For class elements:
   --  Attribute to declare the class fundamental or not (top-level class
   --  which do not derives from any other type).
   function Is_Glib_Fundamental return Boolean_Key;

   --  For most elements:
   --  False if the element is not introspectable. It doesn't exist in the
   --  bindings, due in general to missing information in the annotations
   --  in the original C code.
   function Is_Introspectable return Boolean_Key;

   --  For glib signal elements:
   --  True if no emission hooks are supported for this signal
   --  (https://docs.gtk.org/gobject/flags.SignalFlags.html).
   function Is_No_Hooks return Boolean_Key;

   --  For glib signal elements:
   --  True if signals emitted for an object while currently being in emission
   --  for this very object will not be emitted recursively, but instead cause
   --  the first emission to be restarted
   --  (https://docs.gtk.org/gobject/flags.SignalFlags.html).
   function Is_No_Recurse return Boolean_Key;

   --  For parameter, instance parameter and return value elements:
   --  True if the parameter can have a null value.
   function Is_Nullable return Boolean_Key;

   --  For record elements:
   --  Attribute for a typedef struct that does not have a corresponding public
   --  structure definition.
   function Is_Opaque return Boolean_Key;

   --  For parameter elements:
   --  True if the parameter is optional.
   function Is_Optional return Boolean_Key;

   --  For record elements:
   --  Attribute for a typedef struct pointer, e.g.
   --  typedef struct Foo* FooPtr.
   function Is_Pointer return Boolean_Key;

   --  For field elements:
   --  True if the field is private to the structure or has public
   --  ("0") visibility.
   function Is_Private return Boolean_Key;

   --  For property elements:
   --  True if the property is readable, that is it has a getter function.
   --
   --  For field elements:
   --  True if the field is readable.
   function Is_Readable return Boolean_Key;

   --  For parameter and return value elements:
   --  True if the parameter can be omitted from the introspected output.
   function Is_Skip return Boolean_Key;

   --  For callback elements:
   --  True if the callback can throw an error.
   --
   --  For callables (function, callback, closure... elements):
   --  True if the callable can throw an error.
   function Is_Throws return Boolean_Key;

   --  For property elements:
   --  True if the property is writeable, that is it has a setter function.
   --
   --  For field elements:
   --  True if the field is writeable.
   function Is_Writable return Boolean_Key;

   --  For array elements:
   --  True if the last element of the array is zero. For example,
   --  in an array of pointers, the last pointer would be NULL.
   function Is_Zero_Terminated return Boolean_Key;

   -----------------------------------------------
   --  List of Integer keys found in GIR files. --
   -----------------------------------------------

   --  For field elements:
   --  Number of bits of the field.
   function Bits return Integer_Key;

   --  For parameter and return value elements:
   --  The parameter is a user_data for callbacks. The value points to a
   --  different parameter that is the actual callback.
   function Closure return Integer_Key;

   --  For parameter and return value elements:
   --  The parameter is a destroy_data for callbacks. The value points to a
   --  different parameter that is the actual callback.
   function Destroy return Integer_Key;

   --  For array elements:
   --  Size of an array of predetermined fixed size. For example a C array
   --  declared as char arr[5].
   function Fixed_Size return Integer_Key;

   --  For array elements:
   --  0-based index of parameter element that specifies the length of the
   --  array.
   function Length return Integer_Key;

   -----------------------------------------------------------
   --  List of Parameter_Direction keys found in GIR files. --
   -----------------------------------------------------------

   --  For parameter and instance parameter elements:
   --  Direction of the parameter.
   --  - "in" goes into the callable,
   --  - "out" for output parameters from the callable (reference in C++,
   --     var in Pascal, etc),
   --  - "inout" for both (like a pre-allocated structure which will
   --     be filled-in by the callable).
   function Direction return Parameter_Direction_Key;

   ------------------------------------------------------
   --  List of Lifetime_Scope keys found in GIR files. --
   ------------------------------------------------------

   --  For parameter and instance parameter elements:
   --  The parameter is a callback, the value indicates the lifetime of the
   --  call. For language bindings which want to know when the resources
   --  required to do the call can be freed.
   --  - "notified" valid until a GDestroyNotify argument is called,
   --  - "async" only valid for the duration of the first callback invocation
   --    (can only be called once),
   --  - "call" only valid for the duration of the call, can be called multiple
   --     times during the call,
   --  - "forever" valid until the process terminates.
   function Scope return Lifetime_Scope_Key;

   -----------------------------------------------
   --  List of Ownership keys found in GIR files. --
   -----------------------------------------------

   --  Attribute used by many elements for the transfer of ownership, with for
   --  example, a returned value.
   --  - "none" if the recipient does not own the value,
   --  - "container" if the recipient owns the container but not the value
   --  (for arrays or lists for example),
   --  - "full" the recipient owns the entire value.
   --  For details, see
   --  https://gi.readthedocs.io/en/latest/annotations/giannotations.html#memory-and-lifecycle-management
   function Transfer_Ownership return Ownership_Key;

   -------------------------------------------------------
   --  List of Signal_When_Run keys found in GIR files. --
   -------------------------------------------------------

   --  For glib signal attributes:
   --  When to run the signal during the 5 steps of signal emission
   --  (https://docs.gtk.org/gobject/concepts.html#signal-emission and
   --  https://docs.gtk.org/gobject/flags.SignalFlags.html).
   function Signal_When return Signal_Emission_Key;

   --------------------------------------------
   --  List of Text keys found in GIR files. --
   --------------------------------------------

   --  For constant elements and callables (function, callback, closure...
   --  elements):
   --  Corresponding C identifier in the source code.
   --
   --  For member elements:
   --  Corresponding C type of the member.
   function C_Identifier return Text_Key;

   --  For repository and namespace elements:
   --  Prefixes to filter out from C identifiers for data structures and types.
   --  For example, GtkWindow will be Window. If c:symbol-prefixes is not used,
   --  then this element is used for both.
   function C_Identifier_Prefixes return Text_Key;

   --  For namespace elements:
   --  Deprecated.
   --  The same as c:identifier-prefixes. Only used for backward compatibility.
   function C_Prefix return Text_Key;

   --  For interface, class, glib boxed, record and union elements:
   --  Prefix to filter out from C functions. For example, gtk_window_new
   --  will lose gtk_.
   function C_Symbol_Prefix return Text_Key;

   --  For repository and namespace elements:
   --  Prefixes to filter out from C functions. For example, gtk_window_new
   --  will lose gtk_.
   function C_Symbol_Prefixes return Text_Key;

   --  For class elements:
   --  C type of the class.
   --
   --  For record elements:
   --  Corresponding C type of the record.
   --
   --  For constant elements:
   --  Corresponding C type of the constant in C.
   --
   --  For callback elements:
   --  The C type returned by the callback closure (i.e. function).
   --
   --  For type elements:
   --  the C representation of the type.
   --
   --  For array elements:
   --  The C representation of the array type.
   --
   --  For union elements:
   --  C type defining the union.
   --
   --  For bitfield elements:
   --  Corresponding C type of the bit field type.
   --
   --  For enumeration elements:
   --  Corresponding C type of the enumeration type.
   --
   --  For alias elements:
   --  The corresponding C type's name.
   --
   --  For interface elements:
   --  Corresponding C type.
   function C_Type return Text_Key;

   --  For doc and source position elements:
   --  The first column of the documentation in the source code.
   function Column return Text_Key;

   --  For unions elements:
   --  Name of the function used to copy the union.
   --
   --  For record elements:
   --  Name of the function used to copy the record.
   function Copy_Function return Text_Key;

   --  For property elements:
   --  The default value of the property, as a string; if missing, the default
   --  value is zero for integer types, and null for pointer types.
   function Default_Value return Text_Key;

   --  For most elements:
   --  Version number from which this element is deprecated.
   function Deprecated_Version return Text_Key;

   --  For glib signal elements:
   --  The emitter method for the signal.
   function Emitter return Text_Key;

   --  For doc elements:
   --  The file containing this documentation.
   --
   --  For source position elements:
   --  File name of the source of the documentation.
   function Filename return Text_Key;

   --  For union elements:
   --  Name of the function used to free the union.
   --
   --  For record elements:
   --  Name of the function used to free the record.
   function Free_Function return Text_Key;

   --  For property elements:
   --  The getter function for this property.
   function Getter return Text_Key;

   --  For callables (functions, callbacks, closures... elements):
   --  The name of the asynchronous version of this callable.
   function Glib_Async_Func return Text_Key;

   --  For enumeration elements:
   --  Error domain of this enumeration in a stringified form.
   function Glib_Error_Domain return Text_Key;

   --  For callables (functions, callbacks, closures... elements):
   --  The name of the callable which finishes the asynchronuos operation
   --  of this function.
   function Glib_Finish_Func return Text_Key;

   --  For method elements:
   --  The GObject property that is retrieved by this method.
   function Glib_Get_Property return Text_Key;

   --  For interface elements:
   --  Function to get the GObject compatible type of the interface.
   --
   --  For class elements:
   --  Function to get the GObject compatible type of the class.
   --
   --  For glib boxed elements:
   --  Function to get the GObject compatible type of the boxed type.
   --
   --  For record elements:
   --  Function to get the GObject compatible type of the record.
   --
   --  For union, bitfield and enumeration elements:
   --  Function to retrieve the GObject compatible type of the element.
   function Glib_Get_Type return Text_Key;

   --  For class elements:
   --  GObject compatible function to get a value of a property of the class.
   function Glib_Get_Value_Func return Text_Key;

   --  For record elements:
   --  Name of the GObject compatible gtype this record represents. If empty,
   --  this record will be hidden from generated public APIs.
   function Glib_Is_Gtype_Struct_For return Text_Key;

   --  For glib boxed elements:
   --  GObject compatible type name of the boxed type.
   --
   --  For member elements:
   --  Name of the member (from GEnumValue/GFlagsValue).
   function Glib_Name return Text_Key;

   --  For member elements:
   --  Short nickname of the member (from GEnumValue/GFlagsValue).
   function Glib_Nick return Text_Key;

   --  For class elements:
   --  GObject compatible function to reference or increase the reference count
   --  of the class.
   function Glib_Ref_Func return Text_Key;

   --  For method elements:
   --  The GObject property that is set by this method.
   function Glib_Set_Property return Text_Key;

   --  For class elements:
   --  GObject compatible function to set a value of a property of the class.
   function Glib_Set_Value_Func return Text_Key;

   --  For callables (functions, callbacks, closures... elements):
   --  The name of the synchronous version of this callable.
   function Glib_Sync_Func return Text_Key;

   --  For interface elements:
   --  Type name compatible with the GObject type system.
   --
   --  For class elements:
   --  GObject compatible type name of the class.
   --
   --  For glib boxed elements:
   --  GObject compatible type name of the boxed type.
   --
   --  For record elements:
   --  GObject compatible C type of the record.
   --
   --  For union, bitfield and enumeration elements:
   --  GObject compatible type name.
   function Glib_Type_Name return Text_Key;

   --  For interface elements:
   --  GObject compatible C structure defining the Interface.
   --
   --  For class elements:
   --  GObject compatible C structure defining the class.
   function Glib_Type_Struct return Text_Key;

   --  For class elements:
   --  GObject compatible function to unreference or decrease the reference
   --  count of the class.
   function Glib_Unref_Func return Text_Key;

   --  For virtual method elements:
   --  Name of the callable called when invoking this virtual method.
   function Invoker return Text_Key;

   --  For doc and source position elements:
   --  The first line of the documentation in the source code.
   function Line return Text_Key;

   --  For callables (function, callback, closure... elements):
   --  if for backward compatibility reason the callable has a name in the
   --  source code but should be known by another one, this attribute contains
   --  the new name.
   function Moved_To return Text_Key;

   --  For most elements:
   --  Name of the element.
   function Name return Text_Key;

   --  For class elements:
   --  Name of the parent class if any.
   function Parent return Text_Key;

   --  For property elements:
   --  The setter function for this property.
   function Setter return Text_Key;

   --  For callables (function, callback, closure... elements):
   --  Callable it is shadowed by. For example, in C++, only one version of
   --  an overloaded callable will appear.
   function Shadowed_By return Text_Key;

   --  For callables (function, callback, closure... elements):
   --  Callable it shadows. For example, in C++, only one version of an
   --  overloaded callable will appear.
   function Shadows return Text_Key;

   --  For namespace elements:
   --  Path to the shared library implementing the namespace. It can be a
   --  comma-separated list, with relative path only.
   function Shared_Library return Text_Key;

   --  For most elements:
   --  Give the statibility status of the element. Can take the values
   --  "Stable", "Unstable" or "Private".
   function Stability return Text_Key;

   --  For attribute elements:
   --  Value of the attribute.
   --
   --  For constant elements:
   --  Value of the constant.
   --
   --  For member elements:
   --  Value of the member.
   function Value return Text_Key;

   --  For repository elements:
   --  Version number of the repository.
   --
   --  For namespace elements:
   --  Version number of the namespace.
   --
   --  For include elements:
   --  Version of the dependant namespace to use.
   --
   --  For most other elements:
   --  Version number of an element.
   function Version return Text_Key;

   --  For doc version, doc stability, doc and doc deprecated elements:
   --  Preserve the original formatting of the documentation from the source
   --  code.
   function Preserve_Xml_Space return Text_Key;

   --  For doc version and doc stability elements:
   --  Preserve the original formatting of the documentation from the source
   --  code. Recommended to use this instead of xml:space.
   --
   --  For doc and doc deprecated elements:
   --  Keep the whitespace as they were in the source code.
   function Preserve_Xml_Whitespace return Text_Key;

   --  For repository elements:
   --  XML namespace.
   function Xmlns return Text_Key;

   --  For repository elements:
   --  XML C namespace.
   function Xmlns_C return Text_Key;

   --  For repository elements:
   --  XML glib namespace.
   function Xmlns_Glib return Text_Key;

   --  For most elements:
   --  Content of the element (i.e. between XML start and end tags).
   function Content return Text_Key;

   -----------------------------------------------
   --  List of Element keys found in GIR files. --
   -----------------------------------------------

   --  Type's name substitution, representing a typedef in C.
   function Alias return Element_Key;

   --  An array type of data where each element is of the same type.
   function Array_Element return Element_Key;

   --  Element defining an annotation from the source code, usually a
   --  user-defined annotation associated to a parameter or a return value.
   function Attribute return Element_Key;

   --  Element defining a bit field (as in C).
   function Bitfield return Element_Key;

   --  Dependant C header file which should be included in C programs.
   function C_Include return Element_Key;

   --  A callback closure, that is a function called when a signal is emitted
   --  (as an answer to that signal).
   function Callback return Element_Key;

   --  GObject inherited class definition.
   function Class return Element_Key;

   --  A constant entity, similar to const variable in C.
   function Constant_Element return Element_Key;

   --  A constructor of a class.
   function Constructor return Element_Key;

   --  Documentation of an element.
   function Doc return Element_Key;

   --  Deprecated documentation of an element. Kept for historical reasons
   --  in general.
   function Doc_Deprecated return Element_Key;

   --  Give the stability of the documentation.
   function Doc_Stability return Element_Key;

   --  Version of the documentation.
   function Doc_Version return Element_Key;

   --  Element defining a gtk-doc documentation section.
   function Doc_Section return Element_Key;

   --  Element defining a enumeration type similar to enum in C/C++?
   function Enumeration return Element_Key;

   --  A field of struct of union structure, that is a C bit field, that is a
   --  fixed length in bits variable.
   function Field return Element_Key;

   --  Element defining a standalone function (as usual in most programming
   --  languages).
   function Function_Element return Element_Key;

   --  Element defining an inline function.
   function Function_Inline return Element_Key;

   --  Element defining a pre-processor macro that behaves like a function.
   --  Unlike functions, function macros do not have a return value.
   function Function_Macro return Element_Key;

   --  Boxed type (wrapper to opaque C structures registered by the type
   --  system).
   function Glib_Boxed return Element_Key;

   --  A signal as defined in the GObject system
   --  (https://docs.gtk.org/gobject/concepts.html#signals).
   function Glib_Signal return Element_Key;

   --  Give the name of the interface it implements. This element is generally
   --  used within a class element.
   function Implements return Element_Key;

   --  Dependant namespace to include with the current namespace. For example,
   --  Gtk will need the namespace GLib.
   function Include return Element_Key;

   --  Instance parameter is a parameter of a C function which is an instance
   --  of an existing object. So the callable is surely a method of a class,
   --  and this parameter points to the instance of the object. In C++, this
   --  would be equivalent to the pointer this which is not passed to the
   --  method, in Python it's equivalent to self.
   function Instance_Parameter return Element_Key;

   --  Abstract interface to other classes.
   function Interface_Element return Element_Key;

   --  Element defining a member of a bit field or an enumeration.
   function Member return Element_Key;

   --  Element defining a method from a class.
   function Method return Element_Key;

   --  Element defining an inline method from a type.
   function Method_Inline return Element_Key;

   --  Namespace which maps metadata entries to C functionality. This a
   --  similar concept to namespace in C++, but for GObject-based C libraries.
   function Namespace return Element_Key;

   --  Deprecated: package name containing the library.
   function Package_Element return Element_Key;

   --  Parameter element of a list of parameters.
   function Parameter return Element_Key;

   --  Parameters element of a callable, that is in general parameters of a
   --  function or similar.
   function Parameters return Element_Key;

   --  Interface which is pre-required to implement another interface. This
   --  node is generally using within an interface element.
   function Prerequisite return Element_Key;

   --  Property, that is a variable or members with getter and setter
   --  functions.
   function Property return Element_Key;

   --  Record definition, equivalent to a C struct, that is a simple structure,
   --  not a class.
   function Record_Element return Element_Key;

   --  Root node of a GIR repository. It contains namespaces, which can in turn
   --  be implemented in several libraries.
   function Repository return Element_Key;

   --  Return value of a callable.
   function Return_Value return Element_Key;

   --  Position of the documentation in the original source code.
   function Source_Position return Element_Key;

   --  A simple type of data (as opposed to an array).
   function Type_Element return Element_Key;

   --  Element defining a type of data being a union of type, similar to union
   --  in C/C++ but extended with fields and methods.
   function Union return Element_Key;

   --  An element usually found in a parameter element for variadic parameter
   --  in a function or callable.
   function Varargs return Element_Key;

   --  Element defining a virtual method from a class, concept similar to C++.
   function Virtual_Method return Element_Key;

private

   --  Indefinite_Holders are used here to hide implementation details to
   --  child packages as we don't want to guarantee the stability of those.

   --  @private The 'Root' type is used to instanciate the 'Holders' package.
   type Root is abstract tagged null record;

   --  @private Internal use only.
   package Holders is new
     Ada.Containers.Indefinite_Holders (Element_Type => Root'Class);

   --  The 'Key' type uses a Holder to hide the real type used in the body
   --  of the package.
   --  A 'Put_Image' aspect is used to have a readable and translatable display
   --  output.
   type Key is new Root with record
      Internal : Holders.Holder;
   end record
   with Put_Image => Image;

   type Attribute_Key is abstract new Key with null record;

   type Boolean_Key is new Attribute_Key with null record;
   type Integer_Key is new Attribute_Key with null record;
   type Parameter_Direction_Key is new Attribute_Key with null record;
   type Lifetime_Scope_Key is new Attribute_Key with null record;
   type Ownership_Key is new Attribute_Key with null record;
   type Signal_Emission_Key is new Attribute_Key with null record;
   type Text_Key is new Attribute_Key with null record;

   type Element_Key is new Key with null record;

end Gir_Reader.Keys;
