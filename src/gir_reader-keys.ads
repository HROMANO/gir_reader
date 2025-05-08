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

with Gir_Reader.Key_Types;

package Gir_Reader.Keys is

   use Gir_Reader.Key_Types;

   -----------------------------------------------
   --  List of Boolean keys found in GIR files. --
   -----------------------------------------------

   --  For class elements:
   --  True if a class is abstract.
   --  Is_Abstract : constant Boolean_Key := Create (-"");
   Is_Abstract : constant Boolean_Key := Create (-"is abstract");

   --  For glib signal elements:
   --  True if a signal can be freely emitted on alive objects from user code
   --  (https://docs.gtk.org/gobject/flags.SignalFlags.html).
   Is_Action : constant Boolean_Key := Create (-"is action");

   --  For parameter, instance parameter and return value elements:
   --  Deprecated. Replaced by nullable and optional.
   Is_Allow_None : constant Boolean_Key := Create (-"is allow none");

   --  For parameter, instance parameter and return value elements:
   --  True if the caller should allocate the parameter before calling
   --  the callable.
   Is_Caller_Allocates : constant Boolean_Key :=
     Create (-"is caller allocates");

   --  For property elements:
   --  True if the property will be set upon construction.
   Is_Construct : constant Boolean_Key := Create (-"is construct");

   --  For property elements:
   --  True if the property can only be set upon construction.
   Is_Construct_Only : constant Boolean_Key := Create (-"is construct only");

   --  For most elements:
   --  True if the element has been deprecated.
   Is_Deprecated : constant Boolean_Key := Create (-"is deprecated");

   --  For glib signal elements:
   --  True if the signal has a detailed parameter
   --  (https://docs.gtk.org/gobject/concepts.html#the-detail-argument and
   --  https://docs.gtk.org/gobject/flags.SignalFlags.html).
   Is_Detailed : constant Boolean_Key := Create (-"is detailed");

   --  For record elements:
   --  Deprecated. Binary attribute to tell if the record is disguised,
   --  i.e. whether the c:type is a typedef that doesn't look like a pointer,
   --  but is one internally. Its second meaning is "private" and is set when
   --  any typedef struct is parsed which doesn't also include a full struct
   --  with fields
   --  (https://gitlab.gnome.org/GNOME/gobject-introspection/issues/101).
   --  Replaced by "opaque" and "pointer".
   Is_Disguised : constant Boolean_Key := Create (-"is disguised");

   --  For class elements:
   --  Attribute to declare the class final or not (non-derivable class
   --  in a derivable hierarchy).
   Is_Final : constant Boolean_Key := Create (-"is final");

   --  For record elements:
   --  Attribute to tell if the record is foreign, that is it is not available
   --  in a g-i supported library.
   Is_Foreign : constant Boolean_Key := Create (-"is foreign");

   --  For class elements:
   --  Attribute to declare the class fundamental or not (top-level class
   --  which do not derives from any other type).
   Is_Glib_Fundamental : constant Boolean_Key :=
     Create (-"is glib fundamental");

   --  For most elements:
   --  False if the element is not introspectable. It doesn't exist in the
   --  bindings, due in general to missing information in the annotations
   --  in the original C code.
   Is_Introspectable : constant Boolean_Key := Create (-"is introspectable");

   --  For glib signal elements:
   --  True if no emission hooks are supported for this signal
   --  (https://docs.gtk.org/gobject/flags.SignalFlags.html).
   Is_No_Hooks : constant Boolean_Key := Create (-"is no hooks");

   --  For glib signal elements:
   --  True if signals emitted for an object while currently being in emission
   --  for this very object will not be emitted recursively, but instead cause
   --  the first emission to be restarted
   --  (https://docs.gtk.org/gobject/flags.SignalFlags.html).
   Is_No_Recurse : constant Boolean_Key := Create (-"is no recurse");

   --  For parameter, instance parameter and return value elements:
   --  True if the parameter can have a null value.
   Is_Nullable : constant Boolean_Key := Create (-"is nullable");

   --  For record elements:
   --  Attribute for a typedef struct that does not have a corresponding public
   --  structure definition.
   Is_Opaque : constant Boolean_Key := Create (-"is opaque");

   --  For parameter elements:
   --  True if the parameter is optional.
   Is_Optional : constant Boolean_Key := Create (-"is optional");

   --  For record elements:
   --  Attribute for a typedef struct pointer, e.g.
   --  typedef struct Foo* FooPtr.
   Is_Pointer : constant Boolean_Key := Create (-"is pointer");

   --  For field elements:
   --  True if the field is private to the structure or has public
   --  ("0") visibility.
   Is_Private : constant Boolean_Key := Create (-"is private");

   --  For property elements:
   --  True if the property is readable, that is it has a getter function.
   --
   --  For field elements:
   --  True if the field is readable.
   Is_Readable : constant Boolean_Key := Create (-"is readable");

   --  For parameter and return value elements:
   --  True if the parameter can be omitted from the introspected output.
   Is_Skip : constant Boolean_Key := Create (-"is skip");

   --  For callback elements:
   --  True if the callback can throw an error.
   --
   --  For callables (function, callback, closure... elements):
   --  True if the callable can throw an error.
   Is_Throws : constant Boolean_Key := Create (-"is throws");

   --  For property elements:
   --  True if the property is writeable, that is it has a setter function.
   --
   --  For field elements:
   --  True if the field is writeable.
   Is_Writable : constant Boolean_Key := Create (-"is writable");

   --  For array elements:
   --  True if the last element of the array is zero. For example,
   --  in an array of pointers, the last pointer would be NULL.
   Is_Zero_Terminated : constant Boolean_Key := Create (-"is zero terminated");

   -----------------------------------------------
   --  List of Integer keys found in GIR files. --
   -----------------------------------------------

   --  For field elements:
   --  Number of bits of the field.
   Bits : constant Integer_Key := Create (-"bits");

   --  For parameter and return value elements:
   --  The parameter is a user_data for callbacks. The value points to a
   --  different parameter that is the actual callback.
   Closure : constant Integer_Key := Create (-"closure");

   --  For parameter and return value elements:
   --  The parameter is a destroy_data for callbacks. The value points to a
   --  different parameter that is the actual callback.
   Destroy : constant Integer_Key := Create (-"destroy");

   --  For array elements:
   --  Size of an array of predetermined fixed size. For example a C array
   --  declared as char arr[5].
   Fixed_Size : constant Integer_Key := Create (-"fixed size");

   --  For array elements:
   --  0-based index of parameter element that specifies the length of the
   --  array.
   Length : constant Integer_Key := Create (-"length");

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
   Direction : constant Parameter_Direction_Key := Create (-"direction");

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
   Scope : constant Lifetime_Scope_Key := Create (-"scope");

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
   Transfer_Ownership : constant Ownership_Key :=
     Create (-"transfer ownership");

   -------------------------------------------------------
   --  List of Signal_When_Run keys found in GIR files. --
   -------------------------------------------------------

   --  For glib signal attributes:
   --  When to run the signal during the 5 steps of signal emission
   --  (https://docs.gtk.org/gobject/concepts.html#signal-emission and
   --  https://docs.gtk.org/gobject/flags.SignalFlags.html).
   Signal_When : constant Signal_Emission_Key := Create (-"when");

   --------------------------------------------
   --  List of Text keys found in GIR files. --
   --------------------------------------------

   --  For constant elements and callables (function, callback, closure...
   --  elements):
   --  Corresponding C identifier in the source code.
   --
   --  For member elements:
   --  Corresponding C type of the member.
   C_Identifier : constant Text_Key := Create (-"C identifier");

   --  For repository and namespace elements:
   --  Prefixes to filter out from C identifiers for data structures and types.
   --  For example, GtkWindow will be Window. If c:symbol-prefixes is not used,
   --  then this element is used for both.
   C_Identifier_Prefixes : constant Text_Key :=
     Create (-"C identifier prefixes");

   --  For namespace elements:
   --  Deprecated.
   --  The same as c:identifier-prefixes. Only used for backward compatibility.
   C_Prefix : constant Text_Key := Create (-"C prefix");

   --  For interface, class, glib boxed, record and union elements:
   --  Prefix to filter out from C functions. For example, gtk_window_new
   --  will lose gtk_.
   C_Symbol_Prefix : constant Text_Key := Create (-"C symbol prefix");

   --  For repository and namespace elements:
   --  Prefixes to filter out from C functions. For example, gtk_window_new
   --  will lose gtk_.
   C_Symbol_Prefixes : constant Text_Key := Create (-"C symbol prefixes");

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
   C_Type : constant Text_Key := Create (-"C type");

   --  For doc and source position elements:
   --  The first column of the documentation in the source code.
   Column : constant Text_Key := Create (-"column");

   --  For unions elements:
   --  Name of the used to copy the union.
   --
   --  For record elements:
   --  Name of the used to copy the record.
   Copy_Function : constant Text_Key := Create (-"copy function");

   --  For property elements:
   --  The default value of the property, as a string; if missing, the default
   --  value is zero for integer types, and null for pointer types.
   Default_Value : constant Text_Key := Create (-"default value");

   --  For most elements:
   --  Version number from which this element is deprecated.
   Deprecated_Version : constant Text_Key := Create (-"deprecated version");

   --  For glib signal elements:
   --  The emitter method for the signal.
   Emitter : constant Text_Key := Create (-"emitter");

   --  For doc elements:
   --  The file containing this documentation.
   --
   --  For source position elements:
   --  File name of the source of the documentation.
   Filename : constant Text_Key := Create (-"file name");

   --  For union elements:
   --  Name of the used to free the union.
   --
   --  For record elements:
   --  Name of the used to free the record.
   Free_Function : constant Text_Key := Create (-"free function");

   --  For property elements:
   --  The getter for this property.
   Getter : constant Text_Key := Create (-"getter");

   --  For callables (functions, callbacks, closures... elements):
   --  The name of the asynchronous version of this callable.
   Glib_Async_Func : constant Text_Key := Create (-"glib async function");

   --  For enumeration elements:
   --  Error domain of this enumeration in a stringified form.
   Glib_Error_Domain : constant Text_Key := Create (-"glib error domain");

   --  For callables (functions, callbacks, closures... elements):
   --  The name of the callable which finishes the asynchronuos operation
   --  of this function.
   Glib_Finish_Func : constant Text_Key := Create (-"glib finish function");

   --  For method elements:
   --  The GObject property that is retrieved by this method.
   Glib_Get_Property : constant Text_Key := Create (-"glib get property");

   --  For interface elements:
   --  to get the GObject compatible type of the interface.
   --
   --  For class elements:
   --  to get the GObject compatible type of the class.
   --
   --  For glib boxed elements:
   --  to get the GObject compatible type of the boxed type.
   --
   --  For record elements:
   --  to get the GObject compatible type of the record.
   --
   --  For union, bitfield and enumeration elements:
   --  to retrieve the GObject compatible type of the element.
   Glib_Get_Type : constant Text_Key := Create (-"glib get type");

   --  For class elements:
   --  GObject compatible to get a value of a property of the class.
   Glib_Get_Value_Func : constant Text_Key :=
     Create (-"glib get value function");

   --  For record elements:
   --  Name of the GObject compatible gtype this record represents. If empty,
   --  this record will be hidden from generated public APIs.
   Glib_Is_Gtype_Struct_For : constant Text_Key :=
     Create (-"glib is gtype struct for");

   --  For glib boxed elements:
   --  GObject compatible type name of the boxed type.
   --
   --  For member elements:
   --  Name of the member (from GEnumValue/GFlagsValue).
   Glib_Name : constant Text_Key := Create (-"glib name");

   --  For member elements:
   --  Short nickname of the member (from GEnumValue/GFlagsValue).
   Glib_Nick : constant Text_Key := Create (-"glib nick");

   --  For class elements:
   --  GObject compatible to reference or increase the reference count
   --  of the class.
   Glib_Ref_Func : constant Text_Key := Create (-"glib ref function");

   --  For method elements:
   --  The GObject property that is set by this method.
   Glib_Set_Property : constant Text_Key := Create (-"glib set property");

   --  For class elements:
   --  GObject compatible to set a value of a property of the class.
   Glib_Set_Value_Func : constant Text_Key :=
     Create (-"glib set value function");

   --  For callables (functions, callbacks, closures... elements):
   --  The name of the synchronous version of this callable.
   Glib_Sync_Func : constant Text_Key := Create (-"glib sync function");

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
   Glib_Type_Name : constant Text_Key := Create (-"glib type name");

   --  For interface elements:
   --  GObject compatible C structure defining the Interface.
   --
   --  For class elements:
   --  GObject compatible C structure defining the class.
   Glib_Type_Struct : constant Text_Key := Create (-"glib type struct");

   --  For class elements:
   --  GObject compatible to unreference or decrease the reference
   --  count of the class.
   Glib_Unref_Func : constant Text_Key := Create (-"glib unref function");

   --  For virtual method elements:
   --  Name of the callable called when invoking this virtual method.
   Invoker : constant Text_Key := Create (-"invoker");

   --  For doc and source position elements:
   --  The first line of the documentation in the source code.
   Line : constant Text_Key := Create (-"line");

   --  For callables (function, callback, closure... elements):
   --  if for backward compatibility reason the callable has a name in the
   --  source code but should be known by another one, this attribute contains
   --  the new name.
   Moved_To : constant Text_Key := Create (-"moved to");

   --  For most elements:
   --  Name of the element.
   Name : constant Text_Key := Create (-"name");

   --  For class elements:
   --  Name of the parent class if any.
   Parent : constant Text_Key := Create (-"parent");

   --  For property elements:
   --  The setter for this property.
   Setter : constant Text_Key := Create (-"setter");

   --  For callables (function, callback, closure... elements):
   --  Callable it is shadowed by. For example, in C++, only one version of
   --  an overloaded callable will appear.
   Shadowed_By : constant Text_Key := Create (-"shadowed by");

   --  For callables (function, callback, closure... elements):
   --  Callable it shadows. For example, in C++, only one version of an
   --  overloaded callable will appear.
   Shadows : constant Text_Key := Create (-"shadows");

   --  For namespace elements:
   --  Path to the shared library implementing the namespace. It can be a
   --  comma-separated list, with relative path only.
   Shared_Library : constant Text_Key := Create (-"shared library");

   --  For most elements:
   --  Give the statibility status of the element. Can take the values
   --  "Stable", "Unstable" or "Private".
   Stability : constant Text_Key := Create (-"stability");

   --  For attribute elements:
   --  Value of the attribute.
   --
   --  For constant elements:
   --  Value of the constant.
   --
   --  For member elements:
   --  Value of the member.
   Value : constant Text_Key := Create (-"value");

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
   Version : constant Text_Key := Create (-"version");

   --  For doc version, doc stability, doc and doc deprecated elements:
   --  Preserve the original formatting of the documentation from the source
   --  code.
   Preserve_Xml_Space : constant Text_Key := Create (-"preserve XML space");

   --  For doc version and doc stability elements:
   --  Preserve the original formatting of the documentation from the source
   --  code. Recommended to use this instead of xml:space.
   --
   --  For doc and doc deprecated elements:
   --  Keep the whitespace as they were in the source code.
   Preserve_Xml_Whitespace : constant Text_Key :=
     Create (-"preserve XML white space");

   --  For repository elements:
   --  XML namespace.
   Xmlns : constant Text_Key := Create (-"XML namespace");

   --  For repository elements:
   --  XML C namespace.
   Xmlns_C : constant Text_Key := Create (-"XML C namespace");

   --  For repository elements:
   --  XML glib namespace.
   Xmlns_Glib : constant Text_Key := Create (-"XML glib namespace");

   --  For most elements:
   --  Content of the element (i.e. between XML start and end tags).
   Content : constant Text_Key := Create (-"content");

   -----------------------------------------------
   --  List of Element keys found in GIR files. --
   -----------------------------------------------

   --  Type's name substitution, representing a typedef in C.
   Alias : constant Element_Key := Create (-"alias");

   --  An array type of data where each element is of the same type.
   Array_Element : constant Element_Key := Create (-"array element");

   --  Element defining an annotation from the source code, usually a
   --  user-defined annotation associated to a parameter or a return value.
   Attribute : constant Element_Key := Create (-"attribute");

   --  Element defining a bit field (as in C).
   Bitfield : constant Element_Key := Create (-"bit field");

   --  Dependant C header file which should be included in C programs.
   C_Include : constant Element_Key := Create (-"C include");

   --  A callback closure, that is a called when a signal is emitted
   --  (as an answer to that signal).
   Callback : constant Element_Key := Create (-"callback");

   --  GObject inherited class definition.
   Class : constant Element_Key := Create (-"class");

   --  A constant entity, similar to const variable in C.
   Constant_Element : constant Element_Key := Create (-"constant element");

   --  A constructor of a class.
   Constructor : constant Element_Key := Create (-"constructor");

   --  Documentation of an element.
   Doc : constant Element_Key := Create (-"doc");

   --  Deprecated documentation of an element. Kept for historical reasons
   --  in general.
   Doc_Deprecated : constant Element_Key := Create (-"doc deprecated");

   --  Give the stability of the documentation.
   Doc_Stability : constant Element_Key := Create (-"doc stability");

   --  Version of the documentation.
   Doc_Version : constant Element_Key := Create (-"doc version");

   --  Element defining a gtk-doc documentation section.
   Doc_Section : constant Element_Key := Create (-"doc section");

   --  Element defining a enumeration type similar to enum in C/C++?
   Enumeration : constant Element_Key := Create (-"enumeration");

   --  A field of struct of union structure, that is a C bit field, that is a
   --  fixed length in bits variable.
   Field : constant Element_Key := Create (-"field");

   --  Element defining a standalone (as usual in most programming
   --  languages).
   Function_Element : constant Element_Key := Create (-"function");

   --  Element defining an inline function.
   Function_Inline : constant Element_Key := Create (-"inline function");

   --  Element defining a pre-processor macro that behaves like a function.
   --  Unlike functions, macros do not have a return value.
   Function_Macro : constant Element_Key := Create (-"macro function");

   --  Boxed type (wrapper to opaque C structures registered by the type
   --  system).
   Glib_Boxed : constant Element_Key := Create (-"glib boxed");

   --  A signal as defined in the GObject system
   --  (https://docs.gtk.org/gobject/concepts.html#signals).
   Glib_Signal : constant Element_Key := Create (-"glib signal");

   --  Give the name of the interface it implements. This element is generally
   --  used within a class element.
   Implements : constant Element_Key := Create (-"implements");

   --  Dependant namespace to include with the current namespace. For example,
   --  Gtk will need the namespace GLib.
   Include : constant Element_Key := Create (-"include");

   --  Instance parameter is a parameter of a C which is an instance
   --  of an existing object. So the callable is surely a method of a class,
   --  and this parameter points to the instance of the object. In C++, this
   --  would be equivalent to the pointer this which is not passed to the
   --  method, in Python it's equivalent to self.
   Instance_Parameter : constant Element_Key := Create (-"instance parameter");

   --  Abstract interface to other classes.
   Interface_Element : constant Element_Key := Create (-"interface");

   --  Element defining a member of a bit field or an enumeration.
   Member : constant Element_Key := Create (-"member");

   --  Element defining a method from a class.
   Method : constant Element_Key := Create (-"method");

   --  Element defining an inline method from a type.
   Method_Inline : constant Element_Key := Create (-"inline method");

   --  Namespace which maps metadata entries to C functionality. This a
   --  similar concept to namespace in C++, but for GObject-based C libraries.
   Namespace : constant Element_Key := Create (-"namespace");

   --  Deprecated: package name containing the library.
   Package_Element : constant Element_Key := Create (-"package");

   --  Parameter element of a list of parameters.
   Parameter : constant Element_Key := Create (-"parameter");

   --  Parameters element of a callable, that is in general parameters of a
   --  or similar.
   Parameters : constant Element_Key := Create (-"parameters");

   --  Interface which is pre-required to implement another interface. This
   --  node is generally using within an interface element.
   Prerequisite : constant Element_Key := Create (-"prerequisite");

   --  Property, that is a variable or members with getter and setter
   --  functions.
   Property : constant Element_Key := Create (-"property");

   --  Record definition, equivalent to a C struct, that is a simple structure,
   --  not a class.
   Record_Element : constant Element_Key := Create (-"record");

   --  Root node of a GIR repository. It contains namespaces, which can in turn
   --  be implemented in several libraries.
   Repository : constant Element_Key := Create (-"repository");

   --  Return value of a callable.
   Return_Value : constant Element_Key := Create (-"retrun value");

   --  Position of the documentation in the original source code.
   Source_Position : constant Element_Key := Create (-"source position");

   --  A simple type of data (as opposed to an array).
   Type_Element : constant Element_Key := Create (-"type");

   --  Element defining a type of data being a union of type, similar to union
   --  in C/C++ but extended with fields and methods.
   Union : constant Element_Key := Create (-"union");

   --  An element usually found in a parameter element for variadic parameter
   --  in a or callable.
   Varargs : constant Element_Key := Create (-"variadic arguments");

   --  Element defining a virtual method from a class, concept similar to C++.
   Virtual_Method : constant Element_Key := Create (-"virtual method");

end Gir_Reader.Keys;
