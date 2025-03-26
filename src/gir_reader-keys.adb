package body Gir_Reader.Keys is

   type Real_Key is new Root with record
      Name : Text;
   end record;

   ---------------
   -- Less_Than --
   ---------------

   function Less_Than (Left, Right : Key'Class) return Boolean
   is (Real_Key (Left.Internal.Element).Name
       < Real_Key (Right.Internal.Element).Name);

   -----------
   -- Image --
   -----------

   procedure Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item   : Key) is
   begin
      if Item.Internal.Is_Empty then
         Log_Error (-"No key. This is a bug in the library.");
      end if;

      Output.Put (+Real_Key (Item.Internal.Element).Name);
   end Image;

   ----------------------
   --  Generate_Holder --
   ----------------------

   --  Generates the holder of the real key.
   function Generate_Holder (Long_Name : Text) return Holders.Holder is
      K : constant Real_Key := (Name => Long_Name);
      H : Holders.Holder;
   begin
      H.Replace_Element (K);
      return H;
   end Generate_Holder;

   ------------------
   -- Boolean keys --
   ------------------

   function Is_Abstract return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is abstract"))));

   function Is_Action return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is action"))));

   function Is_Allow_None return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is allow none"))));

   function Is_Caller_Allocates return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is caller allocates"))));

   function Is_Construct return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is construct"))));

   function Is_Construct_Only return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is construct only"))));

   function Is_Deprecated return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is deprecated"))));

   function Is_Detailed return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is detailed"))));

   function Is_Disguised return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is disguised"))));

   function Is_Final return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is final"))));

   function Is_Foreign return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is foreign"))));

   function Is_Glib_Fundamental return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is glib fundamental"))));

   function Is_Introspectable return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is introspectable"))));

   function Is_No_Hooks return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is no hooks"))));

   function Is_No_Recurse return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is no recurse"))));

   function Is_Nullable return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is nullable"))));

   function Is_Opaque return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is opaque"))));

   function Is_Optional return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is optional"))));

   function Is_Pointer return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is pointer"))));

   function Is_Private return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is private"))));

   function Is_Readable return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is readable"))));

   function Is_Skip return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is skip"))));

   function Is_Throws return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is throws"))));

   function Is_Writable return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is writable"))));

   function Is_Zero_Terminated return Boolean_Key
   is ((Internal => Generate_Holder (+(-"is zero terminated"))));

   ------------------
   -- Integer keys --
   ------------------

   function Bits return Integer_Key
   is ((Internal => Generate_Holder (+(-"bits"))));

   function Closure return Integer_Key
   is ((Internal => Generate_Holder (+(-"closure"))));

   function Destroy return Integer_Key
   is ((Internal => Generate_Holder (+(-"destroy"))));

   function Fixed_Size return Integer_Key
   is ((Internal => Generate_Holder (+(-"fixed size"))));

   function Length return Integer_Key
   is ((Internal => Generate_Holder (+(-"length"))));

   ------------------------------
   -- Parameter_Direction keys --
   ------------------------------

   function Direction return Parameter_Direction_Key
   is ((Internal => Generate_Holder (+(-"direction"))));

   -------------------------
   -- Lifetime_Scope keys --
   -------------------------

   function Scope return Lifetime_Scope_Key
   is ((Internal => Generate_Holder (+(-"scope"))));

   --------------------
   -- Ownership keys --
   --------------------

   function Transfer_Ownership return Ownership_Key
   is ((Internal => Generate_Holder (+(-"transfer ownership"))));

   --------------------------
   -- Signal_Emission keys --
   --------------------------

   function Signal_When return Signal_Emission_Key
   is ((Internal => Generate_Holder (+(-"when"))));

   ----------------
   --  Text keys --
   ----------------

   function C_Identifier return Text_Key
   is ((Internal => Generate_Holder (+(-"C identifier"))));

   function C_Identifier_Prefixes return Text_Key
   is ((Internal => Generate_Holder (+(-"C identifier prefixes"))));

   function C_Prefix return Text_Key
   is ((Internal => Generate_Holder (+(-"C prefix"))));

   function C_Symbol_Prefix return Text_Key
   is ((Internal => Generate_Holder (+(-"C symbol prefix"))));

   function C_Symbol_Prefixes return Text_Key
   is ((Internal => Generate_Holder (+(-"C symbol prefixes"))));

   function C_Type return Text_Key
   is ((Internal => Generate_Holder (+(-"C type"))));

   function Column return Text_Key
   is ((Internal => Generate_Holder (+(-"column"))));

   function Copy_Function return Text_Key
   is ((Internal => Generate_Holder (+(-"copy function"))));

   function Default_Value return Text_Key
   is ((Internal => Generate_Holder (+(-"default value"))));

   function Deprecated_Version return Text_Key
   is ((Internal => Generate_Holder (+(-"deprecated version"))));

   function Emitter return Text_Key
   is ((Internal => Generate_Holder (+(-"emitter"))));

   function Filename return Text_Key
   is ((Internal => Generate_Holder (+(-"file name"))));

   function Free_Function return Text_Key
   is ((Internal => Generate_Holder (+(-"free function"))));

   function Getter return Text_Key
   is ((Internal => Generate_Holder (+(-"getter"))));

   function Glib_Async_Func return Text_Key
   is ((Internal => Generate_Holder (+(-"glib async function"))));

   function Glib_Error_Domain return Text_Key
   is ((Internal => Generate_Holder (+(-"glib error domain"))));

   function Glib_Finish_Func return Text_Key
   is ((Internal => Generate_Holder (+(-"glib finish function"))));

   function Glib_Get_Property return Text_Key
   is ((Internal => Generate_Holder (+(-"glib get property"))));

   function Glib_Get_Type return Text_Key
   is ((Internal => Generate_Holder (+(-"glib get type"))));

   function Glib_Get_Value_Func return Text_Key
   is ((Internal => Generate_Holder (+(-"glib get value function"))));

   function Glib_Is_Gtype_Struct_For return Text_Key
   is ((Internal => Generate_Holder (+(-"glib is gtype struct for"))));

   function Glib_Name return Text_Key
   is ((Internal => Generate_Holder (+(-"glib name"))));

   function Glib_Nick return Text_Key
   is ((Internal => Generate_Holder (+(-"glib nick"))));

   function Glib_Ref_Func return Text_Key
   is ((Internal => Generate_Holder (+(-"glib ref function"))));

   function Glib_Set_Property return Text_Key
   is ((Internal => Generate_Holder (+(-"glib set property"))));

   function Glib_Set_Value_Func return Text_Key
   is ((Internal => Generate_Holder (+(-"glib set value function"))));

   function Glib_Sync_Func return Text_Key
   is ((Internal => Generate_Holder (+(-"glib sync function"))));

   function Glib_Type_Name return Text_Key
   is ((Internal => Generate_Holder (+(-"glib type name"))));

   function Glib_Type_Struct return Text_Key
   is ((Internal => Generate_Holder (+(-"glib type struct"))));

   function Glib_Unref_Func return Text_Key
   is ((Internal => Generate_Holder (+(-"glib unref function"))));

   function Invoker return Text_Key
   is ((Internal => Generate_Holder (+(-"invoker"))));

   function Line return Text_Key
   is ((Internal => Generate_Holder (+(-"line"))));

   function Moved_To return Text_Key
   is ((Internal => Generate_Holder (+(-"moved to"))));

   function Name return Text_Key
   is ((Internal => Generate_Holder (+(-"name"))));

   function Parent return Text_Key
   is ((Internal => Generate_Holder (+(-"parent"))));

   function Setter return Text_Key
   is ((Internal => Generate_Holder (+(-"setter"))));

   function Shadowed_By return Text_Key
   is ((Internal => Generate_Holder (+(-"shadowed by"))));

   function Shadows return Text_Key
   is ((Internal => Generate_Holder (+(-"shadows"))));

   function Shared_Library return Text_Key
   is ((Internal => Generate_Holder (+(-"shared library"))));

   function Stability return Text_Key
   is ((Internal => Generate_Holder (+(-"stability"))));

   function Value return Text_Key
   is ((Internal => Generate_Holder (+(-"value"))));

   function Version return Text_Key
   is ((Internal => Generate_Holder (+(-"version"))));

   function Preserve_Xml_Space return Text_Key
   is ((Internal => Generate_Holder (+(-"preserve XML space"))));

   function Preserve_Xml_Whitespace return Text_Key
   is ((Internal => Generate_Holder (+(-"preserve XML whitespace"))));

   function Xmlns return Text_Key
   is ((Internal => Generate_Holder (+(-"XML namespace"))));

   function Xmlns_C return Text_Key
   is ((Internal => Generate_Holder (+(-"XML C namespace"))));

   function Xmlns_Glib return Text_Key
   is ((Internal => Generate_Holder (+(-"XML glib namespace"))));

   function Content return Text_Key
   is ((Internal => Generate_Holder (+(-"content"))));

   ------------------
   -- Element keys --
   ------------------

   function Alias return Element_Key
   is ((Internal => Generate_Holder (+(-"alias"))));

   function Array_Element return Element_Key
   is ((Internal => Generate_Holder (+(-"array element"))));

   function Attribute return Element_Key
   is ((Internal => Generate_Holder (+(-"attribute"))));

   function Bitfield return Element_Key
   is ((Internal => Generate_Holder (+(-"bit field"))));

   function C_Include return Element_Key
   is ((Internal => Generate_Holder (+(-"C include"))));

   function Callback return Element_Key
   is ((Internal => Generate_Holder (+(-"callback"))));

   function Class return Element_Key
   is ((Internal => Generate_Holder (+(-"class"))));

   function Constant_Element return Element_Key
   is ((Internal => Generate_Holder (+(-"constant element"))));

   function Constructor return Element_Key
   is ((Internal => Generate_Holder (+(-"constructor"))));

   function Doc return Element_Key
   is ((Internal => Generate_Holder (+(-"doc"))));

   function Doc_Deprecated return Element_Key
   is ((Internal => Generate_Holder (+(-"doc deprecated"))));

   function Doc_Stability return Element_Key
   is ((Internal => Generate_Holder (+(-"doc stability"))));

   function Doc_Version return Element_Key
   is ((Internal => Generate_Holder (+(-"doc version"))));

   function Doc_Section return Element_Key
   is ((Internal => Generate_Holder (+(-"doc section"))));

   function Enumeration return Element_Key
   is ((Internal => Generate_Holder (+(-"enumeration"))));

   function Field return Element_Key
   is ((Internal => Generate_Holder (+(-"field"))));

   function Function_Element return Element_Key
   is ((Internal => Generate_Holder (+(-"function"))));

   function Function_Inline return Element_Key
   is ((Internal => Generate_Holder (+(-"inline function"))));

   function Function_Macro return Element_Key
   is ((Internal => Generate_Holder (+(-"macro function"))));

   function Glib_Boxed return Element_Key
   is ((Internal => Generate_Holder (+(-"glib boxed"))));

   function Glib_Signal return Element_Key
   is ((Internal => Generate_Holder (+(-"glib signal"))));

   function Implements return Element_Key
   is ((Internal => Generate_Holder (+(-"implements"))));

   function Include return Element_Key
   is ((Internal => Generate_Holder (+(-"include"))));

   function Instance_Parameter return Element_Key
   is ((Internal => Generate_Holder (+(-"instance parameter"))));

   function Interface_Element return Element_Key
   is ((Internal => Generate_Holder (+(-"interface"))));

   function Member return Element_Key
   is ((Internal => Generate_Holder (+(-"member"))));

   function Method return Element_Key
   is ((Internal => Generate_Holder (+(-"method"))));

   function Method_Inline return Element_Key
   is ((Internal => Generate_Holder (+(-"inline method"))));

   function Namespace return Element_Key
   is ((Internal => Generate_Holder (+(-"namespace"))));

   function Package_Element return Element_Key
   is ((Internal => Generate_Holder (+(-"package"))));

   function Parameter return Element_Key
   is ((Internal => Generate_Holder (+(-"parameter"))));

   function Parameters return Element_Key
   is ((Internal => Generate_Holder (+(-"parameters"))));

   function Prerequisite return Element_Key
   is ((Internal => Generate_Holder (+(-"prerequisite"))));

   function Property return Element_Key
   is ((Internal => Generate_Holder (+(-"property"))));

   function Record_Element return Element_Key
   is ((Internal => Generate_Holder (+(-"record"))));

   function Repository return Element_Key
   is ((Internal => Generate_Holder (+(-"repository"))));

   function Return_Value return Element_Key
   is ((Internal => Generate_Holder (+(-"return value"))));

   function Source_Position return Element_Key
   is ((Internal => Generate_Holder (+(-"source position"))));

   function Type_Element return Element_Key
   is ((Internal => Generate_Holder (+(-"type"))));

   function Union return Element_Key
   is ((Internal => Generate_Holder (+(-"union"))));

   function Varargs return Element_Key
   is ((Internal => Generate_Holder (+(-"variadic arguments"))));

   function Virtual_Method return Element_Key
   is ((Internal => Generate_Holder (+(-"virtual method"))));

end Gir_Reader.Keys;
