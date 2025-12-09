with Ada.Containers;
with Ada.Containers.Indefinite_Vectors;

with Input_Sources.File;
with Sax.Attributes;
with Sax.Readers;
with Unicode.CES;

with Gir_Reader.Element_Lists;
with Gir_Reader.Key_Types;
with Gir_Reader.Keys;

package body Gir_Reader.Readers is

   use Gir_Reader.Key_Types;
   use Gir_Reader.Keys;
   use type Ada.Containers.Count_Type;
   use type Gir_Reader.Text;

   --  @private Internal use.
   --  A vector of keys.
   package Key_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => Gir_Reader.Key_Types.Key'Class);

   --
   --  Reader type
   --

   type Reader is new Sax.Readers.Reader with record
      Current_Value        : Text;
      Current_Key_List     : Key_Vectors.Vector;
      Current_Element      : Gir_Reader.Elements.Element;
      Current_Element_List : Gir_Reader.Element_Lists.List;
      Result               : Gir_Reader.Elements.Element;
   end record;

   -------------------
   -- Start_Element --
   -------------------

   overriding
   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   with
     Pre => Handler.Current_Value = Empty_Text,
     Post => Handler.Current_Element_List.Length > 0;

   ----------------
   -- Characters --
   ----------------

   overriding
   procedure Characters
     (Handler : in out Reader; Ch : Unicode.CES.Byte_Sequence);

   -----------------
   -- End_Element --
   -----------------

   overriding
   procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   with
     Pre =>
       Handler.Current_Element_List.Length > 0
       and then Handler.Current_Element_List.Length
                = Handler.Current_Key_List.Length,
     Post =>
       Handler.Current_Value = Empty_Text
       and then Handler.Current_Key_List.Length
                = Handler'Old.Current_Key_List.Length - 1;

   -----------------------
   -- Boolean_Attribute --
   -----------------------

   procedure Boolean_Attribute
     (Handler        : in out Reader;
      Attribute_Name : String;
      Attribute_Key  : Boolean_Key;
      Value          : String) is
   begin
      if Value = "0" then
         Handler.Current_Element.Set (Attribute_Key, False);
      elsif Value = "1" then
         Handler.Current_Element.Set (Attribute_Key, True);
      else
         Log_Error ("'" & Attribute_Name & "' not in (0-1): " & Value);
      end if;
   end Boolean_Attribute;

   -----------------------
   -- Integer_Attribute --
   -----------------------

   procedure Integer_Attribute
     (Handler        : in out Reader;
      Attribute_Name : String;
      Attribute_Key  : Integer_Key;
      Value          : String)
   is
      Number : Integer;
   begin
      Number := Integer'Value (Value);
      Handler.Current_Element.Set (Attribute_Key, Number);
   exception
      when Constraint_Error =>
         Log_Error ("'" & Attribute_Name & "' is not an integer: " & Value);
   end Integer_Attribute;

   -----------------------------------
   -- Parameter_Direction_Attribute --
   -----------------------------------

   procedure Parameter_Direction_Attribute
     (Handler        : in out Reader;
      Attribute_Name : String;
      Attribute_Key  : Parameter_Direction_Key;
      Value          : String) is
   begin
      if Value = "in" then
         Handler.Current_Element.Set (Attribute_Key, Is_In);
      elsif Value = "out" then
         Handler.Current_Element.Set (Attribute_Key, Is_Out);
      elsif Value = "inout" then
         Handler.Current_Element.Set (Attribute_Key, Is_In_Out);
      else
         Log_Error
           ("'" & Attribute_Name & "' not in (in, out, inout): " & Value);
      end if;
   end Parameter_Direction_Attribute;

   ------------------------------
   -- Lifetime_Scope_Attribute --
   ------------------------------

   procedure Lifetime_Scope_Attribute
     (Handler        : in out Reader;
      Attribute_Name : String;
      Attribute_Key  : Lifetime_Scope_Key;
      Value          : String) is
   begin
      if Value = "notified" then
         Handler.Current_Element.Set (Attribute_Key, Notified);
      elsif Value = "async" then
         Handler.Current_Element.Set (Attribute_Key, Async);
      elsif Value = "call" then
         Handler.Current_Element.Set (Attribute_Key, Call);
      elsif Value = "forever" then
         Handler.Current_Element.Set (Attribute_Key, Forever);
      else
         Log_Error
           ("'"
            & Attribute_Name
            & "' not in (notified, async, call, forever): "
            & Value);
      end if;
   end Lifetime_Scope_Attribute;

   -------------------------
   -- Ownership_Attribute --
   -------------------------

   procedure Ownership_Attribute
     (Handler        : in out Reader;
      Attribute_Name : String;
      Attribute_Key  : Ownership_Key;
      Value          : String) is
   begin
      if Value = "none" then
         Handler.Current_Element.Set (Attribute_Key, None);
      elsif Value = "container" then
         Handler.Current_Element.Set (Attribute_Key, Container);
      elsif Value = "full" then
         Handler.Current_Element.Set (Attribute_Key, Full);
      else
         Log_Error
           ("'"
            & Attribute_Name
            & "' not in (none, container, full): "
            & Value);
      end if;
   end Ownership_Attribute;

   -------------------------------
   -- Signal_Emission_Attribute --
   -------------------------------

   procedure Signal_Emission_Attribute
     (Handler        : in out Reader;
      Attribute_Name : String;
      Attribute_Key  : Signal_Emission_Key;
      Value          : String) is
   begin
      if Value = "first" then
         Handler.Current_Element.Set (Attribute_Key, First);
      elsif Value = "last" then
         Handler.Current_Element.Set (Attribute_Key, Last);
      elsif Value = "cleanup" then
         Handler.Current_Element.Set (Attribute_Key, Cleanup);
      else
         Log_Error
           ("'"
            & Attribute_Name
            & "' not in (first, last, cleanup): "
            & Value);
      end if;
   end Signal_Emission_Attribute;

   --------------------
   -- Get_Attributes --
   --------------------

   procedure Get_Attributes
     (Handler : in out Reader; Atts : Sax.Attributes.Attributes'Class)
   is
      Attribute_Name : Text;
   begin
      for J in 0 .. Atts.Get_Length - 1 loop
         Attribute_Name := +Atts.Get_Qname (J);

         if Attribute_Name = "abstract" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Abstract, Atts.Get_Value (J));

         elsif Attribute_Name = "action" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Action, Atts.Get_Value (J));

         elsif Attribute_Name = "allow-none" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Allow_None, Atts.Get_Value (J));

         elsif Attribute_Name = "caller-allocates" then
            Boolean_Attribute
              (Handler,
               +Attribute_Name,
               Is_Caller_Allocates,
               Atts.Get_Value (J));

         elsif Attribute_Name = "construct" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Construct, Atts.Get_Value (J));

         elsif Attribute_Name = "construct-only" then
            Boolean_Attribute
              (Handler,
               +Attribute_Name,
               Is_Construct_Only,
               Atts.Get_Value (J));

         elsif Attribute_Name = "deprecated" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Deprecated, Atts.Get_Value (J));

         elsif Attribute_Name = "detailed" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Detailed, Atts.Get_Value (J));

         elsif Attribute_Name = "disguised" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Disguised, Atts.Get_Value (J));

         elsif Attribute_Name = "final" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Final, Atts.Get_Value (J));

         elsif Attribute_Name = "foreign" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Foreign, Atts.Get_Value (J));

         elsif Attribute_Name = "glib:fundamental" then
            Boolean_Attribute
              (Handler,
               +Attribute_Name,
               Is_Glib_Fundamental,
               Atts.Get_Value (J));

         elsif Attribute_Name = "introspectable" then
            Boolean_Attribute
              (Handler,
               +Attribute_Name,
               Is_Introspectable,
               Atts.Get_Value (J));

         elsif Attribute_Name = "no-hooks" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_No_Hooks, Atts.Get_Value (J));

         elsif Attribute_Name = "no-recurse" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_No_Recurse, Atts.Get_Value (J));

         elsif Attribute_Name = "nullable" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Nullable, Atts.Get_Value (J));

         elsif Attribute_Name = "opaque" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Opaque, Atts.Get_Value (J));

         elsif Attribute_Name = "optional" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Optional, Atts.Get_Value (J));

         elsif Attribute_Name = "pointer" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Pointer, Atts.Get_Value (J));

         elsif Attribute_Name = "private" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Private, Atts.Get_Value (J));

         elsif Attribute_Name = "readable" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Readable, Atts.Get_Value (J));

         elsif Attribute_Name = "skip" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Skip, Atts.Get_Value (J));

         elsif Attribute_Name = "throws" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Throws, Atts.Get_Value (J));

         elsif Attribute_Name = "writable" then
            Boolean_Attribute
              (Handler, +Attribute_Name, Is_Writable, Atts.Get_Value (J));

         elsif Attribute_Name = "zero-terminated" then
            Boolean_Attribute
              (Handler,
               +Attribute_Name,
               Is_Zero_Terminated,
               Atts.Get_Value (J));

         elsif Attribute_Name = "bits" then
            Integer_Attribute
              (Handler, +Attribute_Name, Bits, Atts.Get_Value (J));

         elsif Attribute_Name = "closure" then
            Integer_Attribute
              (Handler, +Attribute_Name, Closure, Atts.Get_Value (J));

         elsif Attribute_Name = "destroy" then
            Integer_Attribute
              (Handler, +Attribute_Name, Destroy, Atts.Get_Value (J));

         elsif Attribute_Name = "fixed-size" then
            Integer_Attribute
              (Handler, +Attribute_Name, Fixed_Size, Atts.Get_Value (J));

         elsif Attribute_Name = "length" then
            Integer_Attribute
              (Handler, +Attribute_Name, Length, Atts.Get_Value (J));

         elsif Attribute_Name = "direction" then
            Parameter_Direction_Attribute
              (Handler, +Attribute_Name, Direction, Atts.Get_Value (J));

         elsif Attribute_Name = "scope" then
            Lifetime_Scope_Attribute
              (Handler, +Attribute_Name, Scope, Atts.Get_Value (J));

         elsif Attribute_Name = "transfer-ownership" then
            Ownership_Attribute
              (Handler,
               +Attribute_Name,
               Transfer_Ownership,
               Atts.Get_Value (J));

         elsif Attribute_Name = "when" then
            Signal_Emission_Attribute
              (Handler, +Attribute_Name, Signal_When, Atts.Get_Value (J));

         elsif Attribute_Name = "c:identifier" then
            Handler.Current_Element.Set (C_Identifier, +Atts.Get_Value (J));

         elsif Attribute_Name = "c:identifier-prefixes" then
            Handler.Current_Element.Set
              (C_Identifier_Prefixes, +Atts.Get_Value (J));

         elsif Attribute_Name = "c:prefix" then
            Handler.Current_Element.Set (C_Prefix, +Atts.Get_Value (J));

         elsif Attribute_Name = "c:symbol-prefix" then
            Handler.Current_Element.Set (C_Symbol_Prefix, +Atts.Get_Value (J));

         elsif Attribute_Name = "c:symbol-prefixes" then
            Handler.Current_Element.Set
              (C_Symbol_Prefixes, +Atts.Get_Value (J));

         elsif Attribute_Name = "c:type" then
            Handler.Current_Element.Set (C_Type, +Atts.Get_Value (J));

         elsif Attribute_Name = "column" then
            Handler.Current_Element.Set (Column, +Atts.Get_Value (J));

         elsif Attribute_Name = "copy-function" then
            Handler.Current_Element.Set (Copy_Function, +Atts.Get_Value (J));

         elsif Attribute_Name = "default-value" then
            Handler.Current_Element.Set (Default_Value, +Atts.Get_Value (J));

         elsif Attribute_Name = "deprecated-version" then
            Handler.Current_Element.Set
              (Deprecated_Version, +Atts.Get_Value (J));

         elsif Attribute_Name = "emitter" then
            Handler.Current_Element.Set (Emitter, +Atts.Get_Value (J));

         elsif Attribute_Name = "filename" then
            Handler.Current_Element.Set (Filename, +Atts.Get_Value (J));

         elsif Attribute_Name = "free-function" then
            Handler.Current_Element.Set (Free_Function, +Atts.Get_Value (J));

         elsif Attribute_Name = "getter" then
            Handler.Current_Element.Set (Getter, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:async-func" then
            Handler.Current_Element.Set (Glib_Async_Func, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:error-domain" then
            Handler.Current_Element.Set
              (Glib_Error_Domain, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:finish-func" then
            Handler.Current_Element.Set
              (Glib_Finish_Func, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:get-property" then
            Handler.Current_Element.Set
              (Glib_Get_Property, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:get-type" then
            Handler.Current_Element.Set (Glib_Get_Type, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:get-value-func" then
            Handler.Current_Element.Set
              (Glib_Get_Value_Func, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:is-gtype-struct-for" then
            Handler.Current_Element.Set
              (Glib_Is_Gtype_Struct_For, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:name" then
            Handler.Current_Element.Set (Glib_Name, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:nick" then
            Handler.Current_Element.Set (Glib_Nick, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:ref-func" then
            Handler.Current_Element.Set (Glib_Ref_Func, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:set-property" then
            Handler.Current_Element.Set
              (Glib_Set_Property, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:set-value-func" then
            Handler.Current_Element.Set
              (Glib_Set_Value_Func, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:sync-func" then
            Handler.Current_Element.Set (Glib_Sync_Func, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:type-name" then
            Handler.Current_Element.Set (Glib_Type_Name, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:type-struct" then
            Handler.Current_Element.Set
              (Glib_Type_Struct, +Atts.Get_Value (J));

         elsif Attribute_Name = "glib:unref-func" then
            Handler.Current_Element.Set (Glib_Unref_Func, +Atts.Get_Value (J));

         elsif Attribute_Name = "invoker" then
            Handler.Current_Element.Set (Invoker, +Atts.Get_Value (J));

         elsif Attribute_Name = "line" then
            Handler.Current_Element.Set (Line, +Atts.Get_Value (J));

         elsif Attribute_Name = "moved-to" then
            Handler.Current_Element.Set (Moved_To, +Atts.Get_Value (J));

         elsif Attribute_Name = "name" then
            Handler.Current_Element.Set (Name, +Atts.Get_Value (J));

         elsif Attribute_Name = "parent" then
            Handler.Current_Element.Set (Parent, +Atts.Get_Value (J));

         elsif Attribute_Name = "setter" then
            Handler.Current_Element.Set (Setter, +Atts.Get_Value (J));

         elsif Attribute_Name = "shadowed-by" then
            Handler.Current_Element.Set (Shadowed_By, +Atts.Get_Value (J));

         elsif Attribute_Name = "shadows" then
            Handler.Current_Element.Set (Shadows, +Atts.Get_Value (J));

         elsif Attribute_Name = "shared-library" then
            Handler.Current_Element.Set (Shared_Library, +Atts.Get_Value (J));

         elsif Attribute_Name = "stability" then
            Handler.Current_Element.Set (Stability, +Atts.Get_Value (J));

         elsif Attribute_Name = "value" then
            Handler.Current_Element.Set (Value, +Atts.Get_Value (J));

         elsif Attribute_Name = "version" then
            Handler.Current_Element.Set (Version, +Atts.Get_Value (J));

         elsif Attribute_Name = "xml:space" then
            Handler.Current_Element.Set
              (Preserve_Xml_Space, +Atts.Get_Value (J));

         elsif Attribute_Name = "xml:whitespace" then
            Handler.Current_Element.Set
              (Preserve_Xml_Whitespace, +Atts.Get_Value (J));

         elsif Attribute_Name = "xmlns" then
            Handler.Current_Element.Set (Xmlns, +Atts.Get_Value (J));

         elsif Attribute_Name = "xmlns:c" then
            Handler.Current_Element.Set (Xmlns_C, +Atts.Get_Value (J));

         elsif Attribute_Name = "xmlns:glib" then
            Handler.Current_Element.Set (Xmlns_Glib, +Atts.Get_Value (J));

         else
            Log_Warning ("Unknown attribute: " & (+Attribute_Name));
         end if;

      end loop;
   end Get_Attributes;

   ---------------------
   --  Start_Element  --
   ---------------------

   overriding
   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class) is
   begin

      Handler.Current_Element_List.Append (Handler.Current_Element);
      Handler.Current_Element := Gir_Reader.Elements.Empty_Element;

      if Local_Name = "alias" then
         Handler.Current_Key_List.Append (Alias);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "array" then
         Handler.Current_Key_List.Append (Array_Element);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "attribute" then
         Handler.Current_Key_List.Append (Attribute);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "bitfield" then
         Handler.Current_Key_List.Append (Bitfield);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "c:include" then
         Handler.Current_Key_List.Append (C_Include);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "callback" then
         Handler.Current_Key_List.Append (Callback);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "class" then
         Handler.Current_Key_List.Append (Class);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "constant" then
         Handler.Current_Key_List.Append (Constant_Element);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "constructor" then
         Handler.Current_Key_List.Append (Constructor);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "doc" then
         Handler.Current_Key_List.Append (Doc);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "doc-deprecated" then
         Handler.Current_Key_List.Append (Doc_Deprecated);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "doc-stability" then
         Handler.Current_Key_List.Append (Doc_Stability);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "doc-version" then
         Handler.Current_Key_List.Append (Doc_Version);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "docsection" then
         Handler.Current_Key_List.Append (Doc_Section);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "enumeration" then
         Handler.Current_Key_List.Append (Enumeration);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "field" then
         Handler.Current_Key_List.Append (Field);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "function" then
         Handler.Current_Key_List.Append (Function_Element);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "function-inline" then
         Handler.Current_Key_List.Append (Function_Inline);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "function-macro" then
         Handler.Current_Key_List.Append (Function_Macro);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "glib:boxed" then
         Handler.Current_Key_List.Append (Glib_Boxed);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "glib:signal" then
         Handler.Current_Key_List.Append (Glib_Signal);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "implements" then
         Handler.Current_Key_List.Append (Implements);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "include" then
         Handler.Current_Key_List.Append (Include);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "instance-parameter" then
         Handler.Current_Key_List.Append (Instance_Parameter);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "interface" then
         Handler.Current_Key_List.Append (Interface_Element);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "member" then
         Handler.Current_Key_List.Append (Member);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "method" then
         Handler.Current_Key_List.Append (Method);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "method-inline" then
         Handler.Current_Key_List.Append (Method_Inline);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "namespace" then
         Handler.Current_Key_List.Append (Namespace);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "package" then
         Handler.Current_Key_List.Append (Package_Element);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "parameter" then
         Handler.Current_Key_List.Append (Parameter);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "parameters" then
         Handler.Current_Key_List.Append (Parameters);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "prerequisite" then
         Handler.Current_Key_List.Append (Prerequisite);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "property" then
         Handler.Current_Key_List.Append (Property);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "record" then
         Handler.Current_Key_List.Append (Record_Element);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "repository" then
         Handler.Current_Key_List.Append (Repository);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "return-value" then
         Handler.Current_Key_List.Append (Return_Value);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "source-position" then
         Handler.Current_Key_List.Append (Source_Position);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "type" then
         Handler.Current_Key_List.Append (Type_Element);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "union" then
         Handler.Current_Key_List.Append (Union);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "varargs" then
         Handler.Current_Key_List.Append (Varargs);
         Get_Attributes (Handler, Atts);

      elsif Local_Name = "virtual-method" then
         Handler.Current_Key_List.Append (Virtual_Method);
         Get_Attributes (Handler, Atts);

      else
         Log_Info ("Unknown start Element " & Local_Name);
         Handler.Current_Element := Handler.Current_Element_List.Last_Element;
         Handler.Current_Element_List.Delete_Last;
      end if;

   end Start_Element;

   ------------------
   --  Characters  --
   ------------------

   overriding
   procedure Characters
     (Handler : in out Reader; Ch : Unicode.CES.Byte_Sequence) is
   begin
      Handler.Current_Value := Handler.Current_Value & Ch;
   end Characters;

   -------------------
   --  End_Element  --
   -------------------

   overriding
   procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "") is
   begin

      if Handler.Current_Value /= Empty_Text then
         Handler.Current_Element.Set (Content, Handler.Current_Value);
         Handler.Current_Value := Empty_Text;
      end if;

      if Handler.Current_Element_List.Is_Empty then

         declare
            Temp      : Gir_Reader.Elements.Element;
            Temp_List : Gir_Reader.Element_Lists.List;
         begin
            if Handler.Current_Key_List.Is_Empty then
               Log_Error ("This is a bug in the library!");
            else
               Temp_List.Append (Handler.Current_Element);
               Temp.Set
                 (Element_Key (Handler.Current_Key_List.Last_Element),
                  Temp_List);
               Handler.Current_Element := Temp;
               Handler.Current_Key_List.Delete_Last;
            end if;
         end;

      else

         declare
            Temp      : Gir_Reader.Elements.Element;
            Temp_List : Gir_Reader.Element_Lists.List;
         begin
            Temp := Handler.Current_Element_List.Last_Element;

            if Temp.Contains (Handler.Current_Key_List.Last_Element) then

               Temp.Append
                 (Element_Key (Handler.Current_Key_List.Last_Element),
                  Handler.Current_Element);
            else
               Temp_List.Append (Handler.Current_Element);
               Temp.Set
                 (Element_Key (Handler.Current_Key_List.Last_Element),
                  Temp_List);
            end if;

            Handler.Current_Element := Temp;
            Handler.Current_Element_List.Delete_Last;
            Handler.Current_Key_List.Delete_Last;
         end;

      end if;

   end End_Element;

   ------------
   --  Read  --
   ------------

   function Read (File_Name : String) return Gir_Reader.Elements.Element is
      Input      : Input_Sources.File.File_Input;
      The_Reader : Reader;
   begin
      The_Reader.Set_Feature (Sax.Readers.Namespace_Prefixes_Feature, False);
      The_Reader.Set_Feature (Sax.Readers.Namespace_Feature, False);
      The_Reader.Set_Feature (Sax.Readers.Validation_Feature, False);

      Input.Set_Public_Id ("GIR file");
      Input.Set_System_Id (File_Name);
      Input_Sources.File.Open (File_Name, Input);
      The_Reader.Parse (Input);
      Input.Close;

      return The_Reader.Current_Element;
   end Read;

end Gir_Reader.Readers;
