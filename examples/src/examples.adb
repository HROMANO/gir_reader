pragma Ada_2022;

with Ada.Containers.Indefinite_Vectors;
with Ada.IO_Exceptions;
with Ada.Text_IO;

with Gir_Reader.Elements;
with Gir_Reader.Element_Lists;
with Gir_Reader.Keys;
with Gir_Reader.Readers;

procedure Examples is

   package TIO renames Ada.Text_IO;
   package String_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type   => Positive,
        Element_Type => String);

   subtype String_List is String_Vectors.Vector;

   Gir        : Gir_Reader.Elements.Element;
   Repository : Gir_Reader.Elements.Element;
   Namespace  : Gir_Reader.Elements.Element;
   Gir_Files  : constant String_List :=
     ["Atk-1.0.gir",
      "cairo-1.0.gir",
      "fontconfig-2.0.gir",
      "freetype2-2.0.gir",
      "Gdk-3.0.gir",
      "Gdk-4.0.gir",
      "GdkPixbuf-2.0.gir",
      "GdkPixdata-2.0.gir",
      "GdkWayland-4.0.gir",
      "GdkWin32-4.0.gir",
      "GdkX11-3.0.gir",
      "GdkX11-4.0.gir",
      "Gio-2.0.gir",
      "GioUnix-2.0.gir",
      "GL-1.0.gir",
      "GLib-2.0.gir",
      "GLibUnix-2.0.gir",
      "GModule-2.0.gir",
      "GObject-2.0.gir",
      "Graphene-1.0.gir",
      "Gsk-4.0.gir",
      "Gtk-3.0.gir",
      "Gtk-4.0.gir",
      "HarfBuzz-0.0.gir",
      "libxml2-2.0.gir",
      "Pango-1.0.gir",
      "PangoCairo-1.0.gir",
      "PangoFc-1.0.gir",
      "PangoFT2-1.0.gir",
      "PangoOT-1.0.gir",
      "PangoXft-1.0.gir",
      "Vulkan-1.0.gir",
      "win32-1.0.gir",
      "xfixes-4.0.gir",
      "xft-2.0.gir",
      "xlib-2.0.gir",
      "xrandr-1.3.gir"];

begin

   for File_Name of Gir_Files loop

      TIO.Put_Line ("Reading " & File_Name);
      Gir := Gir_Reader.Readers.Read ("share/examples/" & File_Name);

      if Gir.Contains (Gir_Reader.Keys.Repository) then
         --  Only one repository in GIR files.
         Repository := Gir.Get (Gir_Reader.Keys.Repository).Get (1);
         TIO.Put_Line ("Repository attributes:");
         TIO.Put_Line (Repository.Get_Attributes'Image);
         TIO.Put_Line
           ("Repository sub elements: "
            & Repository.Get_Sub_Element_Key_List'Image);

         if Repository.Contains (Gir_Reader.Keys.Namespace) then
            --  Only one namespace in a repository.
            Namespace := Repository.Get (Gir_Reader.Keys.Namespace).Get (1);
            TIO.Put_Line ("Namespace attributes:");
            TIO.Put_Line (Namespace.Get_Attributes'Image);
            TIO.Put_Line
              ("Sub elements: " & Namespace.Get_Sub_Element_Key_List'Image);
         end if;
         TIO.New_Line;

      end if;
      --
      --
      --  KEYS :
      --  for K of Result.Get_Keys loop
      --     TIO.Put_Line (K'Image);
      --     --  TIO.Put_Line (Result.Get (K)'Image);
      --  end loop KEYS;

   end loop;

exception
   when Ada.IO_Exceptions.Device_Error =>
      null;

end Examples;
