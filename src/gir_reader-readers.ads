--  This package provides the Read function that converts a GIR file to an
--  Element Ada structure.

with Gir_Reader.Elements;

package Gir_Reader.Readers is

   --  Reads a GIR file and converts it to an Element Ada structure.
   --
   --  @param File_Name Name of the GIR file.
   --  @return An Element type structure.
   function Read (File_Name : String) return Gir_Reader.Elements.Element
   with Pre => File_Name'Length > 0;

end Gir_Reader.Readers;
