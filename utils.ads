with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

package Utils is
    package U_Str renames Ada.Strings.Unbounded;

    function Next_Line (F : in File_Type) return U_Str.Unbounded_String;
end Utils;
