with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;

package Utils is
    package U_Str renames Ada.Strings.Unbounded;
    package F_Str renames Ada.Strings.Fixed;

    function Next_Line (F : in File_Type) return U_Str.Unbounded_String;
    function Next_Natural (S : in String; Previous_Position : in out Natural) return Natural;
end Utils;
