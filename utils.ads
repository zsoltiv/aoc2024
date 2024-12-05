with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

package Utils is
    package U_Str renames Ada.Strings.Unbounded;
    package F_Str renames Ada.Strings.Fixed;
    package Containers renames Ada.Containers;
    package Natural_Vecs is new Containers.Vectors
        (Element_Type => Natural,
         Index_Type => Positive);
    package Natural_Vecs_Sorting is new Natural_Vecs.Generic_Sorting;

    -- used for vec of fixed strings
    package Line_Vector is new Containers.Indefinite_Vectors
        (Index_Type => Positive, -- match `String` index type
         -- only possible because we use `Indefinite_Vectors` instead of `Vectors`
         Element_Type => String);

    function Next_Line (F : in File_Type) return U_Str.Unbounded_String;
    function Next_Natural (S : in String; Previous_Position : in out Natural) return Natural;
    function Next_Positive (S : in String; Previous_Position : in out Natural) return Natural;
    function Reverse_String (S : in String) return String;
    function Line_Vector_Count (Lines : in Line_Vector.Vector; Word : in String) return Natural;
end Utils;
