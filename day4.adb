with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings; -- needed for `Backward`
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors; -- we use fixed strings but we won't hardcode the max length

with Utils; use Utils;

procedure Day4 is
    package U_Str renames Ada.Strings.Unbounded;
    package CLI renames Ada.Command_Line;

    F : File_Type;
    Input_Data : Line_Vector.Vector;
begin
    if CLI.Argument_Count = 0 then
        return;
    end if;
    Open (F, In_File, CLI.Argument (CLI.Argument_Count));
    while not End_Of_File (F) loop
        Input_Data.Append (U_Str.To_String (Next_Line (F)));
    end loop;
    Close (F);
    Put_Line (Line_Vector_Count (Input_Data, "XMAS")'Image);
end Day4;
