with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings; -- needed for `Backward`
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with Utils; use Utils;

procedure Day1 is
    package U_Str renames Ada.Strings.Unbounded;
    package F_Str renames Ada.Strings.Fixed;
    package CLI renames Ada.Command_Line;

    F : File_Type;
    Left : Natural_Vecs.Vector;
    Right : Natural_Vecs.Vector;
    Current_Line : U_Str.Unbounded_String := U_Str.Null_Unbounded_String;
    Right_Num_Idx, Space_Start : Natural;
begin
    if CLI.Argument_Count = 0 then
        return;
    end if;
    -- open file at last arg
    Open (F, In_File, CLI.Argument (CLI.Argument_Count));
    while not End_Of_File (F) loop
        Current_Line := Next_Line (F);
        -- store numbers
        declare
            -- convert to fixed string, unbounded strings have no
            -- `'First` and `'Last`
            S : String := U_Str.To_String (Current_Line);
        begin
            Space_Start := F_Str.Index (Source => S,
                                        Pattern => " ");
            Left.Append (Natural'Value (S (S'First .. (Space_Start - 1))));
            Right_Num_Idx := F_Str.Index (Source => S,
                                          Pattern => " ",
                                          From => S'Last,
                                          Going => Backward);
            Right_Num_Idx := Right_Num_Idx + 1;
            Right.Append (Natural'Value (S (Right_Num_Idx .. S'Last)));
        end;
    end loop;

    Natural_Vecs_Sorting.Sort (Left);
    Natural_Vecs_Sorting.Sort (Right);

    declare
        Accumulator : Natural := 0;
    begin
        for I in Left.First_Index .. Left.Last_Index loop
            Accumulator := Accumulator + abs (Left (I) - Right (I));
        end loop;
        Put_Line (Accumulator'Image);
    end;

    -- part 2
    declare
        Similarity_Score : Natural := 0;
        Instances : Natural;
    begin
        for Location_ID of Left loop
            Instances := 0;
            for ID of Right loop
                if Location_ID = ID then
                    Instances := Instances + 1;
                end if;
            end loop;
            Similarity_Score := Similarity_Score + Instances * Location_ID;
        end loop;
        Put_Line (Similarity_Score'Image);
    end;
end Day1;
