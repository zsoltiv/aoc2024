with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Containers.Vectors;

with Utils; use Utils;

procedure Day2 is
    package CLI renames Ada.Command_Line;
    package U_Str renames Ada.Strings.Unbounded;
    package Containers renames Ada.Containers;

    package Natural_Vecs is new Containers.Vectors
        (Element_Type => Natural,
         Index_Type => Natural);

    F : File_Type;
    Line : U_Str.Unbounded_String := U_Str.Null_Unbounded_String;
    -- my input contains no zeroes, so this will do for initial values
    N : Natural := 0;
    Prev : Natural := 0;
    Safe : Natural := 0;
begin
    if CLI.Argument_Count = 0 then
        return;
    end if;
    -- open file at last arg
    Open (F, In_File, CLI.Argument (CLI.Argument_Count));
    while not End_Of_File (F) loop
        Line := Next_Line (F);
        -- check safety
        declare
            S : String := U_Str.To_String (Line);
            Prev_Position : Natural := S'First;
            Iters : Natural := 1;
            Ascending : Boolean;
            Is_Safe : Boolean := true;
        begin
            while Prev_Position <= S'Last loop
                N := Next_Natural (S, Prev_Position);
                if Iters = 2 then
                    declare
                        Diff : Integer := Integer (N) - Integer (Prev);
                    begin
                        if abs Diff <= 3 then
                            if Diff > 0 then
                                Ascending := true;
                            elsif Diff < 0 then
                                Ascending := false;
                            else
                                Is_Safe := false;
                                exit;
                            end if;
                        else
                            Is_Safe := false;
                            exit;
                        end if;
                    end;
                elsif Iters > 2 then
                    declare
                        Diff : Integer := Integer (N) - Integer (Prev);
                    begin
                        if abs Diff > 3 or
                           Diff = 0 or
                           (Ascending and Diff < 0) or
                           (not Ascending and Diff > 0) then
                            Is_Safe := false;
                            exit;
                        end if;
                    end;
                end if;
                Prev := N;
                Iters := Iters + 1;
            end loop;

            if Is_Safe then
                Safe := Safe + 1;
            end if;
        end;
    end loop;
    Put_Line (Safe'Image);
end Day2;
