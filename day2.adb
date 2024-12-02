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

    function Is_Safe (Report : in Natural_Vecs.Vector) return Boolean
    is
        Iteration : Positive := 1;
        Diff : Integer := 0;
        Prev : Natural := 0;
        Ascending : Boolean;
    begin
        for Level of Report loop
            if Iteration = 2 then
                Diff := Integer (Level) - Integer (Prev);
                if abs Diff > 3 then
                    return false;
                end if;
                if Diff > 0 then
                    Ascending := true;
                elsif Diff < 0 then
                    Ascending := false;
                else
                    return false;
                end if;
            elsif Iteration > 2 then
                Diff := Integer (Level) - Integer (Prev);
                if abs Diff > 3 or
                   Diff = 0 or
                   (Ascending and Diff < 0) or
                   (not Ascending and Diff > 0) then
                    return false;
                end if;
            end if;
            Prev := Level;
            Iteration := Iteration + 1;
        end loop;
        return true;
    end;

    F : File_Type;
    Line : U_Str.Unbounded_String := U_Str.Null_Unbounded_String;
    Safe : Natural := 0;
    Dampened_Safe : Natural := 0;
begin
    if CLI.Argument_Count = 0 then
        return;
    end if;
    Open (F, In_File, CLI.Argument (CLI.Argument_Count));

    while not End_Of_File (F) loop
        Line := Next_Line (F);

        declare
            S : String := U_Str.To_String (Line);
            Prev_Position : Natural := S'First;
            Levels : Natural_Vecs.Vector;
        begin
            while Prev_Position <= S'Last loop
                Levels.Append (Next_Natural (S, Prev_Position));
            end loop;
            if not Is_Safe (Levels) then
                for Ignored_Idx in Levels.First_Index .. Levels.Last_Index loop
                    declare
                        Used_Levels : Natural_Vecs.Vector;
                    begin
                        for Level_Idx in Levels.First_Index .. Levels.Last_Index loop
                            if Level_Idx /= Ignored_Idx then
                                Used_Levels.Append (Levels (Level_Idx));
                            end if;
                        end loop;
                        if Is_Safe (Used_Levels) then
                            Dampened_Safe := Dampened_Safe + 1;
                            exit;
                        end if;
                    end;
                end loop;
            else
                Safe := Safe + 1;
                Dampened_Safe := Dampened_Safe + 1;
            end if;
        end;
    end loop;
    Put_Line (Safe'Image);
    Put_Line (Dampened_Safe'Image);
end Day2;
