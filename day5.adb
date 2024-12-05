with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

with Utils; use Utils;

procedure Day5 is
    package CLI renames Ada.Command_Line;
    package U_Str renames Ada.Strings.Unbounded;

    type Order_Rule is record
        First, Second : Positive;
    end record;

    package ORV is new Ada.Containers.Vectors (
        Element_Type => Order_Rule,
        Index_Type => Positive);

    function Check_Order (Order : in Natural_Vecs.Vector; Rule: in Order_Rule) return Boolean
    is
        First_Idx: Natural := 0;
        Search_Idx : Positive := Order.First_Index;
        Second_Idx : Natural := Order.Find_Index (Rule.Second);
    begin
        if Second_Idx = 0 then
            return true;
        end if;

        loop
            declare
                I : Natural := Order.Find_Index (Rule.First, Search_Idx);
            begin
                if I /= 0 then
                    Search_Idx := First_Idx + 1;
                end if;
                exit when Search_Idx > Order.Last_Index or I = 0;
                First_Idx := I;
            end;
        end loop;
        if First_Idx = 0 then
            return false;
        end if;

        return First_Idx < Second_Idx;
    end;

    F : File_Type;
    Line : U_Str.Unbounded_String := U_Str.Null_Unbounded_String;
    Order_Rules : ORV.Vector;
    Processed_Page_Ordering_Rules : Boolean := false;
    Accum : Natural := 0;
begin
    if CLI.Argument_Count = 0 then
        return;
    end if;

    Open (F, In_File, CLI.Argument (CLI.Argument_Count));

    while not End_Of_File (F) loop
        Line := Next_Line (F);
        declare
            F_Line : String := U_Str.To_String (Line);
        begin
            if F_Line = "" then
                Processed_Page_Ordering_Rules := true;
            end if;

            if not Processed_Page_Ordering_Rules then
                declare
                    Prev_Pos : Natural := F_Line'First;
                    First_Num : Natural := Next_Positive (F_Line, Prev_Pos);
                    Second_Num : Natural := Next_Positive (F_Line, Prev_Pos);
                begin
                    Order_Rules.Append (Order_Rule'(
                        First => First_Num,
                        Second => Second_Num));
                    Put_Line (First_Num'Image & " " & Second_Num'Image);
                end;
            else
                declare
                    Order : Natural_Vecs.Vector;
                    Prev_Position : Natural := F_Line'First;
                    N : Natural;
                    Correct : Boolean := true;
                begin
                    loop
                        N := Next_Positive (F_Line, Prev_Position);
                        exit when N = 0;
                        Order.Append (N);
                    end loop;
                    for Rule of Order_Rules loop
                        Correct := Check_Order (Order, Rule);
                        exit when not Correct;
                    end loop;
                    Put_Line (Correct'Image);
                    if Correct  and Order.Length > 0 then
                        Put_Line (Order'Image);
                        Put_Line (Ada.Containers.Count_Type'Image(Order.Length / 2 + 1));
                        Accum := Accum + Order.Element (Natural (Order.Length) / 2 + 1);
                    end if;
                end;
            end if;
        end;
    end loop;

Put_Line (Accum'Image);

    Close (F);
end Day5;
