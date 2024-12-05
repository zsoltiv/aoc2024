with Ada.Strings.Unbounded;
with Ada.Strings; use Ada.Strings; -- needed for `Backward`
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps;

package body Utils is
    package Maps renames Ada.Strings.Maps;
    function Next_Line (F : in File_Type) return U_Str.Unbounded_String
    is
        Line : U_Str.Unbounded_String := U_Str.Null_Unbounded_String;
        Buffer : String (1 .. 80);
        Last : Natural;
    begin
        Read_Line:
        loop
            Get_Line (F, Buffer, Last);
            U_Str.Append (Source => Line,
                          New_Item => Buffer (1 .. Last));
            exit Read_Line when Last < Buffer'Last
                             or End_Of_File (F);
        end loop Read_Line;

        return Line;
    end Next_Line;

    function Next_Positive (S : in String; Previous_Position : in out Natural) return Natural
    is
        Numerics : constant  Maps.Character_Set := Maps.To_Set ("0123456789");
        Start_Idx, End_Idx : Natural;
    begin
        if Previous_Position = S'Last + 1 then
            return 0;
        end if;
        Start_Idx := F_Str.Index (Source => S,
                                  Set => Numerics,
                                  Test => Inside,
                                  From => Previous_Position);
        if Start_Idx = 0 then
            return 0;
        end if;

        End_Idx := F_Str.Index (Source => S,
                                Set => Numerics,
                                Test => Outside,
                                From => Start_Idx);
        if End_Idx = 0 then
            Previous_Position := S'Last + 1;
            return Natural'Value(S (Start_Idx .. S'Last));
        else
            Previous_Position := End_Idx;
            return Natural'Value(S (Start_Idx .. End_Idx - 1));
        end if;
    end;

    function Next_Natural (S : in String; Previous_Position : in out Natural) return Natural
    is
        Numerics : constant  Maps.Character_Set := Maps.To_Set ("0123456789");
        End_Idx : Natural := F_Str.Index (Source => S,
                                          Set => Numerics,
                                          Test => Outside,
                                          From => Previous_Position);
        Ret : Natural;
    begin
        Put_Line (End_Idx'Image);
        -- end of S
        if End_Idx = 0 then
            Ret := Natural'Value (S (Previous_Position .. S'Last));
            Previous_Position := S'Last + 1;
            return Ret;
        else
            Ret := Natural'Value (S (Previous_Position .. (End_Idx - 1)));
            loop
                if S (End_Idx) /= ' ' then
                    exit;
                end if;
                End_Idx := End_Idx + 1; -- find next one
            end loop;
            Previous_Position := End_Idx;
            return Ret;
        end if;
    end Next_Natural;

    function Reverse_String (S : in String) return String
    is
        R : String (S'Range);
    begin
        for I in S'Range loop
            R (S'Last - I + S'First) := S (I);
        end loop;
        return R;
    end;

    function Line_Vector_Count (Lines : in Line_Vector.Vector; Word : in String) return Natural
    is
        -- helper functions
        function Forward_Count (Line, Word : in String) return Natural
        is
            Last : Natural := Word'First;
            Occurences : Natural := 0;
        begin
            loop
                Last := F_Str.Index (Source => Line,
                                     Pattern => Word,
                                     Going => Forward,
                                     From => Last);
                exit when Last = 0;
                Occurences := Occurences + 1;
                Last := Last + 1; -- don't find the same instance every time
            end loop;
            
            return Occurences;
        end Forward_Count;
        function Backward_Count (Line, Word : in String) return Natural
        is
        begin
            return Forward_Count (Line, Reverse_String (Word));
        end Backward_Count;

        Occurences : Natural := 0;
    begin
        -- horizontal
        Put_Line ("HORIZONTAL" & Occurences'Image);
        for Line of Lines loop
            Occurences := Occurences + Forward_Count  (Line, Word);
            Occurences := Occurences + Backward_Count (Line, Word);
            Put_Line (Line);
        end loop;
        -- vertical
        Put_Line ("VERTICAL" & Occurences'Image);
        declare
            Column : String (Lines.First_Index .. Lines.Last_Index);
            First_Line : String := Lines.Element (Lines.First_Index);
        begin
            for Column_Idx in First_Line'Range loop
                for Row_Idx in Lines.First_Index .. Lines.Last_Index loop
                    declare
                        Line : String := Lines.Element (Row_Idx);
                    begin
                        Column (Row_Idx) := Line (Column_Idx);
                    end;
                end loop;
                --Put_Line (Column);
                Occurences := Occurences + Forward_Count  (Column, Word);
                Occurences := Occurences + Backward_Count (Column, Word);
            end loop;
        end;
        -- upper-left to lower-right and back
        Put_Line ("UP LEFT TO LOW RIGHT 1st" & Occurences'Image);
        for I in Lines.First_Index .. Lines.Last_Index loop
            declare
                S : String (Lines.First_Index .. I);
            begin
                for J in Lines.First_Index .. I loop
                    declare
                        Line : String := Lines.Element (J);
                    begin
                        S (J) := Line (I - J + Line'First);
                    end;
                end loop;
                Put_line (S);
                Occurences := Occurences + Forward_Count  (S, Word);
                Occurences := Occurences + Backward_Count (S, Word);
            end;
        end loop;
        for I in Lines.First_Index + 1 .. Lines.Last_Index loop
            declare
                S : String (Lines.First_Index .. I);
            begin
                for J in reverse Lines.First_Index .. I loop
                    declare
                        Line : String := Lines.Element (J);
                    begin
                        S (J) := Line (I - J + Line'First);
                    end;
                end loop;
                Put_line (S);
                Occurences := Occurences + Forward_Count  (S, Word);
                Occurences := Occurences + Backward_Count (S, Word);
            end;
        end loop;
        -- other half
        -- XXX GNAT won't warn about ranges like `99 .. 1` it'll just omit them :)
        Put_Line ("UP LEFT TO LOW RIGHT 2nd" & Occurences'Image);
        for Start_Idx in reverse Word'Length .. Lines.Last_Index - Word'Length + Lines.First_Index loop
            declare
                S : String (Lines.First_Index .. Lines.Last_Index - Start_Idx + Lines.First_Index);
            begin
                for I in reverse Start_Idx .. Lines.Last_Index loop
                    declare
                        Line : String := Lines.Element (I);
                    begin
                        S (I - Start_Idx + S'First) := Line (Line'Last - Start_Idx + Line'First);
                    end;
                end loop;
                --Put_line (S);
                Occurences := Occurences + Forward_Count  (S, Word);
                Occurences := Occurences + Backward_Count (S, Word);
            end;
        end loop;
        -- upper right to lower left
        Put_Line ("UP RIGHT TO LOW LEFT 1st" & Occurences'Image);
        for Start_Idx in reverse Word'Length .. Lines.Last_Index - Word'Length + Lines.First_Index loop
            declare
                S : String (Lines.First_Index .. Lines.Last_Index - Start_Idx + Lines.First_Index);
            begin
                for I in Lines.First_Index .. S'Length loop
                    declare
                        Line : String := Lines.Element (I);
                    begin
                        S (I) := Line (Start_Idx + I - S'First);
                    end;
                end loop;
                --Put_Line (S);
                Occurences := Occurences + Forward_Count  (S, Word);
                Occurences := Occurences + Backward_Count (S, Word);
            end;
        end loop;
        -- other half again
        Put_Line ("UP RIGHT TO LOW LEFT 2nd" & Occurences'Image);
        for Start_Idx in Word'Length .. Lines.Last_Index loop
            declare
                S : String (Lines.First_Index .. Start_Idx);
            begin
                for I in reverse Lines.Last_Index - Start_Idx + Lines.First_Index .. Lines.Last_Index loop
                    declare
                        Line : String := Lines.Element (I);
                    begin
                        S (Lines.Last_Index - I + Lines.First_Index) := Line (I);
                    end;
                end loop;
                --Put_Line (S);
                Occurences := Occurences + Forward_Count  (S, Word);
                Occurences := Occurences + Backward_Count (S, Word);
            end;
        end loop;

        return Occurences;
    end Line_Vector_Count;
end Utils;
