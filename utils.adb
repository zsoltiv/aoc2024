with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Utils is
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

    function Next_Natural (S : in String; Previous_Position : in out Natural) return Natural
    is
        End_Idx : Natural := F_Str.Index (Source => S,
                                          Pattern => " ",
                                          From => Previous_Position);
        Ret : Natural;
    begin
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
                End_Idx := End_Idx + 1;
            end loop;
            Previous_Position := End_Idx;
            return Ret;
        end if;
    end Next_Natural;
end Utils;
