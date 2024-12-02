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
end Utils;