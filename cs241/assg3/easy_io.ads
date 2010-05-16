with Ada.Text_IO; use Ada.Text_IO;

package Easy_IO is

    function GetInputFile(msg : String) return File_Type;
    function GetOutputFile(msg : String) return File_Type;

end Easy_IO;
