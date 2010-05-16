with Ada.Text_IO; use Ada.Text_IO;

package Easy_IO is

    procedure GetInputFile(msg :in String; file : out File_Type);
    procedure GetOutputFile(msg : in String; file : out File_Type);

end Easy_IO;
