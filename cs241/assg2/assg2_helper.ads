with Ada.Text_IO; use Ada.Text_IO;

package Assg2_Helper is
    subtype StrBuffer is String(1..256);

    procedure OpenOrCreate(file : in out File_Type; fileName : in String);
    procedure OpenRobust(file : in out File_Type; msg : in String);
    procedure GetRobust(item : in out Natural; msg : in String);
    procedure RunHashPerformanceTests(inFile : in out File_Type; outFile : in out File_Type; size : in Natural);

end Assg2_Helper;
