with Ada.Text_IO; use Ada.Text_IO;

package Hashing is
    type ProbingMethod is (LinearProbing, QuadraticProbing, CubicProbing, BeatyProbing, Unknown);

    Duplicate_Exception : Exception;

    function GetClosestPrime(size : Natural) return Natural;

end Hashing;
