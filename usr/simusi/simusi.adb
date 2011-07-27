-- Simulation of manufacturing
-- Each simulation deals with N Columns
-- 2 files are needed as input, each containing N lines:
--  a manufacturing file with lines containing:
--   Ci Cj FeasibleAccuracy
--  a specification file with lines containing:
--   Ci Cj SpecificationCote RequiredAccuracy
-- The simulation computes the manufacturing cotes verifies that the required
--  accuracies are feasible
-- Arguments are [ -v ] <manuf_file> <specif_file>

-- Ex, inputs:
--    01            02            03
--     |             |             |
--  M1 |<- +/-0.1 -->|
--  M2 |<------- +/-0.2 ---------->|
--
--  S1 |<- 3+/-0.5 ->|
--  S2               |<- 2+/-0.5 ->|

-- Solution :
-- M1 = S1 = 3, M2 = S1+S2 = 5
-- Acc1 = M1 = 0.1      <= S1 (0.5)  OK
-- Acc2 = M1 + M2 = 0.3 <= S2 (0.5)  OK
with Common, Resolution, Display;
procedure Simusi is

begin
  Display.Print_Tittle(Common.Manufa);
  Resolution.Solve (Common.Manufa);

  Display.Print_Tittle(Common.Design);
  Resolution.Solve (Common.Design);

  Display.Print_Result;

exception
  when Resolution.Abort_Error =>
    Display.Print_Result;
end Simusi;

