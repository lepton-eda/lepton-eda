LIBRARY disciplines,ieee;
--USE  disciplines.electromagnetic_system.all;
USE  ieee.math_real.all;
USE work.electrical_system.all;

ENTITY ground_node IS
  PORT (TERMINAL t1: electrical);
END entity;

ARCHITECTURE beh OF ground_node IS
  QUANTITY v ACROSS i THROUGH t1 TO ground;
BEGIN
  v == 0.0;
END architecture;
