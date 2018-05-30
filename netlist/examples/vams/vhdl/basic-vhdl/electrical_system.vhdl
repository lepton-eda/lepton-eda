PACKAGE electrical_system IS
    CONSTANT epsi : real := 1.0e-18;
-- declare subtypes for voltage and current
    SUBTYPE voltage IS real; -- TOLERANCE "default_voltage";
    SUBTYPE current IS real; -- TOLERANCE "default_current";
    SUBTYPE charge  IS real; -- TOLERANCE "default_charge";

    -- basic nature and reference terminal for electrical systems
    NATURE electrical IS
        voltage ACROSS
        current THROUGH ground reference;
  FUNCTION always_positive (x:real) RETURN real;

-- a subnature that is compatible with electrical but has
    -- different tolerance codes for across and through aspects
--     SUBNATURE high_voltage IS electrical
--        TOLERANCE "MV" ACROSS "A" THROUGH;

    -- support for terminal arrays
--     NATURE electrical_vector IS ARRAY (integer RANGE <>) OF electrical;

--     Type quantity_vector IS ARRAY (integer RANGE <>) OF real;
--     Type Adresse is array (integer RANGE <>) of integer;

END PACKAGE electrical_system;
---------------------------------------------------------------------

PACKAGE BODY electrical_system IS 
  FUNCTION always_positive (x:real) RETURN real IS
  BEGIN
    IF (x < epsi) THEN
      RETURN epsi;
    ELSE
      RETURN x;
    END if;
  END; 
END package body;

