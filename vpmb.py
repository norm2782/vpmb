#!/usr/bin/env python

# Copyright 2010, Bryan Waite, Erik C. Baker. All rights reserved.
# Redistribution and use in source and binary forms, with or without modification, are
# permitted provided that the following conditions are met:

#    1. Redistributions of source code must retain the above copyright notice, this list of
#       conditions and the following disclaimer.

#    2. Redistributions in binary form must reproduce the above copyright notice, this list
#       of conditions and the following disclaimer in the documentation and/or other materials
#       provided with the distribution.

# THIS SOFTWARE IS PROVIDED BY Bryan Waite, Erik C. Baker ``AS IS'' AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
# FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Bryan Waite, or Erik C. Baker OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# The views and conclusions contained in the software and documentation are those of the
# authors and should not be interpreted as representing official policies, either expressed
# or implied, of Bryan Waite, or Erik C. Baker.

from enum import Enum, unique
from math import log, trunc, exp, sqrt
import datetime
import argparse
import HtmlOutput
import JSONReader


NUM_COMPARTMENTS = 16
COMPARTMENT_RANGE = range(NUM_COMPARTMENTS)
ATM = 101325.0  # 1 atm of pressure
SURFACE_FRACTION_INERT_GAS = 0.79  # oxygen = 21% of air so this is what's left over

# ASSIGN HALF-TIME VALUES TO BUHLMANN COMPARTMENT ARRAYS
HELIUM_HALF_TIMES = [1.88, 3.02, 4.72, 6.99, 10.21, 14.48, 20.53, 29.11, 41.20
                    ,55.19, 70.69, 90.34, 115.29, 147.42, 188.24, 240.03]

NITROGEN_HALF_TIMES = [5.0, 8.0, 12.5, 18.5, 27.0, 38.3, 54.3, 77.0, 109.0
                      ,146.0, 187.0, 239.0, 305.0, 390.0, 498.0, 635.0]


class AltitudeException(Exception):
    """Thrown when altitude is invalid, or diver acclimatized is invalid."""
    def __init__(self, value):
        self.value = value
        super(AltitudeException, self).__init__()

    def __str__(self):
        return repr(self.value)


class MaxIterationException(Exception):
    """Thrown when root finding fails."""
    def __init__(self, value):
        self.value = value
        super(MaxIterationException, self).__init__()

    def __str__(self):
        return repr(self.value)


class InputFileException(Exception):
    """Thrown when there are errors with the input file values."""
    def __init__(self, value):
        self.value = value
        super(InputFileException, self).__init__()

    def __str__(self):
        return repr(self.value)


class DecompressionStepException(Exception):
    """Thrown when the decompression step is too large."""
    def __init__(self, value):
        self.value = value
        super(DecompressionStepException, self).__init__()

    def __str__(self):
        return repr(self.value)


class RootException(Exception):
    """Thrown when root calculated is not within brackets"""
    def __init__(self, value):
        self.value = value
        super(RootException, self).__init__()

    def __str__(self):
        return repr(self.value)


class OffGassingException(Exception):
    """Thrown when Off Gassing gradient is too small"""
    def __init__(self, value):
        self.value = value
        super(OffGassingException, self).__init__()

    def __str__(self):
        return repr(self.value)

# enum DiveError : ErrorType {
    # case AltitudeException(String) // Thrown when altitude is invalid, or diver acclimatized is invalid
    # case MaxIterationException(String) // Thrown when root finding fails
    # case DecompressionStepException(String) // Thrown when the decompression step is too large
    # case RootException(String) // Thrown when root calculated is not within brackets
    # case OffGassingException(String) // Thrown when Off Gassing gradient is too small
    # case ValueError(String)
    # case InputFileException(String)
# }

@unique
class Gas(Enum):
    Oxygen   = 1
    Helium   = 2
    Nitrogen = 4

@unique
class ProfileCode(Enum):
    Descent  = 1
    Constant = 2
    Ascent   = 99

    @staticmethod
    def fromInt(i):
        if i == 1:
            return ProfileCode.Descent
        elif i == 2:
            return ProfileCode.Constant
        else:
            return ProfileCode.Ascent

@unique
class UnitsSW(Enum):
    FSW = 1
    MSW = 2

    @staticmethod
    def fromString(str):
        if str.lower() == "fsw":
            return UnitsSW.FSW
        else:
            return UnitsSW.MSW

    def toWord1(self):
        if self == UnitsSW.FSW:
            return "fswg"
        else:
            return "mswg"

    def toWord2(self):
        if self == UnitsSW.FSW:
            return "fsw/min"
        else:
            return "msw/min"

    def toUnitsFactor(self):
        if self == UnitsSW.FSW:
            return 33.0
        else:
            return 10.1325

    def toWaterVaporPressure(self):
        if self == UnitsSW.FSW:
            return 1.607  # based on respiratory quotient of 0.8 (Schreiner value)
        else:
            return 0.493     # based on respiratory quotient of 0.8 (Schreiner value)

    def __str__(self):
        if self == UnitsSW.FSW:
            return "FSW"
        else:
            return "MSW"

class Dive(object):
    desc = ""
    gasmix_summary = []
    num_gas_mixes = 1
    profile_codes = []
    repetitive_code = 0
    surface_interval_time_minutes = 0.0

class GasMix(object):
    fraction_He = 0.0
    fraction_N2 = 79.0
    fraction_O2 = 21.0

    def __eq__(self, other):
        return self.fraction_O2 == other.fraction_O2 and self.fraction_N2 == other.fraction_N2 and self.fraction_He == other.fraction_He

class Profile(object):
    ascent_summary = []
    depth = 0.0
    ending_depth = 0.0
    gasmix = 0
    number_of_ascent_parameter_changes = 0
    profile_code = ProfileCode.Descent
    rate = 0.0
    run_time_at_end_of_segment = 0.0
    setpoint = 0.0
    starting_depth = 0.0

class Ascent(object):
    gasmix = 0
    rate = 0.0
    setpoint = 0.0
    starting_depth = 0.0
    step_size = 0.0

class AltitudeValues(object):
    altitude = 0.0
    Ascent_to_Altitude_Hours = 0.0
    Altitude_of_Dive = 0.0
    Diver_Acclimatized_at_Altitude = False
    Hours_at_Altitude_Before_Dive = 0.0
    Starting_Acclimatized_Altitude = 0.0

class SettingsValues(object):
    Altitude_Dive_Algorithm = False
    Critical_Radius_N2_Microns = 0.0
    Critical_Radius_He_Microns = 0.0
    Crit_Volume_Parameter_Lambda = 0.0
    Critical_Volume_Algorithm = False
    Gradient_Onset_of_Imperm_Atm = 0.0
    Minimum_Deco_Stop_Time = 0.0
    Pressure_Other_Gases_mmHg = 0.0
    Regeneration_Time_Constant = 0.0
    Surface_Tension_Gamma = 0.0
    Skin_Compression_GammaC = 0.0
    Units = UnitsSW.MSW

class DiveState(object):
    """Contains the program state so that this isn't a huge mess"""

    def __init__(self):
        """Set default values"""

        # Global Arrays
        self.Allowable_Gradient_He = [0.0] * NUM_COMPARTMENTS
        self.Allowable_Gradient_N2 = [0.0] * NUM_COMPARTMENTS

        self.Deco_Gradient_He = [0.0] * NUM_COMPARTMENTS
        self.Deco_Gradient_N2 = [0.0] * NUM_COMPARTMENTS

        # GLOBAL VARIABLES
        self.Segment_Number = 0
        self.Run_Time = 0.0
        self.Segment_Time = 0.0
        self.Ending_Ambient_Pressure = 0.0
        self.Mix_Number = 0
        self.Barometric_Pressure = 0.0

        # GLOBAL ARRAYS

        # Float
        self.Helium_Time_Constant = [0.0] * NUM_COMPARTMENTS
        self.Nitrogen_Time_Constant = [0.0] * NUM_COMPARTMENTS

        self.Helium_Pressure = [0.0] * NUM_COMPARTMENTS
        self.Nitrogen_Pressure = [0.0] * NUM_COMPARTMENTS

        self.Initial_Helium_Pressure = [0.0] * NUM_COMPARTMENTS
        self.Initial_Nitrogen_Pressure = [0.0] * NUM_COMPARTMENTS

        self.Fraction_Helium = []
        self.Fraction_Nitrogen = []

        # TODO Remove
        # self.settings_values
    def set_input_values(self, inputs):
        self.input_values = inputs

    def set_settings(self, settings):
        self.settings_values = settings

    def set_altitude_values(self, altitude_values):
        self.altitude_values = altitude_values

    def set_output_object(self, output_object):
        self.output_object = output_object

    def get_json(self):
        return self.output_object.get_json()

    def decompression_stop(self, settings, deco_stop_depth, step_size):
        """
        Purpose: This subprogram calculates the required time at each
        decompression stop.
        """

        last_run_time = self.Run_Time
        round_up_operation = round((last_run_time / settings.Minimum_Deco_Stop_Time) + 0.5) * settings.Minimum_Deco_Stop_Time
        self.Segment_Time = round_up_operation - self.Run_Time
        self.Run_Time = round_up_operation
        temp_segment_time = self.Segment_Time
        self.Segment_Number += 1
        ambient_pressure = deco_stop_depth + self.Barometric_Pressure
        self.Ending_Ambient_Pressure = ambient_pressure
        next_stop = deco_stop_depth - step_size

        inspired_helium_pressure = (ambient_pressure - settings.Units.toWaterVaporPressure()) * self.Fraction_Helium[self.Mix_Number - 1]
        inspired_nitrogen_pressure = (ambient_pressure - settings.Units.toWaterVaporPressure()) * self.Fraction_Nitrogen[self.Mix_Number - 1]

        # Check to make sure that program won't lock up if unable to decompress
        # to the next stop.  If so, write error message and terminate program.

        for i in COMPARTMENT_RANGE:
            if(inspired_helium_pressure + inspired_nitrogen_pressure) > 0.0:
                weighted_allowable_gradient = (self.Deco_Gradient_He[i] * inspired_helium_pressure + self.Deco_Gradient_N2[i] * inspired_nitrogen_pressure) / (inspired_helium_pressure + inspired_nitrogen_pressure)

                if (inspired_helium_pressure + inspired_nitrogen_pressure + settings.Constant_Pressure_Other_Gases - weighted_allowable_gradient) > (next_stop + self.Barometric_Pressure):
                    raise OffGassingException("ERROR! OFF-GASSING GRADIENT IS TOO SMALL TO DECOMPRESS AT THE %f STOP. Next stop: %f" % (deco_stop_depth, next_stop))

        while True:
            for i in COMPARTMENT_RANGE:
                initial_helium_pressure = self.Helium_Pressure[i]
                initial_nitrogen_pressure = self.Nitrogen_Pressure[i]
                self.Helium_Pressure[i] = haldane_equation(initial_helium_pressure, inspired_helium_pressure, self.Helium_Time_Constant[i], self.Segment_Time)
                self.Nitrogen_Pressure[i] = haldane_equation(initial_nitrogen_pressure, inspired_nitrogen_pressure, self.Nitrogen_Time_Constant[i], self.Segment_Time)

            deco_ceiling_depth = calc_deco_ceiling( self.Helium_Pressure, self.Nitrogen_Pressure
                                                  , self.Deco_Gradient_He, self.Deco_Gradient_N2
                                                  , self.Barometric_Pressure, settings
                                                  )

            if deco_ceiling_depth > next_stop: # TODO Build this into the loop condition
                self.Segment_Time = settings.Minimum_Deco_Stop_Time
                time_counter = temp_segment_time
                temp_segment_time = time_counter + settings.Minimum_Deco_Stop_Time
                last_run_time = self.Run_Time
                self.Run_Time = last_run_time + settings.Minimum_Deco_Stop_Time
                continue
            break

        self.Segment_Time = temp_segment_time

    def main(self):
        """
        Purpose:
        Main decompression loop. Checks that validates the input file, initializes the data
        and loops over the dives.
        """
        settings = self.settings_values # TODO Pass settings as param to main instead

        if settings.Regeneration_Time_Constant <= 0:
            raise InputFileException("Regeneration_Time_Constant must be greater than 0")

        if settings.Units == UnitsSW.FSW and self.altitude_values.Altitude_of_Dive > 30000.0:
            raise AltitudeException("ERROR! ALTITUDE OF DIVE HIGHER THAN MOUNT EVEREST")

        if settings.Units == UnitsSW.MSW and self.altitude_values.Altitude_of_Dive > 9144.0:
            raise AltitudeException("ERROR! ALTITUDE OF DIVE HIGHER THAN MOUNT EVEREST")

        # nitrogen
        if settings.Critical_Radius_N2_Microns < 0.2 or settings.Critical_Radius_N2_Microns > 1.35:
            raise ValueError("Bad Critical Radius N2 Microns: Critical_Radius_N2_Microns = %f, must be between '0.2' and '1.35'" % settings.Critical_Radius_N2_Microns)

        # helium
        if settings.Critical_Radius_He_Microns < 0.2 or settings.Critical_Radius_He_Microns > 1.35:
            raise ValueError("Bad Critical_Radius_He_Microns: Critical_Radius_He_Microns = %f, must be between '0.2' and '1.35'" % settings.Critical_Radius_He_Microns)

        # START initialize_data

        # Purpose: Initialize the object with the data loaded from the input file

        # INITIALIZE CONSTANTS/VARIABLES
        settings.Constant_Pressure_Other_Gases = (settings.Pressure_Other_Gases_mmHg / 760.0) * settings.Units.toUnitsFactor()
        self.Run_Time = 0.0
        self.Segment_Number = 0

        Initial_Critical_Radius_He = [0.0] * NUM_COMPARTMENTS
        Initial_Critical_Radius_N2 = [0.0] * NUM_COMPARTMENTS

        Max_Crushing_Pressure_He = [0.0] * NUM_COMPARTMENTS
        Max_Crushing_Pressure_N2 = [0.0] * NUM_COMPARTMENTS

        Surface_Phase_Volume_Time = [0.0] * NUM_COMPARTMENTS

        Max_Actual_Gradient = [0.0] * NUM_COMPARTMENTS

        Amb_Pressure_Onset_of_Imperm = [0.0] * NUM_COMPARTMENTS
        Gas_Tension_Onset_of_Imperm = [0.0] * NUM_COMPARTMENTS


        for i in COMPARTMENT_RANGE:
            self.Helium_Time_Constant[i] = log(2.0) / HELIUM_HALF_TIMES[i]
            self.Nitrogen_Time_Constant[i] = log(2.0) / NITROGEN_HALF_TIMES[i]
            Initial_Critical_Radius_N2[i] = settings.Critical_Radius_N2_Microns * 1.0E-6
            Initial_Critical_Radius_He[i] = settings.Critical_Radius_He_Microns * 1.0E-6

        if settings.Altitude_Dive_Algorithm and self.altitude_values.Ascent_to_Altitude_Hours <= 0 and not self.altitude_values.Diver_Acclimatized_at_Altitude:
            raise AltitudeException("If diver is not acclimatized, Ascent_to_Altitude_Time must be greater than 0")

        #     INITIALIZE VARIABLES FOR SEA LEVEL OR ALTITUDE DIVE
        #     See subroutines for explanation of altitude calculations.  Purposes are
        #     1) to determine barometric pressure and 2) set or adjust the VPM critical
        #     radius variables and gas loadings, as applicable, based on altitude,
        #     ascent to altitude before the dive, and time at altitude before the dive

        Adjusted_Critical_Radius_He = [0.0] * NUM_COMPARTMENTS
        Adjusted_Critical_Radius_N2 = [0.0] * NUM_COMPARTMENTS

        if settings.Altitude_Dive_Algorithm:
            # START vpm_altitude_dive_algorithm

            # Purpose:  This subprogram updates gas loadings and adjusts critical radii
            # (as required) based on whether or not diver is acclimatized at altitude or
            # makes an ascent to altitude before the dive.

            ascent_to_altitude_time = self.altitude_values.Ascent_to_Altitude_Hours * 60.0
            time_at_altitude_before_dive = self.altitude_values.Hours_at_Altitude_Before_Dive * 60.0

            if self.altitude_values.Diver_Acclimatized_at_Altitude:
                self.Barometric_Pressure = calc_barometric_pressure(self.altitude_values.Altitude_of_Dive, settings.Units)

                for i in COMPARTMENT_RANGE:
                    Adjusted_Critical_Radius_N2[i] = Initial_Critical_Radius_N2[i]
                    Adjusted_Critical_Radius_He[i] = Initial_Critical_Radius_He[i]
                    self.Helium_Pressure[i] = 0.0
                    self.Nitrogen_Pressure[i] = (self.Barometric_Pressure - settings.Units.toWaterVaporPressure()) * SURFACE_FRACTION_INERT_GAS
            else:
                if (self.altitude_values.Starting_Acclimatized_Altitude >= self.altitude_values.Altitude_of_Dive) or (self.altitude_values.Starting_Acclimatized_Altitude < 0.0):
                    raise AltitudeException("ERROR! STARTING ACCLIMATIZED ALTITUDE MUST BE LESS THAN ALTITUDE OF DIVE AND GREATER THAN OR EQUAL TO ZERO")

                self.Barometric_Pressure = calc_barometric_pressure(self.altitude_values.Starting_Acclimatized_Altitude, settings.Units)

                starting_ambient_pressure = self.Barometric_Pressure

                for i in COMPARTMENT_RANGE:
                    self.Helium_Pressure[i] = 0.0
                    self.Nitrogen_Pressure[i] = (self.Barometric_Pressure - settings.Units.toWaterVaporPressure()) * SURFACE_FRACTION_INERT_GAS

                self.Barometric_Pressure = calc_barometric_pressure(self.altitude_values.Altitude_of_Dive, settings.Units)
                ending_ambient_pressure = self.Barometric_Pressure
                initial_inspired_n2_pressure = (starting_ambient_pressure - settings.Units.toWaterVaporPressure()) * SURFACE_FRACTION_INERT_GAS
                rate = (ending_ambient_pressure - starting_ambient_pressure) / ascent_to_altitude_time
                nitrogen_rate = rate * SURFACE_FRACTION_INERT_GAS

                for i in COMPARTMENT_RANGE:
                    initial_nitrogen_pressure = self.Nitrogen_Pressure[i]
                    self.Nitrogen_Pressure[i] = schreiner_equation(initial_inspired_n2_pressure, nitrogen_rate, ascent_to_altitude_time, self.Nitrogen_Time_Constant[i], initial_nitrogen_pressure)
                    compartment_gradient = (self.Nitrogen_Pressure[i] + settings.Constant_Pressure_Other_Gases) - ending_ambient_pressure
                    compartment_gradient_pascals = (compartment_gradient / settings.Units.toUnitsFactor()) * ATM
                    gradient_he_bubble_formation = ((2.0 * settings.Surface_Tension_Gamma * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma)) / (Initial_Critical_Radius_He[i] * settings.Skin_Compression_GammaC))

                    if compartment_gradient_pascals > gradient_he_bubble_formation:
                        new_critical_radius_he = ((2.0 * settings.Surface_Tension_Gamma * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma))) / (compartment_gradient_pascals * settings.Skin_Compression_GammaC)
                        Adjusted_Critical_Radius_He[i] = Initial_Critical_Radius_He[i] + (Initial_Critical_Radius_He[i] - new_critical_radius_he) * exp(-time_at_altitude_before_dive / settings.Regeneration_Time_Constant)
                        Initial_Critical_Radius_He[i] = Adjusted_Critical_Radius_He[i]
                    else:
                        ending_radius_he = 1.0 / (compartment_gradient_pascals / (2.0 * (settings.Surface_Tension_Gamma - settings.Skin_Compression_GammaC)) + 1.0 / Initial_Critical_Radius_He[i])
                        regenerated_radius_he = Initial_Critical_Radius_He[i] + (ending_radius_he - Initial_Critical_Radius_He[i]) * exp(-time_at_altitude_before_dive / settings.Regeneration_Time_Constant)
                        Initial_Critical_Radius_He[i] = regenerated_radius_he
                        Adjusted_Critical_Radius_He[i] = Initial_Critical_Radius_He[i]

                    gradient_n2_bubble_formation = ((2.0 * settings.Surface_Tension_Gamma * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma)) / (Initial_Critical_Radius_N2[i] * settings.Skin_Compression_GammaC))

                    if compartment_gradient_pascals > gradient_n2_bubble_formation:
                        new_critical_radius_n2 = ((2.0 * settings.Surface_Tension_Gamma * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma))) / (compartment_gradient_pascals * settings.Skin_Compression_GammaC)
                        Adjusted_Critical_Radius_N2[i] = Initial_Critical_Radius_N2[i] + (Initial_Critical_Radius_N2[i] - new_critical_radius_n2) * exp(-time_at_altitude_before_dive / settings.Regeneration_Time_Constant)
                        Initial_Critical_Radius_N2[i] = Adjusted_Critical_Radius_N2[i]
                    else:
                        ending_radius_n2 = 1.0 / (compartment_gradient_pascals / (2.0 * (settings.Surface_Tension_Gamma - settings.Skin_Compression_GammaC)) + 1.0 / Initial_Critical_Radius_N2[i])
                        regenerated_radius_n2 = Initial_Critical_Radius_N2[i] + (ending_radius_n2 - Initial_Critical_Radius_N2[i]) * exp(-time_at_altitude_before_dive / settings.Regeneration_Time_Constant)
                        Initial_Critical_Radius_N2[i] = regenerated_radius_n2
                        Adjusted_Critical_Radius_N2[i] = Initial_Critical_Radius_N2[i]

                inspired_nitrogen_pressure = (self.Barometric_Pressure - settings.Units.toWaterVaporPressure()) * SURFACE_FRACTION_INERT_GAS

                for i in COMPARTMENT_RANGE:
                    initial_nitrogen_pressure = self.Nitrogen_Pressure[i]

                    self.Nitrogen_Pressure[i] = haldane_equation(initial_nitrogen_pressure, inspired_nitrogen_pressure, self.Nitrogen_Time_Constant[i], time_at_altitude_before_dive)
            # END vpm_altitude_dive_algorithm
        else:
            self.Barometric_Pressure = calc_barometric_pressure(0.0, settings.Units)

            for i in COMPARTMENT_RANGE:
                Adjusted_Critical_Radius_N2[i] = Initial_Critical_Radius_N2[i]
                Adjusted_Critical_Radius_He[i] = Initial_Critical_Radius_He[i]
                self.Helium_Pressure[i] = 0.0
                self.Nitrogen_Pressure[i] = (self.Barometric_Pressure - settings.Units.toWaterVaporPressure()) * SURFACE_FRACTION_INERT_GAS
        # END initialize_data

        # START OF REPETITIVE DIVE LOOP
        # This is the largest loop in the main program and operates between Lines
        # 30 and 330.  If there is one or more repetitive dives, the program will
        # return to this point to process each repetitive dive.
        # TODO keep these vars here?
        rate = 0.0
        starting_depth = 0.0
        ending_depth = 0.0
        Step_Size = 0.0
        rate_Change = []
        Step_Size_Change = []

        Mix_Change = []
        Depth_Change = []

        Phase_Volume_Time = [0.0] * NUM_COMPARTMENTS
        Last_Phase_Volume_Time = [0.0] * NUM_COMPARTMENTS

        Regenerated_Radius_He = [0.0] * NUM_COMPARTMENTS
        Regenerated_Radius_N2 = [0.0] * NUM_COMPARTMENTS

        Adjusted_Crushing_Pressure_He = [0.0] * NUM_COMPARTMENTS
        Adjusted_Crushing_Pressure_N2 = [0.0] * NUM_COMPARTMENTS

        Initial_Allowable_Gradient_N2 = [0.0] * NUM_COMPARTMENTS
        Initial_Allowable_Gradient_He = [0.0] * NUM_COMPARTMENTS

        He_Pressure_Start_of_Ascent = [0.0] * NUM_COMPARTMENTS
        N2_Pressure_Start_of_Ascent = [0.0] * NUM_COMPARTMENTS
        He_Pressure_Start_of_Deco_Zone = [0.0] * NUM_COMPARTMENTS
        N2_Pressure_Start_of_Deco_Zone = [0.0] * NUM_COMPARTMENTS


        for dive in self.input_values:
            self.output_object.new_dive(dive.desc)

            # START set_gas_mixes

            # Purpose: Checks the given gas mix fractions add up to 1.0, and adds them
            # to the object

            num_gas_mixes = dive.num_gas_mixes
            fraction_oxygen = [0.0] * num_gas_mixes
            self.Fraction_Helium = [0.0] * num_gas_mixes
            self.Fraction_Nitrogen = [0.0] * num_gas_mixes

            gas_mix_range = range(num_gas_mixes)

            for i in gas_mix_range:
                fraction_oxygen[i] = dive.gasmix_summary[i].fraction_O2
                self.Fraction_Nitrogen[i] = dive.gasmix_summary[i].fraction_N2
                self.Fraction_Helium[i] = dive.gasmix_summary[i].fraction_He

                if fraction_oxygen[i] + self.Fraction_Nitrogen[i] + self.Fraction_Helium[i] != 1.0:
                    raise InputFileException("ERROR IN INPUT FILE (gas mixes don't add up to 1.0")

            for i in gas_mix_range:
                self.output_object.add_gasmix(fraction_oxygen[i], self.Fraction_Nitrogen[i], self.Fraction_Helium[i])

            # END set_gas_mixes
            # START profile_code_loop

            # Purpose:
            # PROCESS DIVE AS A SERIES OF ASCENT/DESCENT AND CONSTANT DEPTH
            # SEGMENTS. THIS ALLOWS FOR MULTI-LEVEL DIVES AND UNUSUAL PROFILES. UPDATE
            # GAS LOADINGS FOR EACH SEGMENT.  IF IT IS A DESCENT SEGMENT, CALC CRUSHING
            # PRESSURE ON CRITICAL RADII IN EACH COMPARTMENT.
            # "Instantaneous" descents are not used in the VPM.  All ascent/descent
            # segments must have a realistic rate of ascent/descent.  Unlike Haldanian
            # models, the VPM is actually more conservative when the descent rate is
            # slower because the effective crushing pressure is reduced.  Also, a
            # realistic actual supersaturation gradient must be calculated during
            # ascents as this affects critical radii adjustments for repetitive dives.

            # Profile codes: 1 = Ascent/Descent, 2 = Constant Depth, 99 = Decompress

            for profile in dive.profile_codes:
                if profile.profile_code == ProfileCode.Descent:
                    self.Mix_Number = profile.gasmix

                    pressures = gas_loadings_ascent_descent(self.Helium_Pressure, self.Nitrogen_Pressure, self.Helium_Time_Constant, self.Nitrogen_Time_Constant, self.Initial_Helium_Pressure, self.Initial_Nitrogen_Pressure, self.Fraction_Helium, self.Fraction_Nitrogen, profile.starting_depth, profile.ending_depth, profile.rate, self.Barometric_Pressure, self.Mix_Number, settings)

                    self.Initial_Helium_Pressure = pressures[0]
                    self.Initial_Nitrogen_Pressure = pressures[1]

                    self.Helium_Pressure = pressures[2]
                    self.Nitrogen_Pressure = pressures[3]

                    self.Segment_Time = pressures[4]
                    self.Run_Time += pressures[4]

                    self.Ending_Ambient_Pressure = profile.ending_depth + self.Barometric_Pressure
                    self.Segment_Number += 1

                    if profile.ending_depth > profile.starting_depth:
                        pressure = calc_crushing_pressure( self.Initial_Helium_Pressure, self.Initial_Nitrogen_Pressure
                                                         , self.Helium_Pressure, self.Nitrogen_Pressure
                                                         , self.Helium_Time_Constant, self.Nitrogen_Time_Constant
                                                         , Adjusted_Critical_Radius_He, Adjusted_Critical_Radius_N2
                                                         , Max_Crushing_Pressure_He, Max_Crushing_Pressure_N2
                                                         , Amb_Pressure_Onset_of_Imperm, Gas_Tension_Onset_of_Imperm
                                                         , self.Fraction_Helium[self.Mix_Number - 1], self.Fraction_Nitrogen[self.Mix_Number - 1]
                                                         , self.Barometric_Pressure, profile, settings)
                        Max_Crushing_Pressure_He = pressure[0]
                        Max_Crushing_Pressure_N2 = pressure[1]

                    self.output_object.add_dive_profile_entry_descent(self.Segment_Number, self.Segment_Time, self.Run_Time, self.Mix_Number, profile.starting_depth, profile.ending_depth, profile.rate)

                elif profile.profile_code == ProfileCode.Constant:
                    self.Mix_Number = profile.gasmix

                    # START gas_loadings_constant_depth

                    # Purpose: This subprogram applies the Haldane equation to update the
                    # gas loadings (partial pressures of helium and nitrogen) in the half-time
                    # compartments for a segment at constant depth.

                    self.Segment_Time = profile.run_time_at_end_of_segment - self.Run_Time
                    self.Run_Time = profile.run_time_at_end_of_segment
                    self.Segment_Number += 1
                    ambient_pressure = profile.depth + self.Barometric_Pressure

                    inspired_helium_pressure = (ambient_pressure - settings.Units.toWaterVaporPressure()) * self.Fraction_Helium[self.Mix_Number - 1]

                    inspired_nitrogen_pressure = (ambient_pressure - settings.Units.toWaterVaporPressure()) * self.Fraction_Nitrogen[self.Mix_Number - 1]

                    self.Ending_Ambient_Pressure = ambient_pressure

                    for i in COMPARTMENT_RANGE:
                        temp_helium_pressure = self.Helium_Pressure[i]
                        temp_nitrogen_pressure = self.Nitrogen_Pressure[i]

                        self.Helium_Pressure[i] = haldane_equation(temp_helium_pressure, inspired_helium_pressure, self.Helium_Time_Constant[i], self.Segment_Time)

                        self.Nitrogen_Pressure[i] = haldane_equation(temp_nitrogen_pressure, inspired_nitrogen_pressure, self.Nitrogen_Time_Constant[i], self.Segment_Time)

                    # END gas_loadings_constant_depth

                    self.output_object.add_dive_profile_entry_ascent(self.Segment_Number, self.Segment_Time, self.Run_Time, self.Mix_Number, profile.depth)
                else:
                    break

                # END profile_code_loop

            # START decompression_loop
            # Purpose:
            # BEGIN PROCESS OF ASCENT AND DECOMPRESSION
            # First, calculate the regeneration of critical radii that takes place over
            # the dive time.  The regeneration time constant has a time scale of weeks
            # so this will have very little impact on dives of normal length, but will
            # have major impact for saturation dives.

            # START nuclear_regeneration

            # Purpose: This subprogram calculates the regeneration of VPM critical
            # radii that takes place over the dive time.  The regeneration time constant
            # has a time scale of weeks so this will have very little impact on dives of
            # normal length, but will have a major impact for saturation dives.

            # First convert the maximum crushing pressure obtained for each compartment
            # to Pascals.  Next, compute the ending radius for helium and nitrogen
            # critical nuclei in each compartment.
            for i in COMPARTMENT_RANGE:
                crushing_pressure_pascals_he = (Max_Crushing_Pressure_He[i] / settings.Units.toUnitsFactor()) * ATM

                crushing_pressure_pascals_n2 = (Max_Crushing_Pressure_N2[i] / settings.Units.toUnitsFactor()) * ATM

                ending_radius_he = 1.0 / (crushing_pressure_pascals_he / (2.0 * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma)) + 1.0 / Adjusted_Critical_Radius_He[i])

                ending_radius_n2 = 1.0 / (crushing_pressure_pascals_n2 / (2.0 * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma)) + 1.0 / Adjusted_Critical_Radius_N2[i])
                # A "regenerated" radius for each nucleus is now calculated based on the
                # regeneration time constant.  This means that after application of
                # crushing pressure and reduction in radius, a nucleus will slowly grow
                # back to its original initial radius over a period of time.  This
                # phenomenon is probabilistic in nature and depends on absolute temperature.
                # It is independent of crushing pressure.
                Regenerated_Radius_He[i] = Adjusted_Critical_Radius_He[i] + (ending_radius_he - Adjusted_Critical_Radius_He[i]) * exp(-self.Run_Time / settings.Regeneration_Time_Constant)

                Regenerated_Radius_N2[i] = Adjusted_Critical_Radius_N2[i] + (ending_radius_n2 - Adjusted_Critical_Radius_N2[i]) * exp(-self.Run_Time / settings.Regeneration_Time_Constant)

                # In order to preserve reference back to the initial critical radii after
                # regeneration, an "adjusted crushing pressure" for the nuclei in each
                # compartment must be computed.  In other words, this is the value of
                # crushing pressure that would have reduced the original nucleus to the
                # to the present radius had regeneration not taken place.  The ratio
                # for adjusting crushing pressure is obtained from algebraic manipulation
                # of the standard VPM equations.  The adjusted crushing pressure, in lieu
                # of the original crushing pressure, is then applied in the VPM Critical
                # Volume Algorithm and the VPM Repetitive Algorithm.

                crush_pressure_adjust_ratio_he = (ending_radius_he * (Adjusted_Critical_Radius_He[i] - Regenerated_Radius_He[i])) / (Regenerated_Radius_He[i] * (Adjusted_Critical_Radius_He[i] - ending_radius_he))

                crush_pressure_adjust_ratio_n2 = (ending_radius_n2 * (Adjusted_Critical_Radius_N2[i] - Regenerated_Radius_N2[i])) / (Regenerated_Radius_N2[i] * (Adjusted_Critical_Radius_N2[i] - ending_radius_n2))

                adj_crush_pressure_he_pascals = crushing_pressure_pascals_he * crush_pressure_adjust_ratio_he
                adj_crush_pressure_n2_pascals = crushing_pressure_pascals_n2 * crush_pressure_adjust_ratio_n2

                Adjusted_Crushing_Pressure_He[i] = (adj_crush_pressure_he_pascals / ATM) * settings.Units.toUnitsFactor()
                Adjusted_Crushing_Pressure_N2[i] = (adj_crush_pressure_n2_pascals / ATM) * settings.Units.toUnitsFactor()

            # END nuclear_regeneration

            #   CALCULATE INITIAL ALLOWABLE GRADIENTS FOR ASCENT
            #   This is based on the maximum effective crushing pressure on critical radii
            #   in each compartment achieved during the dive profile.
            # START self.calc_initial_allowable_gradient()

            # Purpose: This subprogram calculates the initial allowable gradients for
            # helium and nitrogen in each compartment.  These are the gradients that
            # will be used to set the deco ceiling on the first pass through the deco
            # loop.  If the Critical Volume Algorithm is set to False, then these
            # gradients will determine the final deco schedule.  Otherwise, if the
            # Critical Volume Algorithm is set to True, these gradients will be further
            # "relaxed" by the Critical Volume Algorithm subroutine.  The initial
            # allowable gradients are referred to as "PssMin" in the papers by Yount
            # and colleagues, i.e., the minimum supersaturation pressure gradients
            # that will probe bubble formation in the VPM nuclei that started with the
            # designated minimum initial radius (critical radius).

            # The initial allowable gradients are computed directly from the
            # "regenerated" radii after the Nuclear Regeneration subroutine.  These
            # gradients are tracked separately for helium and nitrogen.

            # The initial allowable gradients are computed in Pascals and then converted
            # to the diving pressure units.  Two different sets of arrays are used to
            # save the calculations - Initial Allowable Gradients and Allowable
            # Gradients.  The Allowable Gradients are assigned the values from Initial
            # Allowable Gradients however the Allowable Gradients can be changed later
            # by the Critical Volume subroutine.  The values for the Initial Allowable
            # Gradients are saved in a global array for later use by both the Critical
            # Volume subroutine and the VPM Repetitive Algorithm subroutine.

            for i in COMPARTMENT_RANGE:
                initial_allowable_grad_n2_pa = ((2.0 * settings.Surface_Tension_Gamma * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma)) / (Regenerated_Radius_N2[i] * settings.Skin_Compression_GammaC))
                initial_allowable_grad_he_pa = ((2.0 * settings.Surface_Tension_Gamma * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma)) / (Regenerated_Radius_He[i] * settings.Skin_Compression_GammaC))

                Initial_Allowable_Gradient_N2[i] = (initial_allowable_grad_n2_pa / ATM) * settings.Units.toUnitsFactor()
                Initial_Allowable_Gradient_He[i] = (initial_allowable_grad_he_pa / ATM) * settings.Units.toUnitsFactor()

                self.Allowable_Gradient_He[i] = Initial_Allowable_Gradient_He[i]
                self.Allowable_Gradient_N2[i] = Initial_Allowable_Gradient_N2[i]

            # END self.calc_initial_allowable_gradient()

            #     SAVE VARIABLES AT START OF ASCENT (END OF BOTTOM TIME) SINCE THESE WILL
            #     BE USED LATER TO COMPUTE THE FINAL ASCENT PROFILE THAT IS WRITTEN TO THE
            #     OUTPUT FILE.
            #     The VPM uses an iterative process to compute decompression schedules so
            #     there will be more than one pass through the decompression loop.

            for i in COMPARTMENT_RANGE:
                He_Pressure_Start_of_Ascent[i] = self.Helium_Pressure[i]
                N2_Pressure_Start_of_Ascent[i] = self.Nitrogen_Pressure[i]

            Run_Time_Start_of_Ascent = self.Run_Time
            Segment_Number_Start_of_Ascent = self.Segment_Number

            #     INPUT PARAMETERS TO BE USED FOR STAGED DECOMPRESSION AND SAVE IN ARRAYS.
            #     ASSIGN INITIAL PARAMETERS TO BE USED AT START OF ASCENT
            #     The user has the ability to change mix, ascent rate, and step size in any
            #     combination at any depth during the ascent.

            Number_of_Changes = 0

            for profile in dive.profile_codes:
                if profile.profile_code == ProfileCode.Ascent:
                    Number_of_Changes = profile.number_of_ascent_parameter_changes
                    Depth_Change = [0.0] * Number_of_Changes
                    Mix_Change = [0.0] * Number_of_Changes
                    rate_Change = [0.0] * Number_of_Changes
                    Step_Size_Change = [0.0] * Number_of_Changes

                    for i, ascents in enumerate(profile.ascent_summary):
                        Depth_Change[i] = ascents.starting_depth
                        Mix_Change[i] = ascents.gasmix
                        rate_Change[i] = ascents.rate
                        Step_Size_Change[i] = ascents.step_size

                    starting_depth = Depth_Change[0]
                    self.Mix_Number = Mix_Change[0]
                    rate = rate_Change[0]
                    Step_Size = Step_Size_Change[0]

            # CALCULATE THE DEPTH WHERE THE DECOMPRESSION ZONE BEGINS FOR THIS PROFILE
            # BASED ON THE INITIAL ASCENT PARAMETERS AND WRITE THE DEEPEST POSSIBLE
            # DECOMPRESSION STOP DEPTH TO THE OUTPUT FILE
            # Knowing where the decompression zone starts is very important.  Below
            # that depth there is no possibility for bubble formation because there
            # will be no supersaturation gradients.  Deco stops should never start
            # below the deco zone.  The deepest possible stop deco stop depth is
            # defined as the next "standard" stop depth above the point where the
            # leading compartment enters the deco zone.  Thus, the program will not
            # base this calculation on step sizes larger than 10 fsw or 3 msw.  The
            # deepest possible stop depth is not used in the program, per se, rather
            # it is information to tell the diver where to start putting on the brakes
            # during ascent.  This should be prominently displayed by any deco program.

            Depth_Start_of_Deco_Zone = calc_start_of_deco_zone( self.Fraction_Helium[self.Mix_Number - 1]
                                                              , self.Fraction_Nitrogen[self.Mix_Number - 1]
                                                              , self.Helium_Pressure, self.Nitrogen_Pressure
                                                              , self.Helium_Time_Constant, self.Nitrogen_Time_Constant
                                                              , starting_depth, self.Barometric_Pressure, rate, settings
                                                              )

            # TODO Is this used at all?
            # Deepest_Possible_Stop_Depth = 0.0

            # if settings.Units == UnitsSW.FSW:
                # if Step_Size < 10.0:
                    # rounding_op = (Depth_Start_of_Deco_Zone / Step_Size) - 0.5
                    # Deepest_Possible_Stop_Depth = round(rounding_op) * Step_Size
                # else:
                    # rounding_op = (Depth_Start_of_Deco_Zone / 10.0) - 0.5
                    # Deepest_Possible_Stop_Depth = round(rounding_op) * 10.0

            # else:
                # if Step_Size < 3.0:
                    # rounding_op = (Depth_Start_of_Deco_Zone / Step_Size) - 0.5
                    # Deepest_Possible_Stop_Depth = round(rounding_op) * Step_Size
                # else:
                    # rounding_op = (Depth_Start_of_Deco_Zone / 3.0) - 0.5
                    # Deepest_Possible_Stop_Depth = round(rounding_op) * 3.0

            #     TEMPORARILY ASCEND PROFILE TO THE START OF THE DECOMPRESSION ZONE, SAVE
            #     VARIABLES AT THIS POINT, AND INITIALIZE VARIABLES FOR CRITICAL VOLUME LOOP
            #     The iterative process of the VPM Critical Volume Algorithm will operate
            #     only in the decompression zone since it deals with excess gas volume
            #     released as a result of supersaturation gradients (not possible below the
            #     decompression zone).

            pressures = gas_loadings_ascent_descent(self.Helium_Pressure, self.Nitrogen_Pressure, self.Helium_Time_Constant, self.Nitrogen_Time_Constant, self.Initial_Helium_Pressure, self.Initial_Nitrogen_Pressure, self.Fraction_Helium, self.Fraction_Nitrogen, starting_depth, Depth_Start_of_Deco_Zone, rate, self.Barometric_Pressure, self.Mix_Number, settings)
            self.Initial_Helium_Pressure = pressures[0]
            self.Initial_Nitrogen_Pressure = pressures[1]

            self.Helium_Pressure = pressures[2]
            self.Nitrogen_Pressure = pressures[3]

            self.Segment_Time = pressures[4]
            self.Ending_Ambient_Pressure = profile.ending_depth + self.Barometric_Pressure
            self.Run_Time += pressures[4]
            self.Segment_Number += 1

            Run_Time_Start_of_Deco_Zone = self.Run_Time
            Deco_Phase_Volume_Time = 0.0
            Last_Run_Time = 0.0

            for i in COMPARTMENT_RANGE:
                Last_Phase_Volume_Time[i] = 0.0
                He_Pressure_Start_of_Deco_Zone[i] = self.Helium_Pressure[i]
                N2_Pressure_Start_of_Deco_Zone[i] = self.Nitrogen_Pressure[i]
                Max_Actual_Gradient[i] = 0.0

            # START critical_volume_loop
            # Purpose:
            # If the Critical Volume
            # Algorithm is toggled False in the program settings, there will only be
            # one pass through this loop.  Otherwise, there will be two or more passes
            # through this loop until the deco schedule is "converged" - that is when a
            # comparison between the phase volume time of the present iteration and the
            # last iteration is less than or equal to one minute.  This implies that
            # the volume of released gas in the most recent iteration differs from the
            # "critical" volume limit by an acceptably small amount.  The critical
            # volume limit is set by the Critical Volume Parameter Lambda in the program
            # settings (default setting is 7500 fsw-min with adjustability range from
            # from 6500 to 8300 fsw-min according to Bruce Wienke).

            Schedule_Converged = False

            while True:
                Ascent_Ceiling_Depth = calc_ascent_ceiling( self.Nitrogen_Pressure
                                                          , self.Helium_Pressure
                                                          , self.Allowable_Gradient_N2
                                                          , self.Allowable_Gradient_He
                                                          , self.Barometric_Pressure
                                                          , settings)


                if Ascent_Ceiling_Depth <= 0.0:
                    Deco_Stop_Depth = 0.0
                else:
                    rounding_operation2 = (Ascent_Ceiling_Depth / Step_Size) + 0.5
                    Deco_Stop_Depth = round(rounding_operation2) * Step_Size

                if Deco_Stop_Depth > Depth_Start_of_Deco_Zone:
                    raise DecompressionStepException("ERROR! STEP SIZE IS TOO LARGE TO DECOMPRESS")

                # PERFORM A SEPARATE "PROJECTED ASCENT" OUTSIDE OF THE MAIN PROGRAM TO MAKE
                # SURE THAT AN INCREASE IN GAS LOADINGS DURING ASCENT TO THE FIRST STOP WILL
                # NOT CAUSE A VIOLATION OF THE DECO CEILING.  IF SO, ADJUST THE FIRST STOP
                # DEEPER BASED ON STEP SIZE UNTIL A SAFE ASCENT CAN BE MADE.
                # Note: this situation is a possibility when ascending from extremely deep
                # dives or due to an unusual gas mix selection.
                # CHECK AGAIN TO MAKE SURE THAT ADJUSTED FIRST STOP WILL NOT BE BELOW THE
                # DECO ZONE.

                # START projected_ascent

                # Purpose: This subprogram performs a simulated ascent outside of the main
                # program to ensure that a deco ceiling will not be violated due to unusual
                # gas loading during ascent (on-gassing).  If the deco ceiling is violated,
                # the stop depth will be adjusted deeper by the step size until a safe
                # ascent can be made.

                new_ambient_pressure = Deco_Stop_Depth + self.Barometric_Pressure
                starting_ambient_pressure = Depth_Start_of_Deco_Zone + self.Barometric_Pressure

                initial_inspired_he_pressure = (starting_ambient_pressure - settings.Units.toWaterVaporPressure()) * self.Fraction_Helium[self.Mix_Number - 1]
                initial_inspired_n2_pressure = (starting_ambient_pressure - settings.Units.toWaterVaporPressure()) * self.Fraction_Nitrogen[self.Mix_Number - 1]

                helium_rate = rate * self.Fraction_Helium[self.Mix_Number - 1]
                nitrogen_rate = rate * self.Fraction_Nitrogen[self.Mix_Number - 1]

                temp_gas_loading = [0.0] * NUM_COMPARTMENTS
                allowable_gas_loading = [0.0] * NUM_COMPARTMENTS
                initial_helium_pressure = [0.0] * NUM_COMPARTMENTS
                initial_nitrogen_pressure = [0.0] * NUM_COMPARTMENTS

                for i in COMPARTMENT_RANGE:
                    initial_helium_pressure[i]   = self.Helium_Pressure[i]
                    initial_nitrogen_pressure[i] = self.Nitrogen_Pressure[i]

                keep_going = True
                while keep_going:
                    keep_going = False
                    ending_ambient_pressure = new_ambient_pressure

                    segment_time = (ending_ambient_pressure - starting_ambient_pressure) / rate

                    for i in COMPARTMENT_RANGE:
                        temp_helium_pressure = schreiner_equation(initial_inspired_he_pressure, helium_rate, segment_time, self.Helium_Time_Constant[i], initial_helium_pressure[i])
                        temp_nitrogen_pressure = schreiner_equation(initial_inspired_n2_pressure, nitrogen_rate, segment_time, self.Nitrogen_Time_Constant[i], initial_nitrogen_pressure[i])
                        temp_gas_loading[i] = temp_helium_pressure + temp_nitrogen_pressure

                        if temp_gas_loading[i] > 0.0:
                            weighted_allowable_gradient = (self.Allowable_Gradient_He[i] * temp_helium_pressure + self.Allowable_Gradient_N2[i] * temp_nitrogen_pressure) / temp_gas_loading[i]
                        else:
                            weighted_allowable_gradient = min(self.Allowable_Gradient_He[i], self.Allowable_Gradient_N2[i])

                        allowable_gas_loading[i] = ending_ambient_pressure + weighted_allowable_gradient - settings.Constant_Pressure_Other_Gases

                    for j in COMPARTMENT_RANGE:
                        if temp_gas_loading[j] > allowable_gas_loading[j]:
                            new_ambient_pressure = ending_ambient_pressure + Step_Size
                            Deco_Stop_Depth = Deco_Stop_Depth + Step_Size
                            keep_going = True

                # END projected_ascent

                if Deco_Stop_Depth > Depth_Start_of_Deco_Zone:
                    raise DecompressionStepException("ERROR! STEP SIZE IS TOO LARGE TO DECOMPRESS")

                #     HANDLE THE SPECIAL CASE WHEN NO DECO STOPS ARE REQUIRED - ASCENT CAN BE
                #     MADE DIRECTLY TO THE SURFACE
                #     Write ascent data to output file and exit the Critical Volume Loop.

                if Deco_Stop_Depth == 0.0:
                    for i in COMPARTMENT_RANGE:
                        self.Helium_Pressure[i] = He_Pressure_Start_of_Ascent[i]
                        self.Nitrogen_Pressure[i] = N2_Pressure_Start_of_Ascent[i]

                    self.Run_Time = Run_Time_Start_of_Ascent
                    self.Segment_Number = Segment_Number_Start_of_Ascent
                    starting_depth = Depth_Change[0]
                    ending_depth = 0.0
                    pressures = gas_loadings_ascent_descent(self.Helium_Pressure, self.Nitrogen_Pressure, self.Helium_Time_Constant, self.Nitrogen_Time_Constant, self.Initial_Helium_Pressure, self.Initial_Nitrogen_Pressure, self.Fraction_Helium, self.Fraction_Nitrogen, starting_depth, ending_depth, rate, self.Barometric_Pressure, self.Mix_Number, settings)
                    self.Initial_Helium_Pressure = pressures[0]
                    self.Initial_Nitrogen_Pressure = pressures[1]

                    self.Helium_Pressure = pressures[2]
                    self.Nitrogen_Pressure = pressures[3]

                    self.Segment_Time = pressures[4]
                    self.Ending_Ambient_Pressure = profile.ending_depth + self.Barometric_Pressure
                    self.Run_Time += pressures[4]
                    self.Segment_Number += 1

                    self.output_object.add_decompression_profile_ascent(self.Segment_Number, self.Segment_Time, self.Run_Time, self.Mix_Number, Deco_Stop_Depth, rate)
                    break

                # ASSIGN VARIABLES FOR ASCENT FROM START OF DECO ZONE TO FIRST STOP.  SAVE
                # FIRST STOP DEPTH FOR LATER USE WHEN COMPUTING THE FINAL ASCENT PROFILE

                starting_depth = Depth_Start_of_Deco_Zone
                First_Stop_Depth = Deco_Stop_Depth

                # START deco_stop_loop_block_within_critical_volume_loop

                # Purpose:
                # DECO STOP LOOP BLOCK WITHIN CRITICAL VOLUME LOOP
                # This loop computes a decompression schedule to the surface during each
                # iteration of the critical volume loop.  No output is written from this
                # loop, rather it computes a schedule from which the in-water portion of the
                # total phase volume time (Deco_Phase_Volume_Time) can be extracted.  Also,
                # the gas loadings computed at the end of this loop are used in the subroutine
                # which computes the out-of-water portion of the total phase volume time
                # (Surface_Phase_Volume_Time) for that schedule.

                # Note that exit is made from the loop after last ascent is made to a deco
                # stop depth that is less than or equal to zero.  A final deco stop less
                # than zero can happen when the user makes an odd step size change during
                # ascent - such as specifying a 5 msw step size change at the 3 msw stop!

                while True:
                    pressures = gas_loadings_ascent_descent(self.Helium_Pressure, self.Nitrogen_Pressure, self.Helium_Time_Constant, self.Nitrogen_Time_Constant, self.Initial_Helium_Pressure, self.Initial_Nitrogen_Pressure, self.Fraction_Helium, self.Fraction_Nitrogen, starting_depth, Deco_Stop_Depth, rate, self.Barometric_Pressure, self.Mix_Number, settings)

                    self.Initial_Helium_Pressure = pressures[0]
                    self.Initial_Nitrogen_Pressure = pressures[1]

                    self.Helium_Pressure = pressures[2]
                    self.Nitrogen_Pressure = pressures[3]

                    self.Segment_Time = pressures[4]
                    self.Ending_Ambient_Pressure = profile.ending_depth + self.Barometric_Pressure
                    self.Run_Time += pressures[4]
                    self.Segment_Number += 1

                    if Deco_Stop_Depth <= 0.0: # TODO Bake this condition into the loop
                        break

                    if Number_of_Changes > 1:
                        for i in range(1, Number_of_Changes):
                            if Depth_Change[i] >= Deco_Stop_Depth:
                                self.Mix_Number = Mix_Change[i]
                                rate = rate_Change[i]
                                Step_Size = Step_Size_Change[i]

                    comps = boyles_law_compensation(First_Stop_Depth, Deco_Stop_Depth, Step_Size, self.Allowable_Gradient_He, self.Allowable_Gradient_N2, self.Barometric_Pressure, settings)
                    self.Deco_Gradient_He = comps[0]
                    self.Deco_Gradient_N2 = comps[1]

                    self.decompression_stop(settings, Deco_Stop_Depth, Step_Size)

                    starting_depth = Deco_Stop_Depth
                    Next_Stop = Deco_Stop_Depth - Step_Size
                    Deco_Stop_Depth = Next_Stop
                    Last_Run_Time = self.Run_Time

                # END deco_stop_loop_block_within_critical_volume_loop

                # COMPUTE TOTAL PHASE VOLUME TIME AND MAKE CRITICAL VOLUME COMPARISON
                # The deco phase volume time is computed from the run time.  The surface
                # phase volume time is computed in a subroutine based on the surfacing gas
                # loadings from previous deco loop block.  Next the total phase volume time
                # (in-water + surface) for each compartment is compared against the previous
                # total phase volume time.  The schedule is converged when the difference is
                # less than or equal to 1 minute in any one of the NUM_COMPARTMENTS compartments.

                # Note:  the "phase volume time" is somewhat of a mathematical concept.
                # It is the time divided out of a total integration of supersaturation
                # gradient x time (in-water and surface).  This integration is multiplied
                # by the excess bubble number to represent the amount of free-gas released
                # as a result of allowing a certain number of excess bubbles to form.
                Deco_Phase_Volume_Time = self.Run_Time - Run_Time_Start_of_Deco_Zone

                Surface_Phase_Volume_Time = calc_surface_phase_volume_time( self.Helium_Pressure, self.Nitrogen_Pressure
                                                                          , self.Helium_Time_Constant, self.Nitrogen_Time_Constant
                                                                          , self.Barometric_Pressure, settings)

                for i in COMPARTMENT_RANGE:
                    Phase_Volume_Time[i] = Deco_Phase_Volume_Time + Surface_Phase_Volume_Time[i]
                    Critical_Volume_Comparison = abs(Phase_Volume_Time[i] - Last_Phase_Volume_Time[i])
                    if Critical_Volume_Comparison <= 1.0:
                        Schedule_Converged = True

                # There are two options here.  If the Critical Volume Algorithm setting is
                # True and the schedule is converged, or the Critical Volume Algorithm
                # setting was False in the first place, the program will re-assign variables
                # to their values at the start of ascent (end of bottom time) and process
                # a complete decompression schedule once again using all the same ascent
                # parameters and first stop depth.  This decompression schedule will match
                # the last iteration of the Critical Volume Loop and the program will write
                # the final deco schedule to the output file.

                # Note: if the Critical Volume Algorithm setting was False, the final deco
                # schedule will be based on "Initial Allowable Supersaturation Gradients."
                # If it was True, the final schedule will be based on "Adjusted Allowable
                # Supersaturation Gradients" (gradients that are "relaxed" as a result of
                # the Critical Volume Algorithm).

                # If the Critical Volume Algorithm setting is True and the schedule is not
                # converged, the program will re-assign variables to their values at the
                # start of the deco zone and process another trial decompression schedule.

                if Schedule_Converged or not settings.Critical_Volume_Algorithm:
                    # START critical_volume_decision_tree

                    for i in COMPARTMENT_RANGE:
                        self.Helium_Pressure[i] = He_Pressure_Start_of_Ascent[i]
                        self.Nitrogen_Pressure[i] = N2_Pressure_Start_of_Ascent[i]

                    self.Run_Time = Run_Time_Start_of_Ascent
                    self.Segment_Number = Segment_Number_Start_of_Ascent
                    starting_depth = Depth_Change[0]
                    self.Mix_Number = Mix_Change[0]
                    rate = rate_Change[0]
                    Step_Size = Step_Size_Change[0]
                    Deco_Stop_Depth = First_Stop_Depth
                    Last_Run_Time = 0.0

                    # DECO STOP LOOP BLOCK FOR FINAL DECOMPRESSION SCHEDULE

                    while True:
                        pressures = gas_loadings_ascent_descent(self.Helium_Pressure, self.Nitrogen_Pressure, self.Helium_Time_Constant, self.Nitrogen_Time_Constant, self.Initial_Helium_Pressure, self.Initial_Nitrogen_Pressure, self.Fraction_Helium, self.Fraction_Nitrogen, starting_depth, Deco_Stop_Depth, rate, self.Barometric_Pressure, self.Mix_Number, settings)

                        self.Initial_Helium_Pressure = pressures[0]
                        self.Initial_Nitrogen_Pressure = pressures[1]

                        self.Helium_Pressure = pressures[2]
                        self.Nitrogen_Pressure = pressures[3]

                        self.Segment_Time = pressures[4]
                        self.Ending_Ambient_Pressure = profile.ending_depth + self.Barometric_Pressure
                        self.Run_Time += pressures[4]
                        self.Segment_Number += 1

                        Max_Actual_Gradient = calc_max_actual_gradient(Max_Actual_Gradient, Deco_Stop_Depth, self.Helium_Pressure, self.Nitrogen_Pressure, self.Barometric_Pressure, settings)

                        self.output_object.add_decompression_profile_ascent(self.Segment_Number, self.Segment_Time, self.Run_Time, self.Mix_Number, Deco_Stop_Depth, rate)
                        if Deco_Stop_Depth <= 0.0: # TODO Bake this condition into the loop
                            break

                        if Number_of_Changes > 1:
                            for i in range(1, Number_of_Changes):
                                if Depth_Change[i] >= Deco_Stop_Depth:
                                    self.Mix_Number = Mix_Change[i]
                                    rate = rate_Change[i]
                                    Step_Size = Step_Size_Change[i]

                        comps = boyles_law_compensation(First_Stop_Depth, Deco_Stop_Depth, Step_Size, self.Allowable_Gradient_He, self.Allowable_Gradient_N2, self.Barometric_Pressure, settings)
                        self.Deco_Gradient_He = comps[0]
                        self.Deco_Gradient_N2 = comps[1]

                        self.decompression_stop(settings, Deco_Stop_Depth, Step_Size)
                        # This next bit just rounds up the stop time at the first stop to be in
                        # whole increments of the minimum stop time (to make for a nice deco table).

                        if Last_Run_Time == 0.0:
                            Stop_Time = round((self.Segment_Time / settings.Minimum_Deco_Stop_Time) + 0.5) * settings.Minimum_Deco_Stop_Time
                        else:
                            Stop_Time = self.Run_Time - Last_Run_Time

                        # DURING FINAL DECOMPRESSION SCHEDULE, IF MINIMUM STOP TIME PARAMETER IS A
                        # WHOLE NUMBER (i.e. 1 minute) THEN WRITE DECO SCHEDULE USING INTEGER
                        # NUMBERS (looks nicer).  OTHERWISE, USE DECIMAL NUMBERS.
                        # Note: per the request of a noted exploration diver(!), program now allows
                        # a minimum stop time of less than one minute so that total ascent time can
                        # be minimized on very long dives.  In fact, with step size set at 1 fsw or
                        # 0.2 msw and minimum stop time set at 0.1 minute (6 seconds), a near
                        # continuous decompression schedule can be computed.

                        if trunc(settings.Minimum_Deco_Stop_Time) == settings.Minimum_Deco_Stop_Time:
                            self.output_object.add_decompression_profile_constant(self.Segment_Number, self.Segment_Time, self.Run_Time, self.Mix_Number, int(Deco_Stop_Depth), int(Stop_Time))
                        else:
                            self.output_object.add_decompression_profile_constant(self.Segment_Number, self.Segment_Time, self.Run_Time, self.Mix_Number, Deco_Stop_Depth, Stop_Time)

                        starting_depth = Deco_Stop_Depth
                        Next_Stop = Deco_Stop_Depth - Step_Size
                        Deco_Stop_Depth = Next_Stop
                        Last_Run_Time = self.Run_Time

                    # END critical_volume_decision_tree

                else:
                    crit = critical_volume( self.Allowable_Gradient_He, self.Allowable_Gradient_N2
                                          , Deco_Phase_Volume_Time, Surface_Phase_Volume_Time
                                          , Adjusted_Crushing_Pressure_He, Adjusted_Crushing_Pressure_N2
                                          , Initial_Allowable_Gradient_He, Initial_Allowable_Gradient_N2
                                          , settings)
                    self.Allowable_Gradient_He = crit[0]
                    self.Allowable_Gradient_N2 = crit[1]

                    Deco_Phase_Volume_Time = 0.0
                    self.Run_Time = Run_Time_Start_of_Deco_Zone
                    starting_depth = Depth_Start_of_Deco_Zone
                    self.Mix_Number = Mix_Change[0]
                    rate = rate_Change[0]
                    Step_Size = Step_Size_Change[0]
                    for i in COMPARTMENT_RANGE:
                        Last_Phase_Volume_Time[i] = Phase_Volume_Time[i]
                        self.Helium_Pressure[i] = He_Pressure_Start_of_Deco_Zone[i]
                        self.Nitrogen_Pressure[i] = N2_Pressure_Start_of_Deco_Zone[i]
                    continue
                break
            # END critical_volume_loop
            # END decompression_loop


            # IF THERE IS A REPETITIVE DIVE, COMPUTE GAS LOADINGS (OFF-GASSING) DURING
            # SURFACE INTERVAL TIME.  ADJUST CRITICAL RADII USING VPM REPETITIVE
            # ALGORITHM.  RE-INITIALIZE SELECTED VARIABLES AND RETURN TO START OF
            # REPETITIVE LOOP AT LINE 30.
            if dive.repetitive_code:
                # START gas_loadings_surface_interval

                # Purpose: This subprogram calculates the gas loading (off-gassing) during
                # a surface interval.

                inspired_helium_pressure = 0.0
                inspired_nitrogen_pressure = (self.Barometric_Pressure - settings.Units.toWaterVaporPressure()) * SURFACE_FRACTION_INERT_GAS

                for i in COMPARTMENT_RANGE:
                    temp_helium_pressure = self.Helium_Pressure[i]
                    temp_nitrogen_pressure = self.Nitrogen_Pressure[i]

                    self.Helium_Pressure[i] = haldane_equation(temp_helium_pressure, inspired_helium_pressure, self.Helium_Time_Constant[i], dive.surface_interval_time_minutes)
                    self.Nitrogen_Pressure[i] = haldane_equation(temp_nitrogen_pressure, inspired_nitrogen_pressure, self.Nitrogen_Time_Constant[i], dive.surface_interval_time_minutes)

                # END gas_loadings_surface_interval

                # START vpm_repetitive_algorithm

                # Purpose: This subprogram implements the VPM Repetitive Algorithm that was
                # envisioned by Professor David E. Yount only months before his passing.

                for i in COMPARTMENT_RANGE:
                    max_actual_gradient_pascals = (Max_Actual_Gradient[i] / settings.Units.toUnitsFactor()) * ATM

                    adj_crush_pressure_he_pascals = (Adjusted_Crushing_Pressure_He[i] / settings.Units.toUnitsFactor()) * ATM
                    adj_crush_pressure_n2_pascals = (Adjusted_Crushing_Pressure_N2[i] / settings.Units.toUnitsFactor()) * ATM

                    if Max_Actual_Gradient[i] > Initial_Allowable_Gradient_N2[i]:
                        new_critical_radius_n2 = new_critical_radius(settings, max_actual_gradient_pascals, adj_crush_pressure_n2_pascals)

                        Adjusted_Critical_Radius_N2[i] = Initial_Critical_Radius_N2[i] + (Initial_Critical_Radius_N2[i] - new_critical_radius_n2) * exp(-dive.surface_interval_time_minutes / settings.Regeneration_Time_Constant)

                    else:
                        Adjusted_Critical_Radius_N2[i] = Initial_Critical_Radius_N2[i]

                    if Max_Actual_Gradient[i] > Initial_Allowable_Gradient_He[i]:
                        new_critical_radius_he = new_critical_radius(settings, max_actual_gradient_pascals, adj_crush_pressure_he_pascals)

                        Adjusted_Critical_Radius_He[i] = Initial_Critical_Radius_He[i] + (Initial_Critical_Radius_He[i] - new_critical_radius_he) * exp(-dive.surface_interval_time_minutes / settings.Regeneration_Time_Constant)
                    else:
                        Adjusted_Critical_Radius_He[i] = Initial_Critical_Radius_He[i]

                # END vpm_repetitive_algorithm

                for i in COMPARTMENT_RANGE:
                    Max_Crushing_Pressure_He[i] = 0.0
                    Max_Crushing_Pressure_N2[i] = 0.0
                    Max_Actual_Gradient[i] = 0.0

                self.Run_Time = 0.0
                self.Segment_Number = 0

# functions

# CALCULATE INITIAL ASCENT CEILING BASED ON ALLOWABLE SUPERSATURATION
# GRADIENTS AND SET FIRST DECO STOP.  CHECK TO MAKE SURE THAT SELECTED STEP
# SIZE WILL NOT ROUND UP FIRST STOP TO A DEPTH THAT IS BELOW THE DECO ZONE.

# Purpose: This subprogram calculates the ascent ceiling (the safe ascent
# depth) in each compartment, based on the allowable gradients, and then
# finds the deepest ascent ceiling across all compartments.
def calc_ascent_ceiling(helium_pressures, nitrogen_pressures, helium_gradients, nitrogen_gradients, barometric_pressure, settings):
    def _ascent_ceiling_depth(t):
        helium_pressure   = t[0]
        nitrogen_pressure = t[1]
        helium_gradient   = t[2]
        nitrogen_gradient = t[3]

        gas_loading = helium_pressure + nitrogen_pressure

        if gas_loading > 0.0:
            weighted_allowable_gradient = (helium_gradient * helium_pressure + nitrogen_gradient * nitrogen_pressure) / (helium_pressure + nitrogen_pressure)
            tolerated_ambient_pressure = (gas_loading + settings.Constant_Pressure_Other_Gases) - weighted_allowable_gradient

        else:
            weighted_allowable_gradient = min(helium_gradient, nitrogen_gradient)
            tolerated_ambient_pressure = settings.Constant_Pressure_Other_Gases - weighted_allowable_gradient

        #     The tolerated ambient pressure cannot be less than zero absolute, i.e.,
        #     the vacuum of outer space!
        if tolerated_ambient_pressure < 0.0:
            tolerated_ambient_pressure = 0.0

        #     The Ascent Ceiling Depth is computed in a loop after all of the individual
        #     compartment ascent ceilings have been calculated.  It is important that
        #     the Ascent Ceiling Depth (max ascent ceiling across all compartments) only
        #     be extracted from the compartment values and not be compared against some
        #     initialization value.  For example, if MAX(Ascent_Ceiling_Depth . .) was
        #     compared against zero, this could cause a program lockup because sometimes
        #     the Ascent Ceiling Depth needs to be negative (but not less than zero
        #     absolute ambient pressure) in order to decompress to the last stop at zero
        #     depth.

        return tolerated_ambient_pressure - barometric_pressure

    return max(map(_ascent_ceiling_depth
                  , zip(helium_pressures, nitrogen_pressures, helium_gradients, nitrogen_gradients)))


def boyles_law_compensation(first_stop_depth, deco_stop_depth, step_size, allowable_gradients_he, allowable_gradients_n2, barometric_pressure, settings):
    """
    Purpose: This subprogram calculates the reduction in allowable gradients
    with decreasing ambient pressure during the decompression profile based
    on Boyle's Law considerations.
    """
    deco_gradients_he = [0.0] * NUM_COMPARTMENTS
    deco_gradients_n2 = [0.0] * NUM_COMPARTMENTS

    next_stop = deco_stop_depth - step_size
    ambient_pressure_first_stop = first_stop_depth + barometric_pressure
    ambient_pressure_next_stop = next_stop + barometric_pressure

    amb_press_first_stop_pascals = (ambient_pressure_first_stop / settings.Units.toUnitsFactor()) * ATM
    amb_press_next_stop_pascals = (ambient_pressure_next_stop / settings.Units.toUnitsFactor()) * ATM

    for i in COMPARTMENT_RANGE:
        # Helium Calculation
        deco_gradients_he[i] = calculate_deco_gradient(settings, allowable_gradients_he[i], amb_press_first_stop_pascals, amb_press_next_stop_pascals)

        # Nitrogen Calculation
        deco_gradients_n2[i] = calculate_deco_gradient(settings, allowable_gradients_n2[i], amb_press_first_stop_pascals, amb_press_next_stop_pascals)

    return (deco_gradients_he, deco_gradients_n2)


def gas_loadings_ascent_descent(helium_pressure, nitrogen_pressure, helium_time_constant, nitrogen_time_constant, initial_helium_pressure, initial_nitrogen_pressure, fraction_helium, fraction_nitrogen, starting_depth, ending_depth, rate, barometric_pressure, mix_number, settings):
    """
     Purpose: This subprogram applies the Schreiner equation to update the
     gas loadings (partial pressures of helium and nitrogen) in the half-time
     compartments due to a linear ascent or descent segment at a constant rate.
     """
    segment_time = float(ending_depth - starting_depth) / rate

    starting_ambient_pressure = starting_depth + barometric_pressure
    initial_inspired_he_pressure = (starting_ambient_pressure - settings.Units.toWaterVaporPressure()) * fraction_helium[mix_number - 1]
    initial_inspired_n2_pressure = (starting_ambient_pressure - settings.Units.toWaterVaporPressure()) * fraction_nitrogen[mix_number - 1]
    helium_rate   = rate * fraction_helium[mix_number - 1]
    nitrogen_rate = rate * fraction_nitrogen[mix_number - 1]

    new_initial_helium_pressure   = [0.0] * NUM_COMPARTMENTS
    new_initial_nitrogen_pressure = [0.0] * NUM_COMPARTMENTS

    new_helium_pressure   = [0.0] * NUM_COMPARTMENTS
    new_nitrogen_pressure = [0.0] * NUM_COMPARTMENTS

    for i in COMPARTMENT_RANGE:
        new_initial_helium_pressure[i]   = helium_pressure[i]
        new_initial_nitrogen_pressure[i] = nitrogen_pressure[i]

        new_helium_pressure[i]   = schreiner_equation(initial_inspired_he_pressure, helium_rate,   segment_time, helium_time_constant[i],   helium_pressure[i])
        new_nitrogen_pressure[i] = schreiner_equation(initial_inspired_n2_pressure, nitrogen_rate, segment_time, nitrogen_time_constant[i], nitrogen_pressure[i])

    return (new_initial_helium_pressure, new_initial_nitrogen_pressure, new_helium_pressure, new_nitrogen_pressure, segment_time)

def calc_max_actual_gradient(max_actual_gradient, deco_stop_depth, helium_pressure, nitrogen_pressure, barometric_pressure, settings):
  # DURING FINAL DECOMPRESSION SCHEDULE PROCESS, COMPUTE MAXIMUM ACTUAL
  # SUPERSATURATION GRADIENT RESULTING IN EACH COMPARTMENT
  # If there is a repetitive dive, this will be used later in the VPM
  # Repetitive Algorithm to adjust the values for critical radii.

  # Purpose: This subprogram calculates the actual supersaturation gradient
  # obtained in each compartment as a result of the ascent profile during
  # decompression.  Similar to the concept with crushing pressure, the
  # supersaturation gradients are not cumulative over a multi-level, staged
  # ascent.  Rather, it will be the maximum value obtained in any one discrete
  # step of the overall ascent.  Thus, the program must compute and store the
  # maximum actual gradient for each compartment that was obtained across all
  # steps of the ascent profile.  This subroutine is invoked on the last pass
  # through the deco stop loop block when the final deco schedule is being
  # generated.

  # The max actual gradients are later used by the VPM Repetitive Algorithm to
  # determine if adjustments to the critical radii are required.  If the max
  # actual gradient did not exceed the initial allowable gradient, then no
  # adjustment will be made.  However, if the max actual gradient did exceed
  # the initial allowable gradient, such as permitted by the Critical Volume
  # Algorithm, then the critical radius will be adjusted (made larger) on the
  # repetitive dive to compensate for the bubbling that was allowed on the
  # previous dive.  The use of the max actual gradients is intended to prevent
  # the repetitive algorithm from being overly conservative.

  # Note: negative supersaturation gradients are meaningless for this
  # application, so the values must be equal to or greater than zero.

  for i in COMPARTMENT_RANGE:
      compartment_gradient = (helium_pressure[i] + nitrogen_pressure[i] + settings.Constant_Pressure_Other_Gases) - (deco_stop_depth + barometric_pressure)
      if compartment_gradient <= 0.0:
          compartment_gradient = 0.0

      max_actual_gradient[i] = max(max_actual_gradient[i], compartment_gradient)

  return max_actual_gradient


def calc_deco_ceiling(helium_pressure, nitrogen_pressure, deco_gradient_he, deco_gradient_n2, barometric_pressure, settings):
  # Purpose: This subprogram calculates the deco ceiling (the safe ascent
  # depth) in each compartment, based on the allowable "deco gradients"
  # computed in the Boyle's Law Compensation subroutine, and then finds the
  # deepest deco ceiling across all compartments.  This deepest value
  # (Deco Ceiling Depth) is then used by the Decompression Stop subroutine
  # to determine the actual deco schedule.

  # Since there are two sets of deco gradients being tracked, one for
  # helium and one for nitrogen, a "weighted allowable gradient" must be
  # computed each time based on the proportions of helium and nitrogen in
  # each compartment.  This proportioning follows the methodology of
  # Buhlmann/Keller.  If there is no helium and nitrogen in the compartment,
  # such as after extended periods of oxygen breathing, then the minimum value
  # across both gases will be used.  It is important to note that if a
  # compartment is empty of helium and nitrogen, then the weighted allowable
  # gradient formula cannot be used since it will result in division by zero.

  compartment_deco_ceiling = [0.0] * NUM_COMPARTMENTS

  gas_loading = 0.0
  tolerated_ambient_pressure = 0.0
  weighted_allowable_gradient = 0.0

  for i in COMPARTMENT_RANGE:
      gas_loading = helium_pressure[i] + nitrogen_pressure[i]

      if gas_loading > 0.0:
          weighted_allowable_gradient = (deco_gradient_he[i] * helium_pressure[i] + deco_gradient_n2[i] * nitrogen_pressure[i]) / (helium_pressure[i] + nitrogen_pressure[i])
          tolerated_ambient_pressure = (gas_loading + settings.Constant_Pressure_Other_Gases) - weighted_allowable_gradient
      else:
          weighted_allowable_gradient = min(deco_gradient_he[i], deco_gradient_n2[i])
          tolerated_ambient_pressure = settings.Constant_Pressure_Other_Gases - weighted_allowable_gradient

      # The tolerated ambient pressure cannot be less than zero absolute, i.e.,
      # the vacuum of outer space!
      if tolerated_ambient_pressure < 0.0:
          tolerated_ambient_pressure = 0.0

      # The Deco Ceiling Depth is computed in a loop after all of the individual
      # compartment deco ceilings have been calculated.  It is important that the
      # Deco Ceiling Depth (max deco ceiling across all compartments) only be
      # extracted from the compartment values and not be compared against some
      # initialization value.  For example, if MAX(deco_ceiling_depth . .) was
      # compared against zero, this could cause a program lockup because sometimes
      # the Deco Ceiling Depth needs to be negative (but not less than absolute
      # zero) in order to decompress to the last stop at zero depth.

      compartment_deco_ceiling[i] = tolerated_ambient_pressure - barometric_pressure

  return max(compartment_deco_ceiling)


def calc_surface_phase_volume_time( helium_pressure, nitrogen_pressure
                                  , helium_time_constant, nitrogen_time_constant
                                  , barometric_pressure, settings):
  # Purpose: This subprogram computes the surface portion of the total phase
  # volume time.  This is the time factored out of the integration of
  # supersaturation gradient x time over the surface interval.  The VPM
  # considers the gradients that allow bubbles to form or to drive bubble
  # growth both in the water and on the surface after the dive.

  # This subroutine is a new development to the VPM algorithm in that it
  # computes the time course of supersaturation gradients on the surface
  # when both helium and nitrogen are present.  Refer to separate write-up
  # for a more detailed explanation of this algorithm.

  Surface_Phase_Volume_Time = [0.0] * NUM_COMPARTMENTS

  surface_inspired_n2_pressure = (barometric_pressure - settings.Units.toWaterVaporPressure()) * SURFACE_FRACTION_INERT_GAS
  for i in COMPARTMENT_RANGE:
      if nitrogen_pressure[i] > surface_inspired_n2_pressure:
          Surface_Phase_Volume_Time[i] = (helium_pressure[i] / helium_time_constant[i] + (nitrogen_pressure[i] - surface_inspired_n2_pressure) / nitrogen_time_constant[i]) / (helium_pressure[i] + nitrogen_pressure[i] - surface_inspired_n2_pressure)
      elif (nitrogen_pressure[i] <= surface_inspired_n2_pressure) and (helium_pressure[i] + nitrogen_pressure[i] >= surface_inspired_n2_pressure):
          decay_time_to_zero_gradient = 1.0 / (nitrogen_time_constant[i] - helium_time_constant[i]) * log((surface_inspired_n2_pressure - nitrogen_pressure[i]) / helium_pressure[i])
          integral_gradient_x_time = helium_pressure[i] / helium_time_constant[i] * (1.0 - exp(-helium_time_constant[i] * decay_time_to_zero_gradient)) + (nitrogen_pressure[i] - surface_inspired_n2_pressure) / nitrogen_time_constant[i] * (1.0 - exp(-nitrogen_time_constant[i] * decay_time_to_zero_gradient))
          Surface_Phase_Volume_Time[i] = integral_gradient_x_time / (helium_pressure[i] + nitrogen_pressure[i] - surface_inspired_n2_pressure)
      else:
          Surface_Phase_Volume_Time[i] = 0.0

  return Surface_Phase_Volume_Time

def onset_of_impermeability( starting_ambient_pressure, ending_ambient_pressure
                           , fraction_helium, fraction_nitrogen
                           , initial_helium_pressure, initial_nitrogen_pressure
                           , helium_time_constant, nitrogen_time_constant
                           , profile, settings):
    # Purpose:  This subroutine uses the Bisection Method to find the ambient
    # pressure and gas tension at the onset of impermeability for a given
    # compartment.  Source:  "Numerical Recipes in Fortran 77",
    # Cambridge University Press, 1992.

    # First convert the Gradient for Onset of Impermeability to the diving
    # pressure units that are being used

    gradient_onset_of_imperm = settings.Gradient_Onset_of_Imperm_Atm * settings.Units.toUnitsFactor()

    # ESTABLISH THE BOUNDS FOR THE ROOT SEARCH USING THE BISECTION METHOD
    # In this case, we are solving for time - the time when the ambient pressure
    # minus the gas tension will be equal to the Gradient for Onset of
    # Impermeability.  The low bound for time is set at zero and the high
    # bound is set at the elapsed time (segment time) it took to go from the
    # starting ambient pressure to the ending ambient pressure.  The desired
    # ambient pressure and gas tension at the onset of impermeability will
    # be found somewhere between these endpoints.  The algorithm checks to
    # make sure that the solution lies in between these bounds by first
    # computing the low bound and high bound function values.

    initial_inspired_he_pressure = (starting_ambient_pressure - settings.Units.toWaterVaporPressure()) * fraction_helium

    initial_inspired_n2_pressure = (starting_ambient_pressure - settings.Units.toWaterVaporPressure()) * fraction_nitrogen

    helium_rate = profile.rate * fraction_helium
    nitrogen_rate = profile.rate *  fraction_nitrogen
    low_bound = 0.0

    high_bound = (ending_ambient_pressure - starting_ambient_pressure) / profile.rate

    starting_gas_tension = initial_helium_pressure + initial_nitrogen_pressure + settings.Constant_Pressure_Other_Gases

    function_at_low_bound = starting_ambient_pressure - starting_gas_tension - gradient_onset_of_imperm

    high_bound_helium_pressure = schreiner_equation(initial_inspired_he_pressure, helium_rate, high_bound, helium_time_constant, initial_helium_pressure)

    high_bound_nitrogen_pressure = schreiner_equation(initial_inspired_n2_pressure, nitrogen_rate, high_bound, nitrogen_time_constant, initial_nitrogen_pressure)

    ending_gas_tension = high_bound_helium_pressure + high_bound_nitrogen_pressure + settings.Constant_Pressure_Other_Gases

    function_at_high_bound = ending_ambient_pressure - ending_gas_tension - gradient_onset_of_imperm

    if(function_at_high_bound * function_at_low_bound) >= 0.0:
        raise RootException("ERROR! ROOT IS NOT WITHIN BRACKETS")

    # APPLY THE BISECTION METHOD IN SEVERAL ITERATIONS UNTIL A SOLUTION WITH
    # THE DESIRED ACCURACY IS FOUND
    # Note: the program allows for up to 100 iterations.  Normally an exit will
    # be made from the loop well before that number.  If, for some reason, the
    # program exceeds 100 iterations, there will be a pause to alert the user.

    if function_at_low_bound < 0.0:
        time = low_bound
        differential_change = high_bound - low_bound
    else:
        time = high_bound
        differential_change = low_bound - high_bound

    for j in range(100):
        last_diff_change = differential_change
        differential_change = last_diff_change * 0.5
        mid_range_time = time + differential_change

        mid_range_ambient_pressure = (starting_ambient_pressure + profile.rate * mid_range_time)

        mid_range_helium_pressure = schreiner_equation(initial_inspired_he_pressure, helium_rate, mid_range_time, helium_time_constant, initial_helium_pressure)

        mid_range_nitrogen_pressure = schreiner_equation(initial_inspired_n2_pressure, nitrogen_rate, mid_range_time, nitrogen_time_constant, initial_nitrogen_pressure)

        gas_tension_at_mid_range = mid_range_helium_pressure + mid_range_nitrogen_pressure + settings.Constant_Pressure_Other_Gases

        function_at_mid_range = mid_range_ambient_pressure - gas_tension_at_mid_range - gradient_onset_of_imperm

        if function_at_mid_range <= 0.0:
            time = mid_range_time

        # When a solution with the desired accuracy is found, the program breaks
        if (abs(differential_change) < 1.0E-3) or (function_at_mid_range == 0.0):
            break

    return (mid_range_ambient_pressure, gas_tension_at_mid_range)


def calc_start_of_deco_zone( Fraction_Helium, Fraction_Nitrogen
                           , Helium_Pressure, Nitrogen_Pressure
                           , Helium_Time_Constant, Nitrogen_Time_Constant
                           , starting_depth, barometric_pressure, rate
                           , settings
                           ):
    # Purpose: This subroutine uses the Bisection Method to find the depth at
    # which the leading compartment just enters the decompression zone.
    # Source:  "Numerical Recipes in Fortran 77", Cambridge University Press,
    # 1992.

    # First initialize some variables
    Depth_Start_of_Deco_Zone = 0.0
    starting_ambient_pressure = starting_depth + barometric_pressure

    initial_inspired_he_pressure = (starting_ambient_pressure - settings.Units.toWaterVaporPressure()) * Fraction_Helium
    initial_inspired_n2_pressure = (starting_ambient_pressure - settings.Units.toWaterVaporPressure()) * Fraction_Nitrogen

    helium_rate = rate * Fraction_Helium
    nitrogen_rate = rate * Fraction_Nitrogen

    # ESTABLISH THE BOUNDS FOR THE ROOT SEARCH USING THE BISECTION METHOD
    # AND CHECK TO MAKE SURE THAT THE ROOT WILL BE WITHIN BOUNDS.  PROCESS
    # EACH COMPARTMENT INDIVIDUALLY AND FIND THE MAXIMUM DEPTH ACROSS ALL
    # COMPARTMENTS (LEADING COMPARTMENT)
    # In this case, we are solving for time - the time when the gas tension in
    # the compartment will be equal to ambient pressure.  The low bound for time
    # is set at zero and the high bound is set at the time it would take to
    # ascend to zero ambient pressure (absolute).  Since the ascent rate is
    # negative, a multiplier of -1.0 is used to make the time positive.  The
    # desired point when gas tension equals ambient pressure is found at a time
    # somewhere between these endpoints.  The algorithm checks to make sure that
    # the solution lies in between these bounds by first computing the low bound
    # and high bound function values.

    low_bound = 0.0
    high_bound = -1.0 * (starting_ambient_pressure / rate)

    for i in COMPARTMENT_RANGE:
        initial_helium_pressure = Helium_Pressure[i]
        initial_nitrogen_pressure = Nitrogen_Pressure[i]

        function_at_low_bound = initial_helium_pressure + initial_nitrogen_pressure + settings.Constant_Pressure_Other_Gases - starting_ambient_pressure
        high_bound_helium_pressure = schreiner_equation(initial_inspired_he_pressure, helium_rate, high_bound, Helium_Time_Constant[i], initial_helium_pressure)
        high_bound_nitrogen_pressure = schreiner_equation(initial_inspired_n2_pressure, nitrogen_rate, high_bound, Nitrogen_Time_Constant[i], initial_nitrogen_pressure)
        function_at_high_bound = high_bound_helium_pressure + high_bound_nitrogen_pressure + settings.Constant_Pressure_Other_Gases

        if (function_at_high_bound * function_at_low_bound) >= 0.0:
            raise RootException("ERROR! ROOT IS NOT WITHIN BRACKETS")

        # APPLY THE BISECTION METHOD IN SEVERAL ITERATIONS UNTIL A SOLUTION WITH
        # THE DESIRED ACCURACY IS FOUND
        # Note: the program allows for up to 100 iterations.  Normally an exit will
        # be made from the loop well before that number.  If, for some reason, the
        # program exceeds 100 iterations, there will be a pause to alert the user.

        if function_at_low_bound < 0.0:
            time_to_start_of_deco_zone = low_bound
            differential_change = high_bound - low_bound
        else:
            time_to_start_of_deco_zone = high_bound
            differential_change = low_bound - high_bound

        for j in range(100):
            last_diff_change = differential_change
            differential_change = last_diff_change * 0.5

            mid_range_time = time_to_start_of_deco_zone + differential_change
            mid_range_helium_pressure = schreiner_equation(initial_inspired_he_pressure, helium_rate, mid_range_time, Helium_Time_Constant[i], initial_helium_pressure)
            mid_range_nitrogen_pressure = schreiner_equation(initial_inspired_n2_pressure, nitrogen_rate, mid_range_time, Nitrogen_Time_Constant[i], initial_nitrogen_pressure)
            function_at_mid_range = mid_range_helium_pressure + mid_range_nitrogen_pressure + settings.Constant_Pressure_Other_Gases - (starting_ambient_pressure + rate * mid_range_time)

            if function_at_mid_range <= 0.0:
                time_to_start_of_deco_zone = mid_range_time

            if abs(differential_change) < 1.0E-3 or function_at_mid_range == 0.0:
                break

            if j == 100:
                raise MaxIterationException('ERROR! ROOT SEARCH EXCEEDED MAXIMUM ITERATIONS')

        # When a solution with the desired accuracy is found, the program jumps out
        # of the loop to Line 170 and assigns the solution value for the individual
        # compartment.

        cpt_depth_start_of_deco_zone = (starting_ambient_pressure + rate * time_to_start_of_deco_zone) - barometric_pressure
        # The overall solution will be the compartment with the maximum depth where
        # gas tension equals ambient pressure (leading compartment).

        Depth_Start_of_Deco_Zone = max(Depth_Start_of_Deco_Zone, cpt_depth_start_of_deco_zone)

    return Depth_Start_of_Deco_Zone

def calc_crushing_pressure( Initial_Helium_Pressure, Initial_Nitrogen_Pressure
                          , Helium_Pressure, Nitrogen_Pressure
                          , Helium_Time_Constant, Nitrogen_Time_Constant
                          , Adjusted_Critical_Radius_He, Adjusted_Critical_Radius_N2
                          , Max_Crushing_Pressure_He, Max_Crushing_Pressure_N2
                          , Amb_Pressure_Onset_of_Imperm, Gas_Tension_Onset_of_Imperm
                          , Fraction_Helium, Fraction_Nitrogen
                          , Barometric_Pressure, profile, settings):
    # Purpose: Compute the effective "crushing pressure" in each compartment as
    # a result of descent segment(s).  The crushing pressure is the gradient
    # (difference in pressure) between the outside ambient pressure and the
    # gas tension inside a VPM nucleus (bubble seed).  This gradient acts to
    # reduce (shrink) the radius smaller than its initial value at the surface.
    # This phenomenon has important ramifications because the smaller the radius
    # of a VPM nucleus, the greater the allowable supersaturation gradient upon
    # ascent.  Gas loading (uptake) during descent, especially in the fast
    # compartments, will reduce the magnitude of the crushing pressure.  The
    # crushing pressure is not cumulative over a multi-level descent.  It will
    # be the maximum value obtained in any one discrete segment of the overall
    # descent.  Thus, the program must compute and store the maximum crushing
    # pressure for each compartment that was obtained across all segments of
    # the descent profile.

    # The calculation of crushing pressure will be different depending on
    # whether or not the gradient is in the VPM permeable range (gas can diffuse
    # across skin of VPM nucleus) or the VPM impermeable range (molecules in
    # skin of nucleus are squeezed together so tight that gas can no longer
    # diffuse in or out of nucleus; the gas becomes trapped and further resists
    # the crushing pressure).  The solution for crushing pressure in the VPM
    # permeable range is a simple linear equation.  In the VPM impermeable
    # range, a cubic equation must be solved using a numerical method.

    # Separate crushing pressures are tracked for helium and nitrogen because
    # they can have different critical radii.  The crushing pressures will be
    # the same for helium and nitrogen in the permeable range of the model, but
    # they will start to diverge in the impermeable range.  This is due to
    # the differences between starting radius, radius at the onset of
    # impermeability, and radial compression in the impermeable range.

    # First, convert the Gradient for Onset of Impermeability from units of
    # atmospheres to diving pressure units (either fsw or msw) and to Pascals
    # (SI units).  The reason that the Gradient for Onset of Impermeability is
    # given in the program settings in units of atmospheres is because that is
    # how it was reported in the original research papers by Yount and
    # colleagues.

    gradient_onset_of_imperm = settings.Gradient_Onset_of_Imperm_Atm * settings.Units.toUnitsFactor()  # convert to diving units
    gradient_onset_of_imperm_pa = settings.Gradient_Onset_of_Imperm_Atm * ATM     # convert to Pascals

    # Assign values of starting and ending ambient pressures for descent segment

    starting_ambient_pressure = profile.starting_depth + Barometric_Pressure
    ending_ambient_pressure = profile.ending_depth + Barometric_Pressure

    # MAIN LOOP WITH NESTED DECISION TREE
    # For each compartment, the program computes the starting and ending
    # gas tensions and gradients.  The VPM is different than some dissolved gas
    # algorithms, Buhlmann for example, in that it considers the pressure due to
    # oxygen, carbon dioxide, and water vapor in each compartment in addition to
    # the inert gases helium and nitrogen.  These "other gases" are included in
    # the calculation of gas tensions and gradients.

    for i in COMPARTMENT_RANGE:
        starting_gas_tension = Initial_Helium_Pressure[i] + Initial_Nitrogen_Pressure[i] + settings.Constant_Pressure_Other_Gases

        starting_gradient = starting_ambient_pressure - starting_gas_tension

        ending_gas_tension = Helium_Pressure[i] + Nitrogen_Pressure[i] + settings.Constant_Pressure_Other_Gases

        ending_gradient = ending_ambient_pressure - ending_gas_tension

        # Compute radius at onset of impermeability for helium and nitrogen
        # critical radii

        radius_onset_of_imperm_he = 1.0 / (gradient_onset_of_imperm_pa / (2.0 * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma)) + 1.0 / Adjusted_Critical_Radius_He[i])

        radius_onset_of_imperm_n2 = 1.0 / (gradient_onset_of_imperm_pa / (2.0 * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma)) + 1.0 / Adjusted_Critical_Radius_N2[i])

        # FIRST BRANCH OF DECISION TREE - PERMEABLE RANGE
        # Crushing pressures will be the same for helium and nitrogen
        if ending_gradient <= gradient_onset_of_imperm:
            crushing_pressure_he = ending_ambient_pressure - ending_gas_tension
            crushing_pressure_n2 = ending_ambient_pressure - ending_gas_tension

        # SECOND BRANCH OF DECISION TREE - IMPERMEABLE RANGE
        # Both the ambient pressure and the gas tension at the onset of
        # impermeability must be computed in order to properly solve for the ending
        # radius and resultant crushing pressure.  The first decision block
        # addresses the special case when the starting gradient just happens to be
        # equal to the gradient for onset of impermeability (not very likely!).

        # if ending_gradient > gradient_onset_of_imperm:
        else:

            if starting_gradient == gradient_onset_of_imperm:
                Amb_Pressure_Onset_of_Imperm[i] = starting_ambient_pressure
                Gas_Tension_Onset_of_Imperm[i] = starting_gas_tension
            # In most cases, a subroutine will be called to find these values using a
            # numerical method.
            if starting_gradient < gradient_onset_of_imperm:
                onset = onset_of_impermeability( starting_ambient_pressure, ending_ambient_pressure
                                               , Fraction_Helium
                                               , Fraction_Nitrogen
                                               , Initial_Helium_Pressure[i]
                                               , Initial_Nitrogen_Pressure[i]
                                               , Helium_Time_Constant[i]
                                               , Nitrogen_Time_Constant[i]
                                               , profile, settings)

                Amb_Pressure_Onset_of_Imperm[i] = onset[0] # mid_range_ambient_pressure
                Gas_Tension_Onset_of_Imperm[i]  = onset[1] # gas_tension_at_mid_range

            # Next, using the values for ambient pressure and gas tension at the onset
            # of impermeability, the equations are set up to process the calculations
            # through the radius root finder subroutine.  This subprogram will find the
            # root (solution) to the cubic equation using a numerical method.  In order
            # to do this efficiently, the equations are placed in the form
            # Ar^3 - Br^2 - C = 0, where r is the ending radius after impermeable
            # compression.  The coefficients A, B, and C for helium and nitrogen are
            # computed and passed to the subroutine as arguments.  The high and low
            # bounds to be used by the numerical method of the subroutine are also
            # computed (see separate page posted on Deco List ftp site entitled
            # "VPM: Solving for radius in the impermeable regime").  The subprogram
            # will return the value of the ending radius and then the crushing
            # pressures for helium and nitrogen can be calculated.
            ending_ambient_pressure_pa = (ending_ambient_pressure / settings.Units.toUnitsFactor()) * ATM

            amb_press_onset_of_imperm_pa = (Amb_Pressure_Onset_of_Imperm[i] / settings.Units.toUnitsFactor()) * ATM

            gas_tension_onset_of_imperm_pa = (Gas_Tension_Onset_of_Imperm[i] / settings.Units.toUnitsFactor()) * ATM

            crushing_pressure_he = crushing_pressure_helper(settings, radius_onset_of_imperm_he, ending_ambient_pressure_pa, amb_press_onset_of_imperm_pa, gas_tension_onset_of_imperm_pa, gradient_onset_of_imperm_pa)

            crushing_pressure_n2 = crushing_pressure_helper(settings, radius_onset_of_imperm_n2, ending_ambient_pressure_pa, amb_press_onset_of_imperm_pa, gas_tension_onset_of_imperm_pa, gradient_onset_of_imperm_pa)

        # UPDATE VALUES OF MAX CRUSHING PRESSURE IN Object ARRAYS
        Max_Crushing_Pressure_He[i] = max(Max_Crushing_Pressure_He[i], crushing_pressure_he)
        Max_Crushing_Pressure_N2[i] = max(Max_Crushing_Pressure_N2[i], crushing_pressure_n2)

    return (Max_Crushing_Pressure_He, Max_Crushing_Pressure_N2)

def critical_volume( Allowable_Gradient_He, Allowable_Gradient_N2
                   , Deco_Phase_Volume_Time, Surface_Phase_Volume_Time
                   , Adjusted_Crushing_Pressure_He, Adjusted_Crushing_Pressure_N2
                   , Initial_Allowable_Gradient_He, Initial_Allowable_Gradient_N2
                   , settings):
    # Purpose: This subprogram applies the VPM Critical Volume Algorithm.  This
    # algorithm will compute "relaxed" gradients for helium and nitrogen based
    # on the setting of the Critical Volume Parameter Lambda.

    # Note:  Since the Critical Volume Parameter Lambda was defined in units of
    # fsw-min in the original papers by Yount and colleagues, the same
    # convention is retained here.  Although Lambda is adjustable only in units
    # of fsw-min in the program settings (range from 6500 to 8300 with default
    # 7500), it will convert to the proper value in Pascals-min in this
    # subroutine regardless of which diving pressure units are being used in
    # the main program - feet of seawater (fsw) or meters of seawater (msw).
    # The allowable gradient is computed using the quadratic formula (refer to
    # separate write-up posted on the Deco List web site).

    parameter_lambda_pascals = (settings.Crit_Volume_Parameter_Lambda / 33.0) * ATM

    for i in COMPARTMENT_RANGE:
        phase_volume_time = Deco_Phase_Volume_Time + Surface_Phase_Volume_Time[i]

        # Helium Calculations
        adj_crush_pressure_he_pascals = (Adjusted_Crushing_Pressure_He[i] / settings.Units.toUnitsFactor()) * ATM
        initial_allowable_grad_he_pa = (Initial_Allowable_Gradient_He[i] / settings.Units.toUnitsFactor()) * ATM
        B = initial_allowable_grad_he_pa + (parameter_lambda_pascals * settings.Surface_Tension_Gamma) / (settings.Skin_Compression_GammaC * phase_volume_time)
        C = (settings.Surface_Tension_Gamma * (settings.Surface_Tension_Gamma * (parameter_lambda_pascals * adj_crush_pressure_he_pascals))) / (settings.Skin_Compression_GammaC * (settings.Skin_Compression_GammaC * phase_volume_time))
        new_allowable_grad_he_pascals = (B + sqrt(B ** 2 - 4.0 * C)) / 2.0
        Allowable_Gradient_He[i] = (new_allowable_grad_he_pascals / ATM) * settings.Units.toUnitsFactor()

        # Nitrogen Calculations
        adj_crush_pressure_n2_pascals = (Adjusted_Crushing_Pressure_N2[i] / settings.Units.toUnitsFactor()) * ATM
        initial_allowable_grad_n2_pa = (Initial_Allowable_Gradient_N2[i] / settings.Units.toUnitsFactor()) * ATM
        B = initial_allowable_grad_n2_pa + (parameter_lambda_pascals * settings.Surface_Tension_Gamma) / (settings.Skin_Compression_GammaC * phase_volume_time)
        C = (settings.Surface_Tension_Gamma * (settings.Surface_Tension_Gamma * (parameter_lambda_pascals * adj_crush_pressure_n2_pascals))) / (settings.Skin_Compression_GammaC * (settings.Skin_Compression_GammaC * phase_volume_time))
        new_allowable_grad_n2_pascals = (B + sqrt(B ** 2 - 4.0 * C)) / 2.0
        Allowable_Gradient_N2[i] = (new_allowable_grad_n2_pascals / ATM) * settings.Units.toUnitsFactor()

    return (Allowable_Gradient_He, Allowable_Gradient_N2)





def schreiner_equation(initial_inspired_gas_pressure, rate_change_insp_gas_pressure, interval_time, gas_time_constant, initial_gas_pressure):
    """Function for ascent and descent gas loading calculations"""
    return initial_inspired_gas_pressure + rate_change_insp_gas_pressure * (interval_time - 1.0 / gas_time_constant) - (initial_inspired_gas_pressure - initial_gas_pressure - rate_change_insp_gas_pressure / gas_time_constant) * exp(-gas_time_constant * interval_time)


def haldane_equation(initial_gas_pressure, inspired_gas_pressure, gas_time_constant, interval_time):
    """Function for gas loading calculations at a constant depth"""
    return initial_gas_pressure + (inspired_gas_pressure - initial_gas_pressure) * (1.0 - exp(-gas_time_constant * interval_time))


def radius_root_finder(A, B, C, low_bound, high_bound):
    """
    Purpose: This subroutine is a "fail-safe" routine that combines the
    Bisection Method and the Newton-Raphson Method to find the desired root.
    This hybrid algorithm takes a bisection step whenever Newton-Raphson would
    take the solution out of bounds, or whenever Newton-Raphson is not
    converging fast enough.  Source:  "Numerical Recipes in Fortran 77",
    Cambridge University Press, 1992.
    """
    # BEGIN CALCULATIONS BY MAKING SURE THAT THE ROOT LIES WITHIN BOUNDS
    # In this case we are solving for radius in a cubic equation of the form,
    # Ar^3 - Br^2 - C = 0.  The coefficients A, B, and C were passed to this
    # subroutine as arguments.
    function_at_low_bound = low_bound * (low_bound * (A * low_bound - B)) - C

    function_at_high_bound = high_bound * (high_bound * (A * high_bound - B)) - C

    if function_at_low_bound > 0.0 and function_at_high_bound > 0.0:
        raise RootException("ERROR! ROOT IS NOT WITHIN BRACKETS")

    # Next the algorithm checks for special conditions and then prepares for
    # the first bisection.
    if function_at_low_bound < 0.0 and function_at_high_bound < 0.0:
        raise RootException("ERROR! ROOT IS NOT WITHIN BRACKETS")

    if function_at_low_bound == 0.0:
        return low_bound
    elif function_at_high_bound == 0.0:
        return high_bound
    elif function_at_low_bound < 0.0:
        radius_at_low_bound = low_bound
        radius_at_high_bound = high_bound
    else:
        radius_at_high_bound = low_bound
        radius_at_low_bound = high_bound

    ending_radius = 0.5 * (low_bound + high_bound)
    last_diff_change = abs(high_bound - low_bound)
    differential_change = last_diff_change

    # At this point, the Newton-Raphson Method is applied which uses a function
    # and its first derivative to rapidly converge upon a solution.
    # Note: the program allows for up to 100 iterations.  Normally an exit will
    # be made from the loop well before that number.  If, for some reason, the
    # program exceeds 100 iterations, there will be a pause to alert the user.
    # When a solution with the desired accuracy is found, exit is made from the
    # loop by returning to the calling program.  The last value of ending
    # radius has been assigned as the solution.

    function = ending_radius * (ending_radius * (A * ending_radius - B)) - C

    derivative_of_function = ending_radius * (ending_radius * 3.0 * A - 2.0 * B)

    for i in range(100):
        # TODO: Choose better name for the 'a' and 'b' checks
        a = (((ending_radius - radius_at_high_bound) * derivative_of_function - function) *
             ((ending_radius - radius_at_low_bound) * derivative_of_function - function) >= 0.0)
        b = (abs(2.0 * function) > (abs(last_diff_change * derivative_of_function)))

        if a or b:

            last_diff_change = differential_change
            differential_change = 0.5 * (radius_at_high_bound - radius_at_low_bound)

            ending_radius = radius_at_low_bound + differential_change
            if radius_at_low_bound == ending_radius:
                return ending_radius
        else:
            last_diff_change = differential_change
            differential_change = function / derivative_of_function
            last_ending_radius = ending_radius
            ending_radius -= differential_change
            if last_ending_radius == ending_radius:
                return ending_radius
        if abs(differential_change) < 1.0E-12:
            return ending_radius
        function = ending_radius * (ending_radius * (A * ending_radius - B)) - C

        derivative_of_function = ending_radius * (ending_radius * 3.0 * A - 2.0 * B)

        if function < 0.0:
            radius_at_low_bound = ending_radius
        else:
            radius_at_high_bound = ending_radius

    raise MaxIterationException('ERROR! ROOT SEARCH EXCEEDED MAXIMUM ITERATIONS')


def calc_barometric_pressure(altitude, units_fsw):
    """
    Purpose: This function calculates barometric pressure at altitude based on the
    publication "U.S. Standard Atmosphere, 1976", U.S. Government Printing
    Office, Washington, D.C. The basis for this code is a Fortran 90 program
    written by Ralph L. Carmichael (retired NASA researcher) and endorsed by
    the National Geophysical Data Center of the National Oceanic and
    Atmospheric Administration.  It is available for download free from
    Public Domain Aeronautical Software at:  http://www.pdas.com/atmos.htm
    """

    radius_of_earth = 6369.0  # kilometers
    acceleration_of_gravity = 9.80665  # meters/second^2
    molecular_weight_of_air = 28.9644  # mols
    gas_constant_r = 8.31432  # Joules/mol*deg Kelvin
    temp_at_sea_level = 288.15  # degrees Kelvin

    # Change in Temp deg Kelvin with
    # change in geopotential altitude,
    # valid for first layer of atmosphere
    # up to 11 kilometers or 36,000 feet
    temp_gradient = -6.5

    gmr_factor = acceleration_of_gravity * molecular_weight_of_air / gas_constant_r

    if units_fsw == UnitsSW.FSW:
        altitude_kilometers = altitude / 3280.839895
        pressure_at_sea_level = 33.0  # feet of seawater based on 101325 Pa at sea level (Standard Atmosphere)
    else:
        altitude_kilometers = altitude / 1000.0
        pressure_at_sea_level = 10.0  # meters of seawater based on 100000 Pa at sea level (European System)

    geopotential_altitude = (altitude_kilometers * radius_of_earth) / (altitude_kilometers + radius_of_earth)
    temp_at_geopotential_altitude = temp_at_sea_level + temp_gradient * geopotential_altitude

    barometric_pressure = pressure_at_sea_level * exp(log(temp_at_sea_level / temp_at_geopotential_altitude) * gmr_factor / temp_gradient)
    return barometric_pressure

def new_critical_radius(settings, max_actual_gradient_pascals, adj_crush_pressure_pascals):
    """Calculates the new radius for the `VPM_REPETITIVE_ALGORITHM`
    """
    return ((2.0 * settings.Surface_Tension_Gamma * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma))) / (max_actual_gradient_pascals * settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma * adj_crush_pressure_pascals)

def calculate_deco_gradient(settings, allowable_gradient_molecule, amb_press_first_stop_pascals, amb_press_next_stop_pascals):
    """Calculates the decompression gradient for Boyles_Law_Compensation.
    """

    allow_grad_first_stop_pa = (allowable_gradient_molecule / settings.Units.toUnitsFactor()) * ATM
    radius_first_stop = (2.0 * settings.Surface_Tension_Gamma) / allow_grad_first_stop_pa

    A = amb_press_next_stop_pascals
    B = -2.0 * settings.Surface_Tension_Gamma
    C = (amb_press_first_stop_pascals + (2.0 * settings.Surface_Tension_Gamma) / radius_first_stop) * radius_first_stop * (radius_first_stop * radius_first_stop)

    low_bound = radius_first_stop
    high_bound = radius_first_stop * (amb_press_first_stop_pascals / amb_press_next_stop_pascals) ** (1.0 / 3.0)

    ending_radius = radius_root_finder(A, B, C, low_bound, high_bound)

    deco_gradient_pascals = (2.0 * settings.Surface_Tension_Gamma) / ending_radius
    return (deco_gradient_pascals / ATM) * settings.Units.toUnitsFactor()

def crushing_pressure_helper(settings, radius_onset_of_imperm_molecule, ending_ambient_pressure_pa, amb_press_onset_of_imperm_pa, gas_tension_onset_of_imperm_pa, gradient_onset_of_imperm_pa):
    """Calculate the crushing pressure for a molecule(He or N2) (a helper for CALC_CRUSHING_PRESSURE)
    """

    A = ending_ambient_pressure_pa - amb_press_onset_of_imperm_pa + gas_tension_onset_of_imperm_pa + (2.0 * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma)) / radius_onset_of_imperm_molecule
    B = 2.0 * (settings.Skin_Compression_GammaC - settings.Surface_Tension_Gamma)
    C = gas_tension_onset_of_imperm_pa * radius_onset_of_imperm_molecule ** 3

    high_bound = radius_onset_of_imperm_molecule
    low_bound = B / A

    ending_radius = radius_root_finder(A, B, C, low_bound, high_bound)
    crushing_pressure_pascals = gradient_onset_of_imperm_pa + ending_ambient_pressure_pa - amb_press_onset_of_imperm_pa + gas_tension_onset_of_imperm_pa * (1.0 - radius_onset_of_imperm_molecule ** 3 / ending_radius ** 3)

    return (crushing_pressure_pascals / ATM) * settings.Units.toUnitsFactor()

















def parse_settings():
    """
    Purpose:
    Use OptParse to parse the command line switches and return the
    'options' and 'args' objects
    """

    # Load settings and inputs
    parser = argparse.ArgumentParser()

    parser.add_argument("-i", action="store", dest="input_file_name",
                        default="vpm_decompression_input.json",
                        help="Input file containing dive information")

    parser.add_argument("-o", action="store", dest="output_file_name",
                        default="output.html",
                        help="Output file for dive log")

    parser.add_argument("-j", action="store", dest="json_output", default=None,
                        help="Output json instead of html")

    return parser.parse_args()

# if they ran this at the command line, output the parse the command line
# options and output the results
if __name__ == '__main__':
    args = parse_settings()

    reader = JSONReader.VPMBJSONReader()

    program_state = DiveState()

    reader.load_external_data(program_state, input_file_name=args.input_file_name)
    program_state.main()

    if args.json_output:
        with open(args.json_output, "w") as json_out:
            program_state.output_object.to_json(json_out)

    else:
        program_state.output_object.to_html(args.output_file_name)
