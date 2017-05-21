module Vpmb where

data Gas
  = Oxygen
  | Helium
  | Nitrogen
  deriving (Eq, Show, Enum)

data ProfileCode
  = AscentDescentCode
  | ConstantCode
  | DecompressCode
  deriving (Eq, Show, Enum)

data UnitsSW
  = FSW
  | MSW
  deriving (Eq, Show, Enum)

    {-@staticmethod-}
    {-def fromString(str):-}
        {-if str.lower() == "fsw":-}
            {-return UnitsSW.FSW-}
        {-else:-}
            {-return UnitsSW.MSW-}

    {-def toWord1(self):-}
        {-if self == UnitsSW.FSW:-}
            {-return "fswg"-}
        {-else:-}
            {-return "mswg"-}

    {-def toWord2(self):-}
        {-if self == UnitsSW.FSW:-}
            {-return "fsw/min"-}
        {-else:-}
            {-return "msw/min"-}

    {-def toUnitsFactor(self):-}
        {-if self == UnitsSW.FSW:-}
            {-return 33.0-}
        {-else:-}
            {-return 10.1325-}

    {-def toWaterVaporPressure(self):-}
        {-if self == UnitsSW.FSW:-}
            {-return 1.607  # based on respiratory quotient of 0.8 (Schreiner value)-}
        {-else:-}
            {-return 0.493     # based on respiratory quotient of 0.8 (Schreiner value)-}

    {-def __str__(self):-}
        {-if self == UnitsSW.FSW:-}
            {-return "FSW"-}
        {-else:-}
            {-return "MSW"-}

newtype Planning = Planning [Dive]
  deriving Show

data Dive = Dive
  { desc                          :: String
  , gasmix_summary                :: [GasMix]
  , num_gas_mixes                 :: Int
  , profile_codes                 :: [Profile]
  , repetitive_code               :: Bool
  , surface_interval_time_minutes :: Int
  }
  deriving (Eq, Show)

data GasMix = GasMix
  { fraction_He :: Double
  , fraction_N2 :: Double
  , fraction_O2 :: Double
  }
  deriving (Eq, Show)

gasMix = GasMix { fraction_He = 0.0
                , fraction_N2 = 79.0
                , fraction_O2 = 21.0
                }

data Profile = Profile
  { ascent_summary                     :: [Ascent]
  , depth                              :: Double
  , starting_depth                     :: Double
  , ending_depth                       :: Double
  , profile_code                       :: ProfileCode
  , gasmix                             :: Int
  , number_of_ascent_parameter_changes :: Int
  , rate                               :: Double
  , run_time_at_end_of_segment         :: Double
  , setpoint                           :: Double
  }
  deriving (Eq, Show)

data Ascent = Ascent
  { ascent_gasmix        :: Int
  , ascent_rate          :: Double
  , ascent_setpoint      :: Double
  , ascent_startingDepth :: Double
  , ascent_stepSize      :: Double
  }
  deriving (Eq, Show)

data AltitudeValues = AltitudeValues
  { altitude                       :: Double
  , ascent_to_Altitude_Hours       :: Double
  , altitude_of_Dive               :: Double
  , diver_Acclimatized_at_Altitude :: Bool
  , hours_at_Altitude_Before_Dive  :: Double
  , starting_Acclimatized_Altitude :: Double
  }
  deriving (Eq, Show)

data SettingsValues = SettingsValues
  { altitude_Dive_Algorithm      :: Bool
  , critical_Radius_N2_Microns   :: Double
  , critical_Radius_He_Microns   :: Double
  , crit_Volume_Parameter_Lambda :: Double
  , critical_Volume_Algorithm    :: Bool
  , gradient_Onset_of_Imperm_Atm :: Double
  , minimum_Deco_Stop_Time       :: Double
  , pressure_Other_Gases_mmHg    :: Double
  , regeneration_Time_Constant   :: Double
  , surface_Tension_Gamma        :: Double
  , skin_Compression_GammaC      :: Double
  , units                        :: UnitsSW
  }
  deriving (Eq, Show)
