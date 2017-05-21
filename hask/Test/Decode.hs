{-# LANGUAGE OverloadedStrings #-}

module Test.Decode where

import Data.Aeson
import Data.Aeson.Types
import Vpmb

instance FromJSON GasMix where
  parseJSON = withObject "GasMix" $ \v -> GasMix <$> v .: "fraction_He"
                                                 <*> v .: "fraction_N2"
                                                 <*> v .: "fraction_O2"

instance FromJSON AltitudeValues where
  parseJSON = withObject "altitude" $ \v -> mkAltitudeValues =<< v .: "altitude"
    where
    mkAltitudeValues altitude =
      AltitudeValues 0.0 <$> altitude .: "Ascent_to_Altitude_Hours"
                         <*> altitude .: "Altitude_of_Dive"
                         <*> ((== ("yes" :: String)) <$> altitude .: "Diver_Acclimatized_at_Altitude")
                         <*> altitude .: "Hours_at_Altitude_Before_Dive"
                         <*> altitude .: "Starting_Acclimatized_Altitude"

instance FromJSON SettingsValues where
  parseJSON = withObject "settings" $ \v -> mkSettingsValues =<< v .: "settings"
    where
    mkSettingsValues settings =
      SettingsValues <$> settings .: "Altitude_Dive_Algorithm"
                     <*> settings .: "Crit_Volume_Parameter_Lambda"
                     <*> settings .: "Critical_Radius_He_Microns"
                     <*> settings .: "Critical_Radius_N2_Microns"
                     <*> settings .: "Critical_Volume_Algorithm"
                     <*> settings .: "Gradient_Onset_of_Imperm_Atm"
                     <*> settings .: "Minimum_Deco_Stop_Time"
                     <*> settings .: "Pressure_Other_Gases_mmHg"
                     <*> settings .: "Regeneration_Time_Constant"
                     <*> settings .: "Skin_Compression_GammaC"
                     <*> settings .: "Surface_Tension_Gamma"
                     <*> ((\u -> if u == ("msw" :: String) then MSW else FSW) <$> settings .: "Units")

instance FromJSON Planning where
  parseJSON = withObject "input" $ \v -> Planning <$> v .: "input"

instance FromJSON Dive where
  parseJSON = withObject "Dive" $ \v ->
    Dive <$> v .: "desc"
         <*> v .: "gasmix_summary"
         <*> v .: "num_gas_mixes"
         <*> v .: "profile_codes"
         <*> ((== (1 :: Int)) <$> v .: "repetitive_code")
         <*> v .:? "surface_interval_time_minutes" .!= 0

instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \v ->
      Profile <$> v .:? "ascent_summary" .!= []
              <*> v .:? "depth"          .!= 0.0
              <*> v .:? "starting_depth" .!= 0.0
              <*> v .:? "ending_depth"   .!= 0.0
              <*> (mkProfileCode <$> v .:? "profile_code" .!= 1)
              <*> v .:? "gasmix"         .!= 0
              <*> v .:? "number_of_ascent_parameter_changes" .!= 0
              <*> v .:? "rate"                       .!= 0.0
              <*> v .:? "run_time_at_end_of_segment" .!= 0.0
              <*> v .:? "setpoint"                   .!= 0.0
    where
    mkProfileCode :: Int -> ProfileCode
    mkProfileCode 1 = AscentDescentCode
    mkProfileCode 2 = ConstantCode
    mkProfileCode _ = DecompressCode

instance FromJSON Ascent where
  parseJSON = withObject "Ascent" $ \v ->
      Ascent <$> v .: "gasmix"
             <*> v .: "rate"
             <*> v .: "setpoint"
             <*> v .: "starting_depth"
             <*> v .: "step_size"

{-def data_to_input(self, data):-}
    {-all_dives = []-}

    {-for dive_data in data["input"]:-}
        {-dive = vpmb.Dive()-}
        {-dive.desc = dive_data.get("desc", "")-}
        {-dive.gasmix_summary = self.data_to_gasmix_summary(dive_data["gasmix_summary"])-}
        {-dive.num_gas_mixes = dive_data.get("num_gas_mixes", 0)-}
        {-dive.profile_codes = self.data_to_profiles(dive_data["profile_codes"])-}
        {-dive.repetitive_code = dive_data.get("repetitive_code", 0) == 1-}
        {-dive.surface_interval_time_minutes = dive_data.get("surface_interval_time_minutes", 0.0)-}
        {-all_dives.append(dive)-}

    {-return all_dives-}

testInput = "{    \"input\": [{        \"desc\": \"TRIMIX DIVE TO 80 MSW\",        \"num_gas_mixes\": 1,        \"gasmix_summary\": [{            \"fraction_O2\": 0.15,            \"fraction_He\": 0.45,            \"fraction_N2\": 0.40        }],        \"profile_codes\": [{                \"profile_code\": 1,                \"starting_depth\": 0,                \"ending_depth\": 80,                \"rate\": 23,                \"gasmix\": 1,                \"setpoint\": 0.7            },            {                \"profile_code\": 2,                \"depth\": 80,                \"run_time_at_end_of_segment\": 30,                \"gasmix\": 1,                \"setpoint\": 1.3            },            {                \"profile_code\": 99,                \"number_of_ascent_parameter_changes\": 1,                \"ascent_summary\": [{                    \"starting_depth\": 80,                    \"gasmix\": 1,                    \"rate\": -10,                    \"step_size\": 3,                    \"setpoint\": 1.6                }]            }        ],        \"repetitive_code\": 0    }],    \"altitude\": {        \"Altitude_of_Dive\": 100,        \"Diver_Acclimatized_at_Altitude\": \"yes\",        \"Starting_Acclimatized_Altitude\": 0,        \"Ascent_to_Altitude_Hours\": 0,        \"Hours_at_Altitude_Before_Dive\": 0    },    \"settings\": {        \"Units\": \"msw\",        \"SetPoint_Is_Bar\": true,        \"Altitude_Dive_Algorithm\": true,        \"Minimum_Deco_Stop_Time\": 1.0,        \"Critical_Radius_N2_Microns\": 0.6,        \"Critical_Radius_He_Microns\": 0.5,        \"Critical_Volume_Algorithm\": true,        \"Crit_Volume_Parameter_Lambda\": 7500.0,        \"Gradient_Onset_of_Imperm_Atm\": 8.2,        \"Surface_Tension_Gamma\": 0.0179,        \"Skin_Compression_GammaC\": 0.257,        \"Regeneration_Time_Constant\": 20160.0,        \"Pressure_Other_Gases_mmHg\": 102.0    }}"
testAltitudeValues = decode testInput :: Maybe AltitudeValues
testSettingsValues = decode testInput :: Maybe SettingsValues
testPlanning = decode testInput :: Maybe Planning


{-def data_to_profiles(self, profile_data):-}
    {-profiles = []-}

    {-for profile in profile_data:-}
        {-new_profile = vpmb.Profile()-}

        {-ascent_sums = []-}

        {-for asc_sum_data in profile.get("ascent_summary", []):-}
            {-asc_sum = vpmb.Ascent()-}
            {-asc_sum.gasmix = asc_sum_data["gasmix"]-}
            {-asc_sum.rate = asc_sum_data["rate"]-}
            {-asc_sum.setpoint = asc_sum_data["setpoint"]-}
            {-asc_sum.starting_depth = asc_sum_data["starting_depth"]-}
            {-asc_sum.step_size = asc_sum_data["step_size"]-}
            {-ascent_sums.append(asc_sum)-}

        {-new_profile.ascent_summary = ascent_sums-}
        {-new_profile.depth = profile.get("depth", 0.0)-}
        {-new_profile.ending_depth = profile.get("ending_depth", 0.0)-}
        {-new_profile.gasmix = profile.get("gasmix", 0)-}
        {-new_profile.number_of_ascent_parameter_changes = profile.get("number_of_ascent_parameter_changes", 0)-}
        {-new_profile.profile_code = vpmb.ProfileCode.fromInt(profile.get("profile_code", 1))-}
        {-new_profile.rate = profile.get("rate", 0.0)-}
        {-new_profile.run_time_at_end_of_segment = profile.get("run_time_at_end_of_segment", 0.0)-}
        {-new_profile.setpoint = profile.get("setpoint", 0.0)-}
        {-new_profile.starting_depth = profile.get("starting_depth", 0.0)-}

        {-profiles.append(new_profile)-}

    {-return profiles-}



{-def load_external_data(self, ds, input_file_name=None, json_input=None):-}
    {-"""Take the input_file_name  parsed by parse_settings or raw json data and use it to initialize the program state"""-}

    {-if input_file_name is None and json_input is None:-}
        {-raise ValueError("""DiveState must be given a file name (to load the input from), or raw json data""")-}

    {-if json_input:-}
        {-data = json_input-}
    {-else:-}
        {--- load the file data-}
        {-with open(input_file_name) as input_file:-}
            {-data = json.loads(input_file.read())-}

    {-ds.set_input_values(self.data_to_input(data))-}
    {-ds.set_settings(self.data_to_settings(data))-}
    {-ds.set_altitude_values(self.data_to_altitude(data))-}
    {-ds.set_output_object(HtmlOutput.HtmlOutput(self))-}

