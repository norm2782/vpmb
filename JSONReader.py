import json
import vpmb
import HtmlOutput

class VPMBJSONReader(object):
    def data_to_profiles(self, profile_data):
        profiles = []

        for profile in profile_data:
            new_profile = vpmb.Profile()

            ascent_sums = []

            for asc_sum_data in profile.get("ascent_summary", []):
                asc_sum = vpmb.Ascent()
                asc_sum.gasmix = asc_sum_data["gasmix"]
                asc_sum.rate = asc_sum_data["rate"]
                asc_sum.setpoint = asc_sum_data["setpoint"]
                asc_sum.starting_depth = asc_sum_data["starting_depth"]
                asc_sum.step_size = asc_sum_data["step_size"]
                ascent_sums.append(asc_sum)

            new_profile.ascent_summary = ascent_sums
            new_profile.depth = profile.get("depth", 0.0)
            new_profile.ending_depth = profile.get("ending_depth", 0.0)
            new_profile.gasmix = profile.get("gasmix", 0)
            new_profile.number_of_ascent_parameter_changes = profile.get("number_of_ascent_parameter_changes", 0)
            new_profile.profile_code = vpmb.ProfileCode.fromInt(profile.get("profile_code", 1))
            new_profile.rate = profile.get("rate", 0.0)
            new_profile.run_time_at_end_of_segment = profile.get("run_time_at_end_of_segment", 0.0)
            new_profile.setpoint = profile.get("setpoint", 0.0)
            new_profile.starting_depth = profile.get("starting_depth", 0.0)

            profiles.append(new_profile)

        return profiles

    def data_to_gasmix_summary(self, gas_data):
        summaries = []

        for summary_data in gas_data:
            mix = vpmb.GasMix()
            mix.fraction_He = summary_data["fraction_He"]
            mix.fraction_N2 = summary_data["fraction_N2"]
            mix.fraction_O2 = summary_data["fraction_O2"]
            summaries.append(mix)

        return summaries


    def data_to_input(self, data):
        all_dives = []

        for dive_data in data["input"]:
            dive = vpmb.Dive()
            dive.desc = dive_data.get("desc", "")
            dive.gasmix_summary = self.data_to_gasmix_summary(dive_data["gasmix_summary"])
            dive.num_gas_mixes = dive_data.get("num_gas_mixes", 0)
            dive.profile_codes = self.data_to_profiles(dive_data["profile_codes"])
            dive.repetitive_code = dive_data.get("repetitive_code", 0) == 1
            dive.surface_interval_time_minutes = dive_data.get("surface_interval_time_minutes", 0.0)
            all_dives.append(dive)

        return all_dives

    def data_to_settings(self, data):
        settings = vpmb.SettingsValues()
        settings.Altitude_Dive_Algorithm = data["settings"]["Altitude_Dive_Algorithm"]
        settings.Crit_Volume_Parameter_Lambda = data["settings"]["Crit_Volume_Parameter_Lambda"]
        settings.Critical_Radius_He_Microns = data["settings"]["Critical_Radius_He_Microns"]
        settings.Critical_Radius_N2_Microns = data["settings"]["Critical_Radius_N2_Microns"]
        settings.Critical_Volume_Algorithm = data["settings"]["Critical_Volume_Algorithm"]
        settings.Gradient_Onset_of_Imperm_Atm = data["settings"]["Gradient_Onset_of_Imperm_Atm"]
        settings.Minimum_Deco_Stop_Time = data["settings"]["Minimum_Deco_Stop_Time"]
        settings.Pressure_Other_Gases_mmHg = data["settings"]["Pressure_Other_Gases_mmHg"]
        settings.Regeneration_Time_Constant = data["settings"]["Regeneration_Time_Constant"]
        settings.Skin_Compression_GammaC = data["settings"]["Skin_Compression_GammaC"]
        settings.Surface_Tension_Gamma = data["settings"]["Surface_Tension_Gamma"]
        settings.Units = vpmb.UnitsSW.fromString(data["settings"]["Units"])

        return settings

    def data_to_altitude(self, data):
        alt_vals = vpmb.AltitudeValues()
        alt_vals.Ascent_to_Altitude_Hours = data["altitude"]["Ascent_to_Altitude_Hours"]
        alt_vals.Altitude_of_Dive = data["altitude"]["Altitude_of_Dive"]
        alt_vals.Diver_Acclimatized_at_Altitude = data["altitude"]["Diver_Acclimatized_at_Altitude"] == "yes"
        alt_vals.Hours_at_Altitude_Before_Dive = data["altitude"]["Hours_at_Altitude_Before_Dive"]
        alt_vals.Starting_Acclimatized_Altitude = data["altitude"]["Starting_Acclimatized_Altitude"]

        return alt_vals

    def load_external_data(self, ds, input_file_name=None, json_input=None):
        """Take the input_file_name  parsed by parse_settings or raw json data and use it to initialize the program state"""

        if input_file_name is None and json_input is None:
            raise ValueError("""DiveState must be given a file name (to load the input from), or raw json data""")

        if json_input:
            data = json_input
        else:
            # load the file data
            with open(input_file_name) as input_file:
                data = json.loads(input_file.read())

        ds.set_input_values(self.data_to_input(data))
        ds.set_settings(self.data_to_settings(data))
        ds.set_altitude_values(self.data_to_altitude(data))
        ds.set_output_object(HtmlOutput.HtmlOutput(self))

