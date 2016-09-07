import datetime

class HtmlOutput(object):
    """Provides a convenient dumping ground for output. Can be exported to json and html
    """

    def __init__(self, state_object):
        self.output = []
        self.current_dive = None
        self.state_object = state_object

    def new_dive(self, description):
        """Creates a new dive hash table and updates the current dive to
        point at the newly created hash"""
        self.output.append({"desc": description,
                            "time": str(datetime.datetime.now().strftime("%B %d, %Y at %H:%M")),  # make this configurable
                            "gasmix": [],
                            "dive_profile": [],
                            "decompression_profile": []})

        if self.current_dive is None:
            self.current_dive = 0
        else:
            self.current_dive += 1

    def add_gasmix(self, oxygen, nitrogen, helium):
        """Adds a new gasmix to the current dive"""
        self.output[self.current_dive]["gasmix"].append({"oxygen": oxygen,
                                                         "nitrogen": nitrogen,
                                                         "helium": helium})

    def add_dive_profile_entry_ascent(self, segment_number, segment_time, run_time, mix_number, depth):
        """Adds a new ascent entry to the dive profile table"""
        split = "%d|%6.1f |%6.1f | %d |  | | | | %6.1f| " \
                % (segment_number, segment_time, run_time, mix_number, depth)
        self.output[self.current_dive]["dive_profile"].append(split.split("|"))

    def add_dive_profile_entry_descent(self, segment_number, segment_time, run_time, mix_number, word, starting_depth, ending_depth, rate):
        """Adds a new descent entry to the dive profile table"""
        split = "%d|%6.1f |%6.1f | %d | %s | %6.1f| %6.1f| %6.1f| |" \
                % (segment_number, segment_time, run_time, mix_number, word, starting_depth, ending_depth, rate)
        self.output[self.current_dive]["dive_profile"].append(split.split("|"))

    def add_decompression_profile_ascent(self, segment_number, segment_time, run_time, mix_number, deco_stop_depth, rate):
        """Adds a new ascent entry to the decompression table"""
        split = "%d|%5.1f|%6.1f|%d|%d|%d | | |" \
                % (segment_number, segment_time, run_time, mix_number, deco_stop_depth, rate)
        self.output[self.current_dive]["decompression_profile"].append(split.split("|"))

    def add_decompression_profile_constant(self, segment_number, segment_time, run_time, mix_number, deco_stop_depth, stop_time):
        """Adds a new constant depth entry to the decompression table"""

        split = "%d|%5.1f|%6.1f|%d| | |%d|%d|%d|" \
                % (segment_number, segment_time, run_time, mix_number, deco_stop_depth, stop_time, int(run_time))
        self.output[self.current_dive]["decompression_profile"].append(split.split("|"))

    def to_html(self, filename=None):
        """Export the dive output object to an HTML file or the console"""

        output = ""
        output += self.html_header()

        for dive in self.output:
            output += self.html_description_time_and_gasmix(dive)
            output += self.html_dive_table(dive)
            output += self.html_decompression_table(dive)

        output += self.html_footer()
        if filename:
            with open(filename, "w") as f:
                f.write(output)

        else:
            print(output)

    def html_description_time_and_gasmix(self, dive):
        """Return the table containing the time and gasmix"""
        return_string = ""
        return_string += "<p>"

        return_string += dive.desc
        return_string += "<br/>"
        return_string += dive.time
        return_string += "<br/>"
        return_string += "Gasmix Summary"
        return_string += "</p>"
        return_string += """
            <table border="2" >
            <tr>
            <th>Gasmix number</th>
            <th>O2</th>
            <th>He</th>
            <th>N2</th>
            </tr>
            """

        for i, gas in enumerate(dive.gasmix):
            return_string += """<tr>
            <td>%d</td>
            <td>%5.3f</td>
            <td>%5.3f</td>
            <td>%5.3f</td>
            </tr>
            """ % (i + 1, gas["oxygen"], gas["helium"], gas["nitrogen"])
        return_string += "</table>"

        return return_string

    def html_dive_table(self, dive):
        return_string = ""
        return_string += "<p>Dive Profile</p>"
        return_string += """<table border="2" >
        <tr>
        <th>Segment</th>
        <th>Segment Time (min)</th>
        <th>Run Time (min)</th>
        <th>Gasmix Used #</th>
        <th>Ascent or Descent</th>
        <th>From Depth (%s)</th>
        <th>To Depth (%s)</th>
        <th>Rate +Dn/-Up (%s)</th>
        <th>Constant Depth (%s)</th>
        </tr>
        """ % (self.state_object.Units.toWord1(), self.state_object.Units.toWord1(), self.state_object.Units.toWord2(), self.state_object.Units.toWord1())

        for d in dive.dive_profile:
            return_string += "<tr>"
            for elem in d:
                return_string += "<td>%s</td>" % (str(elem))
            return_string += "</tr>"

        return_string += "</table>"
        return return_string

    def html_decompression_table(self, dive):
        return_string = ""
        return_string += "<p>"

        return_string += "DECOMPRESSION PROFILE"
        return_string += "<br/>"
        return_string += "Leading compartment enters the decompression zone at, %.1f %s" % (self.state_object.Depth_Start_of_Deco_Zone, self.state_object.Units.toWord1())
        return_string += "<br/>"

        return_string += "Deepest possible decompression stop is %.1f %s" % (self.state_object.Deepest_Possible_Stop_Depth, self.state_object.Units.toWord1())
        return_string += "<br/>"
        return_string += "</p>"

        return_string += """<table border="2">
        <tr>
        <th>Segment #</th>
        <th>Segment Time (min)</th>
        <th>Run Time(min)</th>
        <th>Gasmix Used #</th>
        <th>Ascent to (%s)</th>
        <th>Ascent Rate(%s)</th>
        <th>Deco Stop(%s)</th>
        <th>Stop Time (min)</th>
        <th>Run Time(min)</th>
        </tr>
        """ % (self.state_object.Units.toWord1(), self.state_object.Units.toWord2(), self.state_object.Units.toWord1())

        for d in dive.decompression_profile:
            return_string += "<tr>"
            for elem in d:
                return_string += "<td>%s</td>" % (str(elem))
            return_string += "</tr>"

        return_string += "</table>"
        return return_string

    def html_header(self):
        """Return the header of the html output"""
        header = '''<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
        <html xmlns="http://www.w3.org/1999/xhtml"
        lang="en" xml:lang="en">
        <head>
        <meta http-equiv="content-type" content="text/html;charset=utf-8" />
        <title>VPMB Dive chart</title>
        <style type="text/css">

        html { font-family: Times, serif; font-size: 12pt; }
        .title  { text-align: center; }
        .todo   { color: red; }
        .done   { color: green; }
        .tag    { background-color:lightblue; font-weight:normal }
        .target { }
        .timestamp { color: grey }
        .timestamp-kwd { color: CadetBlue }
        p.verse { margin-left: 3% }
        pre {
          border: 1pt solid #AEBDCC;
          background-color: #F3F5F7;
          padding: 5pt;
          font-family: courier, monospace;
          font-size: 90%;
          overflow:auto;
        }
        table { border-collapse:collapse;  }
        td, th { vertical-align: top; }
        dt { font-weight: bold; }
        div.figure { padding: 0.5em; }
        div.figure p { text-align: center; }
        .linenr { font-size:smaller }
        .code-highlighted {background-color:#ffff00;}

        </style>
        </head><body>
        '''
        return header

    def html_footer(self):
        """Return the html footer"""
        return "</body></html>"

    def to_json(self, file_pointer):
        """Export the json formatted output to a file
        """
        json.dump(self.output, file_pointer)

    def get_json(self):
        """Return the output JSON"""
        return self.output
