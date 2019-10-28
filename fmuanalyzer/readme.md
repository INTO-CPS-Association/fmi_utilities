# Details on FMUAnalyzer.sh

The script is to be executed from the root folder of the project.

The FMUAnalyzer\*.jar produces a log file called `experiment.log`.

The script steps into the directory containing the log file and executes the
ResultsProcessing\*.jar from this location. The reason is that
ResultsProcessing\*.jar must be executed within the same directory as the log
file.

More details are available in the script.



