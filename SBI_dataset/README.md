# Conversion of the SBI data to the InTaVia RDF graph

This project provides the Conversion of the SBI data to the InTaVia RDF graph.

The example (subset) converted data is available in the `sbi.ttl` file.

## Running the converter

The converter has the following requirements:

- Python 3.11
- [pdm](https://pdm.fming.dev/) package manager

To start using this project, first create a python virtual environment and install the dependencies:

    pdm install --dev

To start the converter, run the following command:

    pdm run ./convert.py <sbi-data.xml> <output.ttl>

where `<sbi-data.xml>` can be a local file or a URL of the file. You can limit the number of converted entities by specifying the `--count=` parameter.
