# Conversion of the SBI data to the InTaVia RDF graph

This project uses the following requirements:

- `Python 3.10`
- `pipenv`

To start using this project, first create a python virtual environment and install the dependencies:

    pipenv install --dev

To start the converter, run the following command:

    pipenv run ./convert.py <sbi-data.xml> <output.ttl>

where `<sbi-data.xml>` can be a local file or a URL of the file.
