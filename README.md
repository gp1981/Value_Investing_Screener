---
editor_options: 
  markdown: 
    wrap: 72
---

# Value_Investing_Screener

This repository includes a library to use [Financial Modeling
Prep](https://financialmodelingprep.com) to perform fundamental analysis
of companies. You need an API key from Financial Modeling Prep to run
the code.

The code also perform a ranking according to the J. Greenblatt's magic
formula website which is used to compare the ranking obtained.

## Installation

1.  Clone the repository:
    `sh     git clone https://github.com/yourusername/Value_Investing_Screener.git     cd Value_Investing_Screener`

2.  Install the required R packages:
    `r     # Open R or RStudio and run the following command:     source('Functions/Setup.R')`

3.  Store your API key securely using the `keyring` package:
    `r     keyring::key_set("API_FMP_KEY") # set your API Key to access  https://financialmodelingprep.com API`\
    `r     keyring::key_set("MF_username") # set your username to access https://www.magicformulainvesting.com`\
    `r     keyring::key_set("MF_password") # set your passowrd to access https://www.magicformulainvesting.com`

4.  Create the following folders: `"/Output/Data"` where the output of
    the analysis is saved. \## Usage

5.  Set the necessary inputs in `Main_Screener.R`:
    `r     last_business_date <- as.Date("2025-01-03") # Update the last business date     # ...existing code...`

6.  Run the main script: `r     source('Main_Screener.R')`

7.  The script will generate various outputs including financial data,
    rankings, and visualizations.

## Collaboration

1.  Fork the repository on GitHub.
2.  Create a new branch for your feature or bugfix:
    `sh     git checkout -b feature-name`
3.  Commit your changes:
    `sh     git commit -m "Description of your changes"`
4.  Push to the branch: `sh     git push origin feature-name`
5.  Create a pull request on GitHub.

## Disclaimer

This code is provided as-is without any warranty. The author takes no
responsibility for any issues or damages arising from its use.

## Investment Disclaimer

The information provided by this code is for educational and
informational purposes only and should not be construed as financial
advice. The author is not a financial advisor and does not provide
personalized investment advice. Users should conduct their own research
and consult with a qualified financial advisor before making any
investment decisions. The author is not responsible for any financial
losses or damages that may occur as a result of using this code.
