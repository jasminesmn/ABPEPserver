# ABPEPserver

ABPEPserver is an online database and analytical platform that offers the visualization of a large-scale tumour proteomics analysis of Substitutant expression across multiple tumour types. Functionality-wise, it offers analysis of gene-association signatures of Substitutant peptides, comparison of enrichment between tumour and tumor-adjacent normal tissues, and list of peptides that serve as candidates for immunotherapy design. ABPEPserver, hence will greatly enhance the exploration of aberrant protein production in human cancer.

Background information

Cancer cells experience tryptophan shortage as a consequence of interferon-gamma (IFN) signaling pathway. It has been recently demonstrated that such tryptophan shortage leads to tryptophan to phenylalanine codon reassignment (W>F) resulting into aberrant peptide products. 
ABPEPserver offers the visualization of a large-scale proteomics analysis of multiple human cancer types in which aberrant peptides are detected. In this analysis, tryptophan to phenylalanine (W>F) codon reassignment were found to be a vastly abundant phenomenon in multiple cancer types. Furthermore, these W>F mis-incorporations, called W>F substitutants, were found to be enriched in tumors as compared with adjacent normal tissues, and often their appearance was associated with T-cell and oncogenic signaling activities.
Proteomic cancer data from multiple cancer types, hosted by PDC commons database, have been used in this analysis. An in depth description of how the analysis was conducted is shown in the 'Methods' tab. The 'Cancer types' tab shows cancer types analyzed and includes information about each proteomic dataset.
This server will enable ease and fast access to aberrant peptides, which are potent immunotherapeutic targets and hence of high value.
Results are available in:

Barplots depicting cumulative number of W-substitutants detected in tumour and adjacent normal tissue samples.
Violin plots depicting the number of substitutions detected in low (intensity < 0) and high (intensity >0) tumor and adjacent normal tissue samples.
Scatter contour plots depicting for every gene the number of W>F substitutions when the gene is higher expressed (intensity > 0) on X-axis (High Class) and when the gene is lower expressed (intensity < 0) on Y-axis (Low Class).
Excel file outputs with the list of aberrant peptides.

<div id="top"></div>

![image](https://user-images.githubusercontent.com/80966662/158176492-53809b1b-1200-4e45-81bd-e01959be8269.png)![image](https://user-images.githubusercontent.com/80966662/158176499-eac7b370-27cc-459e-a33a-f3d714f88b9a.png)


<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/jasminesmn/ABPEPserver">
    <img src="images/logo.png" alt="Logo">
  </a>

<h3 align="center">ABPEPserver: a web application for documentation and analysis of Substitutants </h3>

  <p>
    ABPEPserver is an online database and analytical platform that offers the visualization of a large-scale tumour proteomics analysis of Substitutant expression across multiple tumour types. Functionality-wise, it offers analysis of gene-association signatures of Substitutant peptides, comparison of enrichment between tumour and tumor-adjacent normal tissues, and list of peptides that serve as candidates for immunotherapy design. ABPEPserver, hence will greatly enhance the exploration of aberrant protein production in human cancer.
    <br />

  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li><a href="#about-the-project">About The Project</a></li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

 <a href="https://rhpc.nki.nl/sites/shiny/ABPEP/">
    <img src="images/welcome.png" alt="Welcome ">
  </a>
[![Product Name Screen Shot][product-screenshot]](https://rhpc.nki.nl/sites/shiny/ABPEP/)

Here's a blank template to get started: To avoid retyping too much info. Do a search and replace with your text editor for the following: `github_username`, `repo_name`, `twitter_handle`, `linkedin_username`, `email_client`, `email`, `project_title`, `project_description`

<p align="right">(<a href="#top">back to top</a>)</p>



<p align="right">(<a href="#top">back to top</a>)</p>



<!-- GETTING STARTED -->
## Getting Started

This is an example of how you may give instructions on setting up your project locally.
To get a local copy up and running follow these simple example steps.

### Prerequisites

This is an example of how to list things you need to use the software and how to install them.
* npm
  ```sh
  npm install npm@latest -g
  ```

### Installation

1. Get a free API Key at [https://example.com](https://example.com)
2. Clone the repo
   ```sh
   git clone https://github.com/github_username/repo_name.git
   ```
3. Install NPM packages
   ```sh
   npm install
   ```
4. Enter your API in `config.js`
   ```js
   const API_KEY = 'ENTER YOUR API';
   ```

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- USAGE EXAMPLES -->
## Usage

Use this space to show useful examples of how a project can be used. Additional screenshots, code examples and demos work well in this space. You may also link to more resources.

_For more examples, please refer to the [Documentation](https://example.com)_

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ROADMAP -->
## Roadmap

- [ ] Feature 1
- [ ] Feature 2
- [ ] Feature 3
    - [ ] Nested Feature

See the [open issues](https://github.com/github_username/repo_name/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Your Name - [@twitter_handle](https://twitter.com/twitter_handle) - email@email_client.com

Project Link: [https://github.com/github_username/repo_name](https://github.com/github_username/repo_name)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

* []()
* []()
* []()

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/github_username/repo_name.svg?style=for-the-badge
[contributors-url]: https://github.com/github_username/repo_name/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/github_username/repo_name.svg?style=for-the-badge
[forks-url]: https://github.com/github_username/repo_name/network/members
[stars-shield]: https://img.shields.io/github/stars/github_username/repo_name.svg?style=for-the-badge
[stars-url]: https://github.com/github_username/repo_name/stargazers
[issues-shield]: https://img.shields.io/github/issues/github_username/repo_name.svg?style=for-the-badge
[issues-url]: https://github.com/github_username/repo_name/issues
[license-shield]: https://img.shields.io/github/license/github_username/repo_name.svg?style=for-the-badge
[license-url]: https://github.com/github_username/repo_name/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/linkedin_username
[product-screenshot]: images/screenshot.png
