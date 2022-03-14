
<div id="top"></div>


<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/jasminesmn/ABPEPserver">
    <img src="images/logo.png" alt="Logo">
  </a>

<h3 align="center">ABPEPserver: a web application for documentation and analysis of Substitutants </h3>

  <p>
    ABPEPserver is an online database and analytical platform that offers the visualization of a large-scale tumour proteomics analysis of Substitutant expression across multiple tumour types. Functionality-wise, it offers analysis of gene-association signatures of Substitutant peptides, comparison of enrichment between tumour and tumor-adjacent normal tissues, and list of peptides that serve as candidates for immunotherapy design. 
    <br />

  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li><a href="#summary">Summary</a></li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#contact">Contact</a></li>

  </ol>
</details>



<!-- SUMMARY -->
## Summary

 <a href="https://rhpc.nki.nl/sites/shiny/ABPEP/">
    <img src="images/welcome.png" alt="Welcome ">
  </a>

<p align="left">Cancer cells experience tryptophan shortage as a consequence of interferon-gamma (IFN) signaling pathway. It has been recently demonstrated that such tryptophan shortage leads to tryptophan to phenylalanine codon reassignment (W>F) resulting into aberrant peptide products. 
ABPEPserver offers the visualization of a large-scale proteomics analysis of multiple human cancer types in which aberrant peptides are detected. In this analysis, tryptophan to phenylalanine (W>F) codon reassignment were found to be a vastly abundant phenomenon in multiple cancer types. Furthermore, these W>F mis-incorporations, called W>F substitutants, were found to be enriched in tumors as compared with tumor-adjacent normal tissues, and often their appearance was associated with T-cell and oncogenic signaling activities.
Proteomic cancer data from multiple cancer types, hosted by PDC commons database, have been used in this analysis. An in depth description of how the analysis was conducted is shown in the 'Methods' tab. The 'Cancer types' tab shows cancer types analyzed and includes information about each proteomic dataset.<br>
This server will enable ease and fast access to aberrant peptides, which are potent immunotherapeutic targets and hence of high value.
Results are available in:</p>
<ul><li>Barplots depicting cumulative number of W-substitutants detected in tumour and adjacent normal tissue samples.</li>
<li>Violin plots depicting the number of substitutions detected in low (intensity < 0) and high (intensity >0) tumor and adjacent normal tissue samples.</li>
<li>Scatter contour plots depicting for every gene the number of W>F substitutions when the gene is higher expressed (intensity > 0) on X-axis (High Class) and when the gene is lower expressed (intensity < 0) on Y-axis (Low Class).</li>
<li>Excel file outputs with the list of aberrant peptides.</li></ul>



<!-- USAGE EXAMPLES -->
## Usage

This application is available freely on: https://rhpc.nki.nl/sites/shiny/ABPEP/.

### Home page 
The user can find relevent background, method and dataset information in the 'Home' page. A broad explanation of the methods used is given for a possibly better understanding of the proteomic analysis. Dataset information of cancer types used in this analysis may be useful to easily find the number of tumour and tumor-adjacent normal tissues samples in their respective mass spectrometry experiment. 

### Analyze page
Next, the 'Analyze' page allows the user to visualize analysis results with multiple plots for all cancer types used in this analysis. Available plots are discussed below.

#### Barplots
Barplots depicting cumulative number of W-Substitutants detected in tumour and adjacent normal tissue samples. User can choose whether to only plot cumulative number of W-Substitutants detected in all samples, tumour samples, or adjacent normal tissue samples.

#### Violin plots
Violin plots depicting the number of Subtitutants detected in lowly expressed (intensity < 0) and highly expressed (intensity > 0) genes in tumor and adjacent normal tissue samples, of which the gene can be chosen. Also, any W-Substitutant can be chosen to plot and compare detected events.

#### Scatter contour plots
Scatter contour plot depicting for every gene the number of W>F peptides when the gene is higher expressed (intensity > 0) on X-axis (High Class) and when the gene is lower expressed (intensity < 0) on Y-axis (Low Class). Contours depict the density of the distribution. W>F peptides in tumours and normal adjacent normal tissues are depicted in red and green, respectively.

### W>F Substitutants page
Lastly, the page 'W>F Substitutants' allows the user to browse through and download actual detected peptides as well as their number of observations along with further details in table format. Furthermore, the user can choose to plot the number of occurences per peptide in tumour and tumor-adjacent normal tissues. 
Peptide lists are available for download in .txt file format. 


<!-- CONTACT -->
## Contact

Email: j.montenegronavarro@nki.nl

Project Link: https://github.com/jasminesmn/ABPEPserver
